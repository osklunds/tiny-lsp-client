
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)

;; -----------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

;; todo: specify types

(defcustom tlc-server-cmds
  '((rust-mode . "rust-analyzer"))
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client)

(defcustom tlc-find-root-function
  'tlc-find-root-default-function
  "Function used for finding the root path of a project.

Default to `tlc-find-root-default-function' which first tries Projectile,
and if that fails, tries using \"git rev-parse --show-toplevel\"." 
  :group 'tiny-lsp-client)

(defcustom tlc-log-io nil
  "Whether JSON messages between tiny-lsp-client and the LSP server should be logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-stderr t
  "Whether stderr output from the LSP server should be logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-debug nil
  "Whether debug logging (in Rust code) should be enabled. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-to-stdio nil
  "In addition to logging to files, if logging should also happen to standard output. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-file (file-truename
                         (file-name-concat
                          user-emacs-directory
                          "tiny-lsp-client.log"))
  "Directory in which log files are placed."
  :group 'tiny-lsp-client
  :type 'string
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

;; -----------------------------------------------------------------------------
;; Minor mode
;;------------------------------------------------------------------------------

;;;###autoload
(define-minor-mode tlc-mode
  "tiny-lsp-client: a minor mode for LSP."
  :lighter " tlc-mode"
  :group 'tiny-lsp-client
  (cond
   ((not buffer-file-name)
    (message "tiny-lsp-client can only be used in file buffers.")
    (setq tlc-mode nil))
   ((not (tlc--root))
    (message "tiny-lsp-client can only be used in buffers where root can be found.")
    (setq tlc-mode nil))
   (t
    (cond
     (tlc-mode
      (tlc--start-server)
      (tlc--notify-text-document-did-open)
      (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)
      (add-hook 'kill-buffer-hook 'tlc--kill-buffer-hook nil t)
      (add-hook 'before-revert-hook 'tlc--before-revert-hook nil t)
      (add-hook 'after-revert-hook 'tlc--after-revert-hook nil t)
      (add-hook 'before-change-functions 'tlc--before-change-hook nil t)
      (add-hook 'after-change-functions 'tlc--after-change-hook nil t)
      (add-hook 'change-major-mode-hook 'tlc--change-major-mode-hook nil t)
      )
     (t
      ;; todo: if last buffer, stop the server
      (tlc--notify-text-document-did-close)
      (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
      (remove-hook 'kill-buffer-hook 'tlc--kill-buffer-hook t)
      (remove-hook 'before-revert-hook 'tlc--before-revert-hook t)
      (remove-hook 'after-revert-hook 'tlc--after-revert-hook t)
      (remove-hook 'before-change-functions 'tlc--before-change-hook t)
      (remove-hook 'after-change-functions 'tlc--after-change-hook t)
      (remove-hook 'change-major-mode-hook 'tlc--change-major-mode-hook t)
      )))))

(defun tlc--start-server ()
  (let* ((server-cmd (if-let ((r (alist-get major-mode tlc-server-cmds)))
                         r
                       (user-error
                        "No server command found for major mode: %s"
                        major-mode))))
    (tlc--rust-start-server (tlc--root) server-cmd)))

(defun tlc--kill-buffer-hook ()
  (when tlc-mode
    (tlc--notify-text-document-did-close)))

(defun tlc--before-revert-hook ()
  (tlc--notify-text-document-did-close))

(defun tlc--after-revert-hook ()
  ;; If revert-buffer-preserve-modes is nil (default), it means that tlc-mode is
  ;; run and didOpen is called from there, and it would result in duplicate
  ;; didOpen calls. See
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Reverting.html
  (when revert-buffer-preserve-modes
    (tlc--notify-text-document-did-open)))

(defun tlc--notify-text-document-did-open ()
  (when (buffer-modified-p)
    ;; todo: document this
    (message "tiny-lsp-client can only open saved buffers, so saving for you.")
    (save-buffer))
  (tlc--rust-send-notification
   (tlc--root)
   "textDocument/didOpen"
   (list (tlc--buffer-file-name))))

(defun tlc--notify-text-document-did-close ()
  (tlc--rust-send-notification
   (tlc--root)
   "textDocument/didClose"
   (list (tlc--buffer-file-name))))

(defun tlc--change-major-mode-hook ()
  (tlc-mode -1))

;; -----------------------------------------------------------------------------
;; didChange
;;------------------------------------------------------------------------------

(defvar-local tlc--change nil)

(defun tlc--before-change-hook (beg end)
  (if tlc--change
      ;; I know this is overly simplified, but when this case happens, I fix it
      (error "tlc--change is not-nil in before-change")
    ;; if revert in progress, it can happen that didChange is sent before didOpen,
    ;; when discarding changes in magit
    (unless revert-buffer-in-progress-p
      (setq tlc--change (append (tlc--pos-to-lsp-pos beg) (tlc--pos-to-lsp-pos end))))))

;; Heavily inspired by eglot
;; nil pos means current point
(defun tlc--pos-to-lsp-pos (&optional pos)
  (let* ((line (- (line-number-at-pos pos) 1))
         (character (save-excursion
                      (when pos
                        (goto-char pos))
                      (current-column))))
    (list line character)))

(defun tlc--after-change-hook (beg end _pre-change-length)
  (let* ((start-line      (nth 0 tlc--change))
         (start-character (nth 1 tlc--change))
         (end-line        (nth 2 tlc--change))
         (end-character   (nth 3 tlc--change))
         (text (buffer-substring-no-properties beg end))
         )
    (setq tlc--change nil)
    ;; if revert in progress, it can happen that didChange is sent before didOpen
    ;; when discarding changes in magit
    (unless revert-buffer-in-progress-p
      (tlc--notify-text-document-did-change start-line
                                            start-character
                                            end-line
                                            end-character
                                            text))))

(defun tlc--notify-text-document-did-change (start-line
                                             start-character
                                             end-line
                                             end-character
                                             text)
  (tlc--rust-send-notification
   (tlc--root)
   "textDocument/didChange"
   (list (tlc--buffer-file-name)
         `((,start-line ,start-character ,end-line ,end-character ,text))
         )))

;; -----------------------------------------------------------------------------
;; Request/response
;;------------------------------------------------------------------------------

(defun tlc--sync-request (method arguments)
  (let ((request-id (tlc--rust-send-request (tlc--root) method arguments)))
    (tlc--wait-for-response request-id)))

(defun tlc--wait-for-response (request-id)
  ;; todo: consider exponential back-off
  (sleep-for 0.01)
  (let ((response (tlc--rust-recv-response (tlc--root))))
    (pcase response
      ;; normal case - response has arrived
      (`(ok ,id ,params)
       (cond
        ;; alternative but valid case - response to old request
        ((< id request-id) (tlc--wait-for-response request-id))

        ;; bug case - response to request id not yet sent
        ((> id request-id) (error "too big id"))

        ;; normal case - response to current request
        (t                 params)))

      ;; normal case - no response yet
      ('no-response (tlc--wait-for-response request-id))

      ;; alternative but valid case - some error response
      ;; For now, just print a message, because so far I've only encountered it
      ;; for temporary issues. In the future, consider passing code and msg.
      ('error-response (user-error
                        (concat "Got error response from LSP server."
                                "It might be a temporary issue."
                                "But if it keeps happening, you can check the IO logs")))

      ;; bug case - some other response
      (_ (error "bad response"))
      )))

;; -----------------------------------------------------------------------------
;; Xref
;;------------------------------------------------------------------------------

(defun tlc-xref-backend () 'xref-tlc)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tlc)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-tlc)) identifier)
  (let* ((file (tlc--buffer-file-name))
         (pos (tlc--pos-to-lsp-pos))
         (line (nth 0 pos))
         (character (nth 1 pos))
         (response (tlc--sync-request "textDocument/definition" (list file line character))))
    (mapcar (lambda (location)
              (pcase-let ((`(,file-target ,line-start ,character-start) location))
                (let ((line-target (+ line-start 1)))
                  (xref-make
                   "todo"
                   (xref-make-file-location file-target line-target character-start)))))
            response)))

;; -----------------------------------------------------------------------------
;; Logging
;;------------------------------------------------------------------------------

(defun tlc-open-log-file ()
  (interactive)
  ;; todo: is this the correct way to get custom?
  (find-file (tlc--rust-get-log-option 'tlc-log-file)))

;; -----------------------------------------------------------------------------
;; Misc helpers
;;------------------------------------------------------------------------------

(defun std-message (format-string &rest args)
  (print (format (concat "[emacs]  " format-string) args) 'external-debugging-output))

(defun tlc--buffer-file-name ()
  ;; In after-revert-hook, if there was a change, buffer-file-name is nil,
  ;; so use this instead
  (let ((name (file-truename buffer-file-truename)))
    (cl-assert name)
    name))

(defvar-local tlc--cached-root nil)

(defun tlc--root ()
  (if tlc--cached-root
      tlc--cached-root
    (funcall tlc-find-root-function)))

(defun tlc-find-root-default-function ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (if-let ((root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
        (unless (string-match-p "fatal:" root)
          root))))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
