
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
      ;; todo: can solve issue
      ;; (add-hook 'change-major-mode-hook #'eglot--managed-mode-off nil t)
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
  (tlc--rust-send-notification
   (tlc--root)
   "textDocument/didOpen"
   (list (tlc--buffer-file-name))))

(defun tlc--notify-text-document-did-close ()
  (tlc--rust-send-notification
   (tlc--root)
   "textDocument/didClose"
   (list (tlc--buffer-file-name))))

(defvar-local tlc--change nil)

(defun tlc--before-change-hook (beg end)
  (setq tlc--change (list (tlc--pos-to-lsp-pos beg) (tlc--pos-to-lsp-pos end)))
  (message "before: %s" tlc--change)
  )

;; Heavily inspired by eglot
(defun tlc--pos-to-lsp-pos (pos)
  (let* ((line (- (line-number-at-pos pos) 1))
         (character (save-excursion
                      (goto-char pos)
                      (current-column))))
    (list line character)))

(defun tlc--after-change-hook (beg end pre-change-length)
  (let* ((start (car tlc--change))
         (start-line (car start))
         (start-character (cadr start))
         (end1 (cadr tlc--change))
         (end-line (car end1))
         (end-character (cadr end1))
         (text (buffer-substring-no-properties beg end))
         )
    (tlc--notify-text-document-did-change start-line
                                          start-character
                                          end-line
                                          end-character
                                          text)))

(defun tlc--notify-text-document-did-change (start-line
                                             start-character
                                             end-line
                                             end-character
                                             text)
  (message "oskar: %s" (list start-line start-character end-line end-character text))
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
      ('error (user-error
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
         (line (- (line-number-at-pos) 1))
         (character (current-column))
         (response (tlc--sync-request "textDocument/definition" (list file line character))))
    (mapcar (lambda (location)
              (pcase-let ((`(,file-target ,line-start ,character-start) location))
                (let ((line-target (+ line-start 1)))
                  (xref-make
                   "todo"
                   (xref-make-file-location file-target line-target character-start)))))
            response)))

;; -----------------------------------------------------------------------------
;; Misc helpers
;;------------------------------------------------------------------------------

(defun tlc--buffer-file-name ()
  (cl-assert buffer-file-name)
  buffer-file-name)

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
