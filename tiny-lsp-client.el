
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
  'tlc--find-root-default-function
  "Function used for finding the root path of a project.

Default to `tlc--find-root-default-function` which first tries Projectile,
and if that fails, tries using git rev-parse --show-toplevel." 
  :group 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Request/response
;;------------------------------------------------------------------------------

(defun tlc--sync-request (method arguments)
  (let ((request-id (tlc--rust-send-request (tlc--find-root) method arguments)))
    (tlc--wait-for-response request-id)))

(defun tlc--wait-for-response (request-id)
  ;; todo: consider exponential back-off
  (sleep-for 0.01)
  (let ((response (tlc--rust-recv-response (tlc--find-root))))
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

(defun tlc--find-root ()
  (if tlc--cached-root
      tlc--cached-root
    (funcall tlc-find-root-function)))

(defun tlc--find-root-default-function ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (if-let ((root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
        (unless (string-match-p "fatal:" root)
          root))))

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
   ((not (tlc--find-root))
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
      (add-hook 'after-revert-hook 'tlc--after-revert-hook nil t))
     (t
      ;; todo: if last buffer, stop the server
      (tlc--notify-text-document-did-close)
      (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
      (remove-hook 'kill-buffer-hook 'tlc--kill-buffer-hook t)
      (remove-hook 'before-revert-hook 'tlc--before-revert-hook t)
      (remove-hook 'after-revert-hook 'tlc--after-revert-hook t))))))

(defun tlc--start-server ()
  (let* ((server-cmd (if-let ((r (alist-get major-mode tlc-server-cmds)))
                         r
                       (user-error
                        "No server command found for major mode: %s"
                        major-mode))))
    (tlc--rust-start-server (tlc--find-root) server-cmd)))

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
   (tlc--find-root)
   "textDocument/didOpen"
   (list (tlc--buffer-file-name))))

(defun tlc--notify-text-document-did-close ()
  (tlc--rust-send-notification
   (tlc--find-root)
   "textDocument/didClose"
   (list (tlc--buffer-file-name))))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
