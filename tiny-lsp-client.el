
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)

;; -----------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

(defcustom tlc-server-cmds
  '((rust-mode . "rust-analyzer"))
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Start
;;------------------------------------------------------------------------------

(defun tlc--start ()
  (let* ((root (if-let ((r (tlc--find-root)))
                   r
                 (user-error "Can't find root")))
         (server-cmd (if-let ((r (alist-get major-mode tlc-server-cmds)))
                         r
                       (user-error
                        "No server command found for major mode: %s"
                        major-mode)))
         (file (if-let ((r (buffer-file-name)))
                   r
                 (user-error "tiny-lsp-client only works for file-based buffers"))))
    (tlc--rust-start-server root server-cmd)
    ;; todo: if buffer-revert-quick, this is sent again, but not didClose
    (tlc--rust-send-notification
     root
     "textDocument/didOpen"
     (list file))
    (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)))

(defun tlc--find-root ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (if-let ((root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
        (unless (string-match-p "fatal:" root)
          root))))

;; -----------------------------------------------------------------------------
;; Stop
;;------------------------------------------------------------------------------

(defun tlc--stop ()
  (let* ((root (if-let ((r (tlc--find-root)))
                   r
                 (user-error "Can't find root")))
         (file (if-let ((r (buffer-file-name)))
                   r
                 (user-error "tiny-lsp-client only works for file-based buffers"))))
    ;; todo: if last buffer, stop the server
    (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
    (tlc--rust-send-notification
     root
     "textDocument/didClose"
     (list file))))

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
  (let* ((file (buffer-file-name))
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
;; Minor mode
;;------------------------------------------------------------------------------

;;;###autoload
(define-minor-mode tlc-mode
  "tiny-lsp-client: a minor mode for LSP."
  :lighter " tlc-mode"
  :group 'tiny-lsp-client
  (cond
   (tlc-mode (tlc--start))
   (t (tlc--stop))))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
