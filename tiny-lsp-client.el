
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

(defun tlc--initialize ()
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
      (`(,id ,params)
       (cond
        ;; alternative but valid case - response to old request
        ((< id request-id) (tlc--wait-for-response request-id))

        ;; error case - response to request id not yet sent
        ((> id request-id) (error "too big id"))

        ;; normal case - response to current request
        (t                 params)))

      ;; normal case - no response yet
      ( 'no-response (tlc--wait-for-response request-id))

      ;; error case - some other response
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
    (pcase response
      (`(,file-target ,line-start ,character-start ,line-end ,character-end)
       (let ((line-target (+ line-start 1)))
         (list (xref-make
                "todo"
                (xref-make-file-location file-target line-target character-start))))
       )
      (_ nil)
      )
    )) 

;; -----------------------------------------------------------------------------
;; Minor mode
;;------------------------------------------------------------------------------

;;;###autoload
(define-minor-mode tlc-mode
  "tiny-lsp-client: a minor mode for LSP."
  :lighter " tlc-mode"
  :group 'tiny-lsp-client
  (cond
   (tlc-mode (tlc--initialize))
   (t nil)))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
