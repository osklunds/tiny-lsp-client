
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)

(defcustom tlc-server-cmds
  '((rust-mode . "rust-analyzer"))
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client)

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

(defun tlc-xref-backend () 'xref-tlc)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tlc)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-tlc)) identifier)
  (list (xref-make "hejhej" (xref-make-file-location "/home/oskar/own_repos/tiny-lsp-client/tiny-lsp-client.el" 1 2))))

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
