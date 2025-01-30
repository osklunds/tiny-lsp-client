
;; (defun my-message-from-rust ()
;;   "Returns a message from Rust"
;;   "dummy")

;; copied from https://emacs.stackexchange.com/questions/33976/how-do-you-reload-a-dynamic-module
(defun fake-module-reload (module)
  (interactive "fReload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(fake-module-reload "/home/oskar/own_repos/tiny-lsp-client/target/debug/libtiny_lsp_client.so")
;; (fake-module-reload
;;  "/home/oskar/own_repos/tiny-lsp-client/target/release/libtiny_lsp_client.so")

(defun tlc-reload ()
  (interactive)
  (fake-module-reload "/home/oskar/own_repos/tiny-lsp-client/target/debug/libtiny_lsp_client.so"))
