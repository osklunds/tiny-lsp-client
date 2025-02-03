
(require 'cl-lib)

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

(defun tlc-reload ()
  (interactive)
  (fake-module-reload "/home/oskar/own_repos/tiny-lsp-client/target/debug/libtiny_lsp_client.so"))
(defun stderr (msg)
  (print (format "emacs-stderr: %s" msg) 'external-debugging-output))

(tlc-reload)

(stderr (tlc--rust-all-server-info))

(stderr (tlc--rust-start-server default-directory "rust-analyzer"))
(stderr (tlc--rust-start-server default-directory "rust-analyzer"))

(sleep-for 1)

(tlc--rust-send-notification
 default-directory
 "textDocument/didOpen"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"))

(sleep-for 1)

(tlc--rust-send-request
 default-directory
 "textDocument/definition"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 4))

(defun recv-response ()
  (let ((response (tlc--rust-recv-response default-directory)))
    (if (equal 'no-response response)
        (progn
          (stderr "again")
          (sleep-for 1)
          (recv-response))
      response)))

(defun assert-equal (exp act)
  (cl-assert (equal exp act) 'show))

(assert-equal (list "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3 7 18)
              (recv-response))

(stderr "done")
