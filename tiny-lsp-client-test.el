
(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

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

(defun assert-equal (exp act)
  (cl-assert (equal exp act) 'show))

(defun std-message (format-string &rest args)
  (print (format (concat "[emacs]  " format-string) args) 'external-debugging-output))

;; -----------------------------------------------------------------------------
;; Test
;;------------------------------------------------------------------------------

(tlc-reload)

(std-message "%s" (tlc--rust-all-server-info))

(std-message "Starting server")

(assert-equal 'started (tlc--rust-start-server default-directory "rust-analyzer"))
(assert-equal 'already-started (tlc--rust-start-server default-directory "rust-analyzer"))

(std-message "Server started")

;; todo: support more results, so that this can bi skipped
(sleep-for 1)

(std-message "Sending didOpen")

(tlc--rust-send-notification
 default-directory
 "textDocument/didOpen"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"))

;; todo: support more results, so that this can bi skipped
(sleep-for 1)

(std-message "Sending definition")

(tlc--rust-send-request
 default-directory
 "textDocument/definition"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10))

(defun recv-response ()
  (let ((response (tlc--rust-recv-response default-directory)))
    (if (equal 'no-response response)
        (progn
          (std-message "recv-response again")
          (sleep-for 1)
          (recv-response))
      response)))

(assert-equal (list "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3 7 18)
              (recv-response))

(std-message "done")
