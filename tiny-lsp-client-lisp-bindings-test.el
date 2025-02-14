
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
  (unless (equal exp act)
    (std-message "Exp %s" exp)
    (std-message "Act %s" act)
    (cl-assert (equal exp act) 'show)))

(defun std-message (format-string &rest args)
  (print (format (concat "[emacs]  " format-string) args) 'external-debugging-output))

;; -----------------------------------------------------------------------------
;; Test
;;------------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Load the module
;;;;----------------------------------------------------------------------------

(std-message "Before load")

(tlc-reload)

(std-message "After load")

(tlc--rust-set-log-option 'tlc-log-file (file-truename
                                         (file-name-concat
                                          user-emacs-directory
                                          "tiny-lsp-client-test.log")))

(assert-equal nil (tlc--rust-all-server-info))

;;;; ---------------------------------------------------------------------------
;;;; Initialize
;;;;----------------------------------------------------------------------------

(setq root-path (file-truename default-directory))

(std-message "Starting server")

(assert-equal 'started (tlc--rust-start-server root-path "rust-analyzer"))

(pcase (tlc--rust-all-server-info)
  (`((,r ,c ,i))
   (assert-equal root-path r)
   (assert-equal "rust-analyzer" c))
  (x
   (error "unexpected return: %s" x)))

(assert-equal 'already-started (tlc--rust-start-server root-path "rust-analyzer"))

(std-message "Server started")

;;;; ---------------------------------------------------------------------------
;;;; didOpen
;;;;----------------------------------------------------------------------------

(sleep-for 1)

(std-message "Sending didOpen")

(tlc--rust-send-notification
 root-path
 "textDocument/didOpen"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"))

;;;; ---------------------------------------------------------------------------
;;;; definition
;;;;----------------------------------------------------------------------------

(sleep-for 1)

(std-message "Sending definition")

(assert-equal 1 (tlc--rust-send-request
                 root-path
                 "textDocument/definition"
                 '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(defun recv-response ()
  (let ((response (tlc--rust-recv-response root-path)))
    (if (equal 'no-response response)
        (progn
          (std-message "recv-response retry")
          (sleep-for 1)
          (recv-response))
      response)))

(assert-equal '(ok 1 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
              (recv-response))

;;;; ---------------------------------------------------------------------------
;;;; didChange
;;;;----------------------------------------------------------------------------

(std-message "Sending didChange")

(tlc--rust-send-notification
 root-path
 "textDocument/didChange"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 6 0 "\n"))))

;;;; ---------------------------------------------------------------------------
;;;; definition after didChange
;;;;----------------------------------------------------------------------------

(std-message "Sending definition after didChange")

(assert-equal 2 (tlc--rust-send-request
                 root-path
                 "textDocument/definition"
                 '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal '(ok 2 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 8 3)))
              (recv-response))

;;;; ---------------------------------------------------------------------------
;; textDocument/didChange to revert the previous change, so that rust-analyzer's
;; view matches the file system
;;;;----------------------------------------------------------------------------

(std-message "Sending didChange to revert")

(tlc--rust-send-notification
 root-path
 "textDocument/didChange"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 7 1 ""))))

;;;; ---------------------------------------------------------------------------
;;;; didClose
;;;;----------------------------------------------------------------------------

(std-message "Sending didClose")

(tlc--rust-send-notification
 root-path
 "textDocument/didClose"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"))

;;;; ---------------------------------------------------------------------------
;;;; didOpen + definition again
;;;;----------------------------------------------------------------------------

;; Do a definition again to ensure that rust-analyzer did not crash due to
;; faulty data sent in didClose above. Since it's a notification we can't
;; wait for a response.

(std-message "Sending didOpen+definition")

(tlc--rust-send-notification
 root-path
 "textDocument/didOpen"
 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"))

(assert-equal 3 (tlc--rust-send-request
                 root-path
                 "textDocument/definition"
                 '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal '(ok 3 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
              (recv-response))

;;;; ---------------------------------------------------------------------------
;;;; End
;;;;----------------------------------------------------------------------------

(std-message "done")
