
;; -----------------------------------------------------------------------------
;; Load common test functions
;; -----------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(load "test/common.el")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Before, there used to be a bug that if garbage-collect was done with
;; --release emacs would crash. But now it seems to work, even though no
;; non-test code change was needed as of this commit to make it work.
(defun test-garbage-collect (mark)
  (std-message "garbage-collect before (%s)" mark)
  (std-message "%s" (garbage-collect))
  (std-message "garbage-collect after (%s)" mark))

(defun kill-server ()
  (interactive)
  (pcase-let ((`((,r ,c ,i)) (tlc--rust-all-server-info)))
    (shell-command (format "kill %s" i))))

;; -----------------------------------------------------------------------------
;; Load the module
;; -----------------------------------------------------------------------------

(std-message "Before load")

(require 'tlc-rust "target/release/libtiny_lsp_client.so")

(std-message "After load")

(test-garbage-collect "after load")

(tlc--rust-set-log-option 'tlc-log-file (file-truename
                                         (file-name-concat
                                          user-emacs-directory
                                          "tiny-lsp-client-test.log")))

(assert-equal nil (tlc--rust-all-server-info))

(test-garbage-collect "after some calls")

;; -----------------------------------------------------------------------------
;; Initialize
;; -----------------------------------------------------------------------------

(setq root-path (file-truename default-directory))

(std-message "Starting server")

(assert-equal 'start-failed (tlc--rust-start-server root-path "doesnt_exist"))

(assert-equal 'started (tlc--rust-start-server root-path "rust-analyzer"))

(pcase (tlc--rust-all-server-info)
  (`((,r ,command ,process-id ,alive))
   (assert-equal root-path r)
   (assert-equal "rust-analyzer" command)
   (assert-equal t (integerp process-id))
   (assert-equal t alive)
   )
  (x
   (error "unexpected return: %s" x)))

(assert-equal 'already-started (tlc--rust-start-server root-path "rust-analyzer"))

(std-message "Server started")

(test-garbage-collect "after server started")

;; -----------------------------------------------------------------------------
;; didOpen
;; -----------------------------------------------------------------------------

;; todo: need to loop even if less clear, becase now is unstable
(sleep-for 2)

(std-message "Sending didOpen")

(assert-equal 'ok (tlc--rust-send-notification
                   root-path
                   "textDocument/didOpen"
                   (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")))

;; -----------------------------------------------------------------------------
;; definition
;; -----------------------------------------------------------------------------

(sleep-for 10)

(std-message "Sending definition")

(assert-equal 'no-server
              (tlc--rust-send-request
               "/some/root_path/that/does/not/exist"
               "textDocument/definition"
               '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal 1
              (tlc--rust-send-request
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

(assert-equal 'no-server (tlc--rust-recv-response "/some/root/path/not/found"))

(assert-equal '(ok-response 1 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
              (recv-response))

(test-garbage-collect "after textDocument/definition")

;; -----------------------------------------------------------------------------
;; didChange
;; -----------------------------------------------------------------------------

(std-message "Sending didChange")

(assert-equal 'no-server
              (tlc--rust-send-notification
               "/some/root/path/not/found"
               "textDocument/didChange"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 6 0 "\n")))))

(assert-equal 'ok
              (tlc--rust-send-notification
               root-path
               "textDocument/didChange"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 6 0 "\n")))))

;; -----------------------------------------------------------------------------
;; definition after didChange
;; -----------------------------------------------------------------------------

(std-message "Sending definition after didChange")

(assert-equal 2
              (tlc--rust-send-request
               root-path
               "textDocument/definition"
               '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal '(ok-response 2 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 8 3)))
              (recv-response))

;; -----------------------------------------------------------------------------
;; textDocument/didChange to revert the previous change, so that rust-analyzer's
;; view matches the file system
;; -----------------------------------------------------------------------------

(std-message "Sending didChange to revert")

(assert-equal 'ok
              (tlc--rust-send-notification
               root-path
               "textDocument/didChange"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 7 1 "")))))

;; -----------------------------------------------------------------------------
;; didClose
;; -----------------------------------------------------------------------------

(std-message "Sending didClose")

(assert-equal 'ok
              (tlc--rust-send-notification
               root-path
               "textDocument/didClose"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")))

;; -----------------------------------------------------------------------------
;; didOpen + definition again
;; -----------------------------------------------------------------------------

;; Do a definition again to ensure that rust-analyzer did not crash due to
;; faulty data sent in didClose above. Since it's a notification we can't
;; wait for a response.

(std-message "Sending didOpen+definition")

(assert-equal 'ok
              (tlc--rust-send-notification
               root-path
               "textDocument/didOpen"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")))

(assert-equal 3
              (tlc--rust-send-request
               root-path
               "textDocument/definition"
               '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal '(ok-response 3 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
              (recv-response))

;; -----------------------------------------------------------------------------
;; Stopping the LSP server
;; -----------------------------------------------------------------------------

(std-message "killing server")

(kill-server)

;; to avoid race condition
(sleep-for 1)

(assert-equal 'no-server
              (tlc--rust-send-notification
               root-path
               "textDocument/didOpen"
               (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")))

(assert-equal 'no-server
              (tlc--rust-send-request
               root-path
               "textDocument/definition"
               '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

(assert-equal 'no-server (recv-response))

;; -----------------------------------------------------------------------------
;; End
;; -----------------------------------------------------------------------------

(test-garbage-collect "in the end")

(std-message "done")
