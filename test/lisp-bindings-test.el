
;; Tests focusing on the lisp bindings that lib.rs exposes.

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "new-common.el"))
(setq test-file-name "lisp-bindings-test")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Before, there used to be a bug that if garbage-collect was done with
;; --release emacs would crash. But now it seems to work, even though no
;; non-test code change was needed as of this commit to make it work.
(defun test-garbage-collect (label)
  (message "garbage-collect before (%s)" label)
  (message "%s" (garbage-collect))
  (message "garbage-collect after (%s)" label))

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cargo build --release")

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(ert-deftest lisp-bindings-test ()
  ;; ---------------------------------------------------------------------------
  ;; Loading the module
  ;; ---------------------------------------------------------------------------
  (message "Before load")

  ;; todo: find a way to assert that newly loaded and release
  (require 'tlc-rust (relative-repo-root "target" "release" "libtiny_lsp_client.so"))

  (message "After load")

  (test-garbage-collect "after load")

  (tlc--rust-set-option 'tlc-log-to-stdio nil)

  (tlc--rust-set-option 'tlc-log-file
                        (relative-repo-root "test" "logs" "lisp-bindings-test.log"))

  (assert-equal nil (tlc--rust-all-server-info))

  (test-garbage-collect "after some calls")

  ;; ---------------------------------------------------------------------------
  ;; Initialize
  ;; ---------------------------------------------------------------------------

  (setq root-path (file-truename default-directory))

  (message "Starting server")

  (assert-equal 'start-failed (tlc--rust-start-server root-path "doesnt_exist"))

  (assert-equal 'started (tlc--rust-start-server root-path "rust-analyzer"))

  (pcase (tlc--rust-all-server-info)
    (`((,r ,command ,process-id))
     (assert-equal root-path r)
     (assert-equal "rust-analyzer" command)
     (assert-equal t (integerp process-id))
     )
    (x
     (error "unexpected return: %s" x)))

  (assert-equal 'already-started (tlc--rust-start-server root-path "rust-analyzer"))

  (message "Server started")

  (test-garbage-collect "after server started")

  ;; ---------------------------------------------------------------------------
  ;; didOpen
  ;; ---------------------------------------------------------------------------

  ;; todo: need to loop even if less clear, becase now is unstable
  (sleep-for 2)

  (message "Sending didOpen")

  (setq path "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")
  (setq content (with-temp-buffer
                  (insert-file-contents path)
                  (buffer-string)))

  (assert-equal 'ok (tlc--rust-send-notification
                     root-path
                     "textDocument/didOpen"
                     (list path content)))

  ;; ---------------------------------------------------------------------------
  ;; definition
  ;; ---------------------------------------------------------------------------

  (sleep-for 10)

  (message "Sending definition")

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
            (message "recv-response retry")
            (sleep-for 1)
            (recv-response))
        response)))

  (assert-equal 'no-server (tlc--rust-recv-response "/some/root/path/not/found"))

  (assert-equal '(ok-response 1 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
                (recv-response))

  (test-garbage-collect "after textDocument/definition")

  ;; ---------------------------------------------------------------------------
  ;; didChange
  ;; ---------------------------------------------------------------------------

  (message "Sending didChange")

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

  ;; ---------------------------------------------------------------------------
  ;; definition after didChange
  ;; ---------------------------------------------------------------------------

  (message "Sending definition after didChange")

  (assert-equal 2
                (tlc--rust-send-request
                 root-path
                 "textDocument/definition"
                 '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

  (assert-equal '(ok-response 2 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 8 3)))
                (recv-response))

  ;; ---------------------------------------------------------------------------
  ;; textDocument/didChange to revert the previous change, so that rust-analyzer's
  ;; view matches the file system
  ;; ---------------------------------------------------------------------------

  (message "Sending didChange to revert")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 root-path
                 "textDocument/didChange"
                 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" '((6 0 7 1 "")))))

  ;; ---------------------------------------------------------------------------
  ;; didClose
  ;; ---------------------------------------------------------------------------

  (message "Sending didClose")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 root-path
                 "textDocument/didClose"
                 (list "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs")))

  ;; ---------------------------------------------------------------------------
  ;; didOpen + definition again
  ;; ---------------------------------------------------------------------------

  ;; Do a definition again to ensure that rust-analyzer did not crash due to
  ;; faulty data sent in didClose above. Since it's a notification we can't
  ;; wait for a response.

  (message "Sending didOpen+definition")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 root-path
                 "textDocument/didOpen"
                 (list path content)))

  (assert-equal 3
                (tlc--rust-send-request
                 root-path
                 "textDocument/definition"
                 '("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 4 10)))

  (assert-equal '(ok-response 3 (("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs" 7 3)))
                (recv-response))

  ;; ---------------------------------------------------------------------------
  ;; Stopping the LSP server
  ;; ---------------------------------------------------------------------------

  (message "killing server")

  (tlc--rust-stop-server root-path)

  (assert-equal 'no-server
                (tlc--rust-send-notification
                 root-path
                 "textDocument/didOpen"
                 (list path content)))

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
  )
