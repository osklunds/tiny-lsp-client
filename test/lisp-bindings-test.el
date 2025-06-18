;; Copyright (C) 2025 Oskar Lundström

;; This file is part of tiny-lsp-client.

;; tiny-lsp-client is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; tiny-lsp-client is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; tiny-lsp-client. If not, see <https:;;www.gnu.org/licenses/>.


;; Tests focusing on the lisp bindings that lib.rs exposes.

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
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
  (setq server-cmd "clangd")

  (message "Starting server")

  ;; todo: test path that doesn't exist
  (assert-equal 'start-failed (tlc--rust-start-server (list root-path "doesnt_exist")))

  (assert-equal 'started (tlc--rust-start-server (list root-path server-cmd)))

  (pcase (tlc--rust-all-server-info)
    (`((,r ,command ,process-id))
     (assert-equal root-path r)
     (assert-equal server-cmd command)
     (assert-equal t (integerp process-id))
     )
    (x
     (error "unexpected return: %s" x)))

  (assert-equal 'already-started (tlc--rust-start-server (list root-path server-cmd)))

  (message "Server started")

  (test-garbage-collect "after server started")

  ;; ---------------------------------------------------------------------------
  ;; didOpen
  ;; ---------------------------------------------------------------------------

  (message "Sending didOpen")

  (setq file-path (relative-repo-root "test" "clangd" "main.cpp"))
  (setq file-uri (concat "file://" file-path))
  (setq content (with-temp-buffer
                  (insert-file-contents file-path)
                  (buffer-string)))

  (assert-equal 'ok (tlc--rust-send-notification
                     (list root-path server-cmd)
                     "textDocument/didOpen"
                     (list file-uri content)))

  ;; ---------------------------------------------------------------------------
  ;; definition
  ;; ---------------------------------------------------------------------------

  (message "Sending definition")

  (assert-equal 'no-server
                (tlc--rust-send-request
                 (list "//some/root_path/that/does/not/exist" server-cmd)
                 "textDocument/definition"
                 `(,file-uri 4 10)))

  (assert-equal 'no-server
                (tlc--rust-send-request
                 (list root-path "some server cmd not used")
                 "textDocument/definition"
                 `(,file-uri 4 10)))

  (assert-equal 1
                (tlc--rust-send-request
                 (list root-path server-cmd)
                 "textDocument/definition"
                 `(,file-uri 10 4)))

  ;; todo: need to loop even if less clear, becase now is unstable and
  ;; wastefully slow
  (sleep-for 2)

  (assert-equal 'no-server (tlc--rust-recv-response
                            (list "/some/root/path/not/found" server-cmd) 100))

  (assert-equal 'no-server (tlc--rust-recv-response
                            (list root-path "some server cmd") 100))

  (assert-equal `(response 1 t ((,file-uri 4 6)))
                (tlc--rust-recv-response (list root-path server-cmd) 0)
                "recv resp first definition")

  (test-garbage-collect "after textDocument/definition")

  ;; ---------------------------------------------------------------------------
  ;; didChange
  ;; ---------------------------------------------------------------------------

  (message "Sending didChange")

  (assert-equal 'no-server
                (tlc--rust-send-notification
                 (list "/some/root/path/not/found" server-cmd)
                 "textDocument/didChange"
                 (list file-uri '((3 0 3 0 "\n")))))

  (assert-equal 'no-server
                (tlc--rust-send-notification
                 (list root-path "some server cmd")
                 "textDocument/didChange"
                 (list file-uri '((3 0 3 0 "\n")))))

  (assert-equal 'ok
                (tlc--rust-send-notification
                 (list root-path server-cmd)
                 "textDocument/didChange"
                 (list file-uri '((3 0 3 0 "\n")))))

  ;; ---------------------------------------------------------------------------
  ;; definition after didChange
  ;; ---------------------------------------------------------------------------

  (message "Sending definition after didChange")

  (assert-equal 2
                (tlc--rust-send-request
                 (list root-path server-cmd)
                 "textDocument/definition"
                 `(,file-uri 11 4)))

  (sleep-for 2)

  (assert-equal `(response 2 t ((,file-uri 5 6)))
                (tlc--rust-recv-response (list root-path server-cmd) 0))

  ;; ---------------------------------------------------------------------------
  ;; textDocument/didChange to revert the previous change, so that clangd's
  ;; view matches the file system
  ;; ---------------------------------------------------------------------------

  (message "Sending didChange to revert")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 (list root-path server-cmd)
                 "textDocument/didChange"
                 `(,file-uri ((3 0 4 1 "")))))

  ;; ---------------------------------------------------------------------------
  ;; didClose
  ;; ---------------------------------------------------------------------------

  (message "Sending didClose")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 (list root-path server-cmd)
                 "textDocument/didClose"
                 `(,file-uri)))

  ;; ---------------------------------------------------------------------------
  ;; didOpen + definition again
  ;; ---------------------------------------------------------------------------

  ;; Do a definition again to ensure that clangd did not crash due to
  ;; faulty data sent in didClose above. Since it's a notification we can't
  ;; wait for a response.

  (message "Sending didOpen+definition")

  (assert-equal 'ok
                (tlc--rust-send-notification
                 (list root-path server-cmd)
                 "textDocument/didOpen"
                 `(,file-uri ,content)))

  (assert-equal 3
                (tlc--rust-send-request
                 (list root-path server-cmd)
                 "textDocument/definition"
                 `(,file-uri 10 4)))

  (sleep-for 2)

  (assert-equal `(response 3 t ((,file-uri 4 6)))
                (tlc--rust-recv-response (list root-path server-cmd) 0))

  ;; ---------------------------------------------------------------------------
  ;; Stopping the LSP server
  ;; ---------------------------------------------------------------------------

  (message "Stopping server")

  (tlc--rust-stop-server (list root-path server-cmd))

  (assert-equal 'no-server
                (tlc--rust-send-notification
                 (list root-path server-cmd)
                 "textDocument/didOpen"
                 `(,file-uri ,content)))

  (assert-equal 'no-server
                (tlc--rust-send-request
                 (list root-path server-cmd)
                 "textDocument/definition"
                 `(,file-uri 4 10)))

  (assert-equal 'no-server (tlc--rust-recv-response (list root-path server-cmd) 0))

  ;; -----------------------------------------------------------------------------
  ;; End
  ;; -----------------------------------------------------------------------------

  (test-garbage-collect "in the end")
  )
