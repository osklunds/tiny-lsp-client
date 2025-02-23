
;; Run with "emacs -batch -l ert -l new-test.el -f ert-run-tests-batch-and-exit"

;; -----------------------------------------------------------------------------
;; Common setup
;; -----------------------------------------------------------------------------

;; Catch-22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load-file (relative-repo-root "test" "common.el"))

(defun my-assert-equal (exp act &optional label)
  (when (not (equal exp act))
    (message "")
    (message "")
    (message "")
    (message "-----------------------------------------------------------------------------")
    (message "Assert failed. label: '%s'" label)
    (message "Exp: '%s'" exp)
    (message "Act: '%s'" act)
    (message "-----------------------------------------------------------------------------")
    (message "")
    (message "")
    (message ""))
  (should (equal exp act)))

(defun run-shell-command (command &rest components)
  (let* ((default-directory (apply 'relative-repo-root components))
         (code (with-temp-buffer
                 (let ((code (call-process-shell-command command nil t)))
                   (message (string-replace "%" "%%" (buffer-string)))
                   code))))
    (my-assert-equal 0 code)))

(run-shell-command "cargo build")
(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

;; Manually require tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'c++-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Common helpers
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(ert-deftest my-test ()
  (delete-file log-file-name)

  (my-assert-equal 0 (number-of-did-open))
  (my-assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (my-assert-equal 1 (number-of-did-open))
  (my-assert-equal 0 (number-of-did-close) "after")
  )

(ert-deftest other-test ()
  (should (= 2 2)))
