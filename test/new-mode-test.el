
;; Tests focusing on tlc-mode itself, not specific to any LSP server. Using
;; clangd since it's faster to start than rust-analyzer.

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "new-common.el"))
(setq test-file-name "new-mode-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

;; todo: move cargo build to common
(run-shell-command "cargo build")
(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

;; Manually require tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'c++-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest start-server-hooks ()
  (defvar num-before-hook-calls 0)
  (defvar num-after-hook-calls 0)

  (defvar hook-last-caller 'after)

  (defun before-hook ()
    (assert-equal (tlc--root) default-directory)
    (cl-incf num-before-hook-calls)
    (assert-equal 'after hook-last-caller)
    (setq hook-last-caller 'before)
    (message "before-hook called"))

  (defun after-hook ()
    (assert-equal (tlc--root) default-directory)
    (cl-incf num-after-hook-calls)
    (assert-equal 'before hook-last-caller)
    (setq hook-last-caller 'after)
    (message "after-hook called"))

  (add-hook 'tlc-before-start-server-hook 'before-hook)
  (add-hook 'tlc-after-start-server-hook 'after-hook)

  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 'c++-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 1 num-before-hook-calls)
  (assert-equal 1 num-after-hook-calls)

  (find-file (relative-repo-root "test" "clangd" "other.cpp"))

  (assert-equal 2 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 1 num-before-hook-calls)
  (assert-equal 1 num-after-hook-calls)
  )
