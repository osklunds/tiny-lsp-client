
(split-window-below)
(other-window 1)
(switch-to-buffer "*Messages*")
(other-window 1)

(add-to-list 'load-path default-directory)

(define-derived-mode rust-mode prog-mode "Rust"
  "Fake rust-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file (file-truename
                                       (file-name-concat
                                        user-emacs-directory
                                        "tiny-lsp-client-test.log")))
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio t)

(add-hook 'rust-mode-hook 'tlc-mode)

(find-file "src/dummy.rs")
