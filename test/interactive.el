
(setq debug-on-error t)

(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'auto-save-default nil)
(setq create-lockfiles nil)

(require 'cc-mode)

(split-window-below)
(other-window 1)
(switch-to-buffer "*Messages*")
(other-window 1)

(add-to-list 'load-path default-directory)


;; Manually require tlc-rust to get debug version, faster to compile that release
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
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

(add-hook 'c++-mode-hook 'tlc-mode)

(define-key c++-mode-map (kbd "M-p") 'completion-at-point)

(find-file "test/clangd/interactive.cpp")

(re-search-forward "other_function" nil nil 2)
(next-line)
