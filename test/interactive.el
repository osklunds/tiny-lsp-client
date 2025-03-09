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


(setq debug-on-error t)

(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'auto-save-default nil)
(setq create-lockfiles nil)

(require 'cc-mode)

(define-derived-mode erlang-mode prog-mode "Erlang"
  "Fake erlang-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(define-derived-mode rust-mode prog-mode "Rust"
  "Fake rust-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(split-window-below)
(other-window 1)
(switch-to-buffer "*Messages*")
(other-window 1)

(add-to-list 'load-path default-directory)

(shell-command "cargo build")

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
(customize-set-variable 'tlc-find-root-function 'tlc-dev-find-root-function)

(add-hook 'c++-mode-hook 'tlc-mode)
(add-hook 'erlang-mode-hook 'tlc-mode)
(add-hook 'rust-mode-hook 'tlc-mode)

(add-hook 'tlc-mode-hook 'tlc-use-xref)
(add-hook 'tlc-mode-hook 'tlc-use-sync-capf)

(define-key prog-mode-map (kbd "M-p") 'completion-at-point)
(define-key prog-mode-map (kbd "M-o") 'tlc--async-collection-fun)

(find-file "test/clangd/main.cpp")
;; (find-file "test/erlang_ls/my_module.erl")

(re-search-forward "other_function" nil nil 2)
;; (re-search-forward "other_function" nil nil 1)
(next-line)
