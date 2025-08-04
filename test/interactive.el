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

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "interactive-test")
(common-setup)
(set-log-file-name "interactive")

;; -----------------------------------------------------------------------------
;; Interactive specific
;; -----------------------------------------------------------------------------

(setq debug-on-error t)

(fset 'yes-or-no-p 'y-or-n-p)

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

(add-hook 'c++-mode-hook 'tlc-mode)
(add-hook 'erlang-mode-hook 'tlc-mode)
(add-hook 'rust-mode-hook 'tlc-mode)

(define-key prog-mode-map (kbd "M-p") 'completion-at-point)
(define-key prog-mode-map (kbd "M-o") 'tlc--async-collection-fun)

;; (find-file "test/clangd/main.cpp")
(find-file "test/erlang_ls/my_module.erl")
(delete-other-windows)

;; (re-search-forward "other_function" nil nil 2)
;; (re-search-forward "other_function" nil nil 1)
;; (next-line)

(add-hook 'eldoc-documentation-functions #'tlc-eldoc-function nil t)
(eldoc-mode t)

