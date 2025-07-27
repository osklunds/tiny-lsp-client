;;; tiny-lsp-client.el --- Tiny LSP Client  -*- lexical-binding: t; -*-

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
(setq test-file-name "performance-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

;; (run-shell-command "cargo build --release")
(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

;; todo handle nil env
;; todo: make prettier
;; todo: handle in other files too, move this to common
(require 'tlc-rust (concat (getenv "CARGO_TARGET_DIR") "/release/libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))
(require 'eglot)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest xref-tlc-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (tlc-mode)
  (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)

  (xref-test)
  )

(tlc-deftest xref-eglot-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (eglot-ensure)

  (xref-test)
  )

(defun xref-test ()
  (let ((result nil))
    (dotimes (i (point-max))
      (goto-char i)
      (save-excursion
        (ignore-errors
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (throw 'dont-read "dont read"))))
            (non-interactive-xref-find-definitions)))
        (push (list (point)) result)))
    (message "oskar: %s" result)))

