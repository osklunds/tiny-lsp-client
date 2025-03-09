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
(setq test-file-name "rust-analyzer-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cargo build")

(define-derived-mode rust-mode prog-mode "Rust"
  "Fake rust-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Manually add tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)
(customize-set-variable 'tlc-find-root-function 'tlc-dev-find-root-function)

(add-hook 'rust-mode-hook 'tlc-mode)
(add-hook 'tlc-mode-hook 'tlc-use-xref)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open-a-file-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))

  ;; Assert
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'rust-mode major-mode)
  (assert-equal t tlc-mode)
  ;; todo: understand why the t is added
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; to see that correct root is used by test cases
  (assert (string-suffix-p "tiny-lsp-client/test/rust_analyzer/" tlc--cached-root))
  )

(tlc-deftest find-definition-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (re-search-forward "other_function")
  (assert-equal 3 (line-number-at-pos))
  (assert-equal 18 (current-column))

  ;; Act
  (sleep-for 2)
  ;; todo: no longer needed once non-busy-wait is used for tlc-sync-request
  (let ((max-lisp-eval-depth 100000))
    (non-interactive-xref-find-definitions))

  ;; Assert
  (assert-equal 10 (line-number-at-pos))
  (assert-equal 3 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest edit-test ()
  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))

  (assert-equal 
   "
fn main() {
    other_function(2);

    println!(\"Hello, world!\");
}



fn other_function(arg: u32) -> u32 {
    arg + 1
}
"
   (current-buffer-string))

  (beginning-of-buffer)
  (insert "\n")

  (re-search-forward "other_function")
  (insert "_")
  (insert "h")
  (insert "e")
  (insert "j")

  (next-line)
  (insert "\n\n")

  (re-search-forward "other_function")
  (insert "_hej")

  (assert-equal 
   "

fn main() {
    other_function_hej(2);



    println!(\"Hello, world!\");
}



fn other_function_hej(arg: u32) -> u32 {
    arg + 1
}
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "other")
  (assert-equal 4 (line-number-at-pos))
  (assert-equal 9 (current-column))

  (sleep-for 2)
  ;; todo: no longer needed once non-busy-wait is used for tlc-sync-request
  (let ((max-lisp-eval-depth 100000))
    (non-interactive-xref-find-definitions))

  (assert-equal 13 (line-number-at-pos))
  (assert-equal 3 (current-column))
  )

(tlc-deftest revert-buffer-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'rust-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act
  (revert-buffer nil 'no-confirm 'preserve-modes)

  ;; Assert
  (assert-equal 'rust-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  ;; todo: for all revert tests, need action afterwards to see that didClose
  ;; didn't cause issues
  )

(tlc-deftest capf-test ()
  (add-hook 'tlc-mode-hook 'tlc-use-sync-capf)
  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))
  (assert-equal 0 (number-of-completion-requests))

  (re-search-forward "other_function")
  ;; Need to find def to detect when rust_analyzer is ready
  (sleep-for 2)
  (let ((max-lisp-eval-depth 100000))
    (non-interactive-xref-find-definitions))

  (assert-equal 10 (line-number-at-pos))
  (assert-equal 3 (current-column))

  (beginning-of-buffer)
  (re-search-forward "other_function")
  (next-line)

  (setq tlc-collection-fun (get-tlc-collection-fun))
  (assert-equal 0 (number-of-completion-requests))

  (let ((result (funcall tlc-collection-fun "" nil t)))
    (assert-equal 1 (number-of-completion-requests))
    (dolist (exp '("main" "other_function" "assert_eq!"))
      (assert (cl-member exp result :test 'string-equal) exp))
    )
  )

;; Only been able to trigger this in rust-analyzer
(tlc-deftest null-result-test ()
  (add-hook 'tlc-mode-hook 'tlc-use-sync-capf)
  (find-file (relative-repo-root "test" "rust_analyzer" "src" "main.rs"))
  (assert-equal 0 (number-of-completion-requests))

  (assert-equal 
   "
fn main() {
    other_function(2);

    println!(\"Hello, world!\");
}



fn other_function(arg: u32) -> u32 {
    arg + 1
}
"
   (current-buffer-string))

  (re-search-forward "other_function")
  ;; Need to find def to detect when rust_analyzer is ready
  (sleep-for 2)
  (let ((max-lisp-eval-depth 100000))
    (non-interactive-xref-find-definitions))

  (assert-equal 10 (line-number-at-pos))
  (assert-equal 3 (current-column))

  (beginning-of-buffer)
  (re-search-forward "other_function")
  (end-of-line)
  (insert " // comment")

  (assert-equal 
   "
fn main() {
    other_function(2); // comment

    println!(\"Hello, world!\");
}



fn other_function(arg: u32) -> u32 {
    arg + 1
}
"
   (current-buffer-string))

  ;; When in a comment, null result
  (let ((result (funcall (get-tlc-collection-fun) "" nil t)))
    (assert-equal 1 (number-of-completion-requests))
    (assert-not result "null result")
    )

  (next-line)

  ;; As soon as move outside, gets some result
  (let ((result (funcall (get-tlc-collection-fun) "" nil t)))
    (assert-equal 2 (number-of-completion-requests))
    (assert result "non null")
    (assert (cl-member "other_function" result :test 'string-equal))
    )

)
