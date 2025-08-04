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


;; Run with "emacs -batch -l ert -l new-test.el -f ert-run-tests-batch-and-exit"

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "clangd-test")
(common-setup)

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

(add-hook 'c++-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open-a-file-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal nil tlc-mode)

  ;; Act
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  ;; Assert
  (assert-equal 1 (number-of-did-open) "after")
  (assert-equal 0 (number-of-did-close) "after")

  (assert-equal 'c++-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)
  )

(tlc-deftest find-definition-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (re-search-forward "other_function")
  (re-search-forward "other_function")
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; Assert
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  (assert-equal 1 (number-of-did-open) "end")
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest edit-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 
   "
#include <iostream>
#include \"other.hpp\"

short other_function(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {
    other_function(123);

    function_in_other_file();
}
"
   (current-buffer-string))
  
  ;; Act
  (beginning-of-buffer)
  (re-search-forward "other_function")
  (insert "_")
  (insert "h")
  (insert "e")
  (insert "j")

  (re-search-forward "other_function")
  (insert "_hej")

  (assert-equal 
   "
#include <iostream>
#include \"other.hpp\"

short other_function_hej(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {
    other_function_hej(123);

    function_in_other_file();
}
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "other_")
  (re-search-forward "other_")
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 10 (current-column))

  ;; Assert

  ;; Use find definition as a way to see that the contents seem synced
  ;; todo: need to insert some newlines
  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  ;; Act2
  (beginning-of-buffer)
  (re-search-forward "arg")
  (backward-delete-char 2)
  (re-search-forward "arg")
  (beginning-of-line)
  (replace-string "arg" "a")

  (assert-equal 
   "
#include <iostream>
#include \"other.hpp\"

short other_function_hej(int a) {
    std::cout << a << std::endl;
    return 1;
}

int main() {
    other_function_hej(123);

    function_in_other_file();
}
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "<< a")
  (backward-char)
  (assert-equal 6 (line-number-at-pos))
  (assert-equal 17 (current-column))

  ;; Assert2

  ;; Use find definition as a way to see that the contents seem synced
  ;; todo: need to insert some newlines
  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 29 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest revert-buffer-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'c++-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act
  (revert-buffer nil 'no-confirm 'preserve-modes)

  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close)) 
  )

(tlc-deftest capf-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal 0 (number-of-completion-requests))

  (re-search-forward "function_in_other_file")
  (previous-line)

  (setq tlc-collection-fun (get-tlc-collection-fun))
  (assert-equal '("other_function") (funcall tlc-collection-fun "oth" nil t))

  (setq pred (lambda (item)
               (string-match-p "function" item)))
  (let ((result (funcall tlc-collection-fun "" nil t)))
    (assert (cl-member "function_in_other_file" result :test 'string-equal))
    (assert (cl-member "other_function" result :test 'string-equal))
    (assert-not (cl-member "junk" result :test 'string-equal)))

  (assert-equal 1 (number-of-completion-requests))
  )

(tlc-deftest eldoc-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (re-search-forward "other_function" nil nil 2)

  (run-until 10 0.1
    (assert-equal "function other_function

→ short
Parameters:
- int arg

short other_function(int arg)" (get-eldoc-msg)))
  )
