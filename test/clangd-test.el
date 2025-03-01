
;; Run with "emacs -batch -l ert -l new-test.el -f ert-run-tests-batch-and-exit"

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "clangd-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

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

  ;; Sleep to let clangd have time to start and be able to return more
  ;; completions
  (sleep-for 0.5)
  (setq tlc-collection-fun (get-tlc-collection-fun))
  (assert-equal '("other_function") (funcall tlc-collection-fun "oth" nil t))

  (setq pred (lambda (item)
               (string-match-p "function" item)))
  ;; todo: fix below when duplicates are removed
  (assert-equal '("function_in_other_file" "function_in_other_file" "other_function")
                (funcall tlc-collection-fun "" pred t))

  (assert-equal 1 (number-of-completion-requests))
  )
