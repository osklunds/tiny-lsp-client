
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

(tlc-deftest toggle-tlc-mode ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act1
  (tlc-mode -1)

  ;; Assert1
  (assert-equal nil tlc-mode)
  (assert-equal '(etags--xref-backend) xref-backend-functions)

  (assert-equal 1 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  ;; Act2
  (tlc-mode t)

  ;; Assert2
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))
  )

(tlc-deftest toggle-tlc-mode-by-changing-major-mode ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 'c++-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act1
  (text-mode)

  ;; Assert1
  (assert-equal 1 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))
  (assert-equal 'text-mode major-mode)
  (assert-equal nil tlc-mode)
  (assert-equal '(etags--xref-backend) xref-backend-functions)

  ;; Act2
  (c++-mode)
  (assert-equal 'c++-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))
  )

(tlc-deftest reverting-buffer ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act1
  (revert-buffer-quick)

  ;; Assert1
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  ;; Act2
  (revert-buffer nil 'no-confirm)

  ;; Assert2
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 3 (number-of-did-open))
  (assert-equal 2 (number-of-did-close)) 

  ;; Act3
  ;; When preserve-modes is true
  (revert-buffer nil 'no-confirm 'preserve-modes)

  ;; Assert3
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 4 (number-of-did-open))
  (assert-equal 3 (number-of-did-close))
  )

(tlc-deftest edit ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

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
  ;; Some single-character edits
  (insert "h")
  (insert "e")
  (insert "j")

  (beginning-of-line)
  (re-search-forward "other")
  (backward-delete-char 1)
  (backward-delete-char 1)
  (backward-delete-char 1)

  (re-search-forward "other_function")
  ;; Some multi-character edits
  (insert "hej")
  (end-of-line)
  (insert "\n") ;; and a newline
  (insert "third_function();\n")

  (previous-line 2)
  (beginning-of-line)
  (re-search-forward "other")
  (backward-delete-char 3)

  (beginning-of-buffer)
  (next-line 3)
  (insert "i")
  (insert "n")
  (insert "t")
  (insert " third_function()")
  (insert " ")
  (insert "{ return 7;")
  (insert " ")
  (insert "}")

  (beginning-of-buffer)
  (re-search-forward "main")
  (previous-line)
  (backward-delete-char 1)

  (assert-equal 
   "
#include <iostream>
#include \"other.hpp\"
int third_function() { return 7; }
short ot_functionhej(int arg) {
    std::cout << arg << std::endl;
    return 1;
}
int main() {
    ot_functionhej(123);
third_function();

    function_in_other_file();
}
"
   (current-buffer-string))

  )








