
;; -----------------------------------------------------------------------------
;; Load common test functions
;; -----------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(load "test/common.el")

;; -----------------------------------------------------------------------------
;; Preparation
;; -----------------------------------------------------------------------------

(let ((default-directory (file-name-concat default-directory "test" "clangd")))
  (assert-equal 0 (call-process-shell-command "cmake .") "cmake")
  (assert-equal 0 (call-process-shell-command "make") "make")
  )

;; Manually add tlc-rust to get debug version
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'c++-mode-hook 'tlc-mode)

(delete-file log-file-name)

;; -----------------------------------------------------------------------------
;; Opening a file
;;------------------------------------------------------------------------------

(assert-equal 0 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(find-file "test/clangd/main.cpp")

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(assert-equal 'c++-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Xref find definition
;;------------------------------------------------------------------------------

(re-search-forward "other_function")
(re-search-forward "other_function")
(assert-equal 11 (line-number-at-pos))
(assert-equal 18 (current-column))

(non-interactive-xref-find-definitions)

(assert-equal 5 (line-number-at-pos))
(assert-equal 6 (current-column))

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------

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

(non-interactive-xref-find-definitions)
(assert-equal 5 (line-number-at-pos))
(assert-equal 6 (current-column))

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

(non-interactive-xref-find-definitions)
(assert-equal 5 (line-number-at-pos))
(assert-equal 29 (current-column))

;; -----------------------------------------------------------------------------
;; Revert buffer
;; -----------------------------------------------------------------------------

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close)) 

(revert-buffer nil 'no-confirm 'preserve-modes)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 2 (number-of-did-open))
(assert-equal 1 (number-of-did-close)) 
