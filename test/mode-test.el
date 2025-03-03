;;;  -*- lexical-binding: t; -*-

;; Tests focusing on tlc-mode itself, not specific to any LSP server. Using
;; clangd since it's faster to start than rust-analyzer.

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
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
;; todo: Add proper tests for these
(add-hook 'tlc-mode-hook 'tlc-use-xref)
(add-hook 'tlc-mode-hook 'tlc-use-sync-capf)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

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

(defun assert-tlc-info (exp-root-path exp-command)
  (let ((info (tlc-info)))
    (assert-equal 1 (length info))
    (pcase (tlc-info)
      (`((,root-path ,command ,process-id))
       (assert-equal exp-root-path root-path)
       (assert-equal exp-command command)
       (assert-equal t (integerp process-id))
       process-id
       )
      (x
       (error "unexpected return: %s" x)))))

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest start-server-hooks-test ()
  (setq num-before-hook-calls 0)
  (setq num-after-hook-calls 0)
  (setq hook-last-caller 'after)

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

(tlc-deftest toggle-tlc-mode-test ()
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

(tlc-deftest toggle-tlc-mode-by-changing-major-mode-test ()
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

(tlc-deftest revert-buffer-test ()
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

(tlc-deftest edit-test ()
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
  ;; Deletes a newline. Important for the test so that the line numbers change
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

  ;; Assert
  (beginning-of-buffer)
  (re-search-forward "third_function")
  (re-search-forward "third_function")

  (assert-equal 11 (line-number-at-pos))
  (assert-equal 14 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 4 (line-number-at-pos))
  (assert-equal 4 (current-column))

  (beginning-of-buffer)
  (re-search-forward "ot_functionhej")
  (re-search-forward "ot_functionhej")

  (assert-equal 10 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest edit-with-restriction-test ()
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
  (re-search-forward "other_function")

  (let* ((p (point))
         (source-buffer (current-buffer)))
    (with-temp-buffer
      (insert "\n") ;; Important so that line numbers change
      (insert "\n")
      (insert "abc")
      (let ((temp-buffer (current-buffer)))
        (with-current-buffer source-buffer
          (narrow-to-region (- p (length "    other_function")) p)
          (replace-buffer-contents temp-buffer)))))

  (widen)

  (beginning-of-buffer)
  (re-search-forward "other_function")
  (backward-delete-char (length "other_function"))
  (insert "abc")

  (assert-equal 
   "
#include <iostream>
#include \"other.hpp\"

short abc(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {


abc(123);

    function_in_other_file();
}
"
   (current-buffer-string))

  ;; Assert
  (beginning-of-buffer)
  (re-search-forward "abc")
  (re-search-forward "abc")
  (assert-equal 13 (line-number-at-pos))
  (assert-equal 3 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  )

(tlc-deftest kill-buffer-test ()
  ;; Arrange
  (setq file (relative-repo-root "test" "clangd" "main.cpp"))

  (find-file file)

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (kill-buffer)

  ;; Assert
  (assert-equal 1 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  ;; Opening the same file should result in didOpen again
  (find-file file)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))
  )

(tlc-deftest open-non-existing-file-test ()
  ;; Arrange
  (setq non-existing-file "doesnt_exist_right.cpp")
  (assert-equal nil (file-exists-p non-existing-file))
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (find-file non-existing-file)

  ;; Assert
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest stop-server-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal 1 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (setq root-path (tlc--root))
  (assert-equal t (stringp root-path))

  (assert-tlc-info root-path "clangd")

  ;; Act
  (tlc-stop-server)

  ;; To see that spamming doesn't cause issues
  (tlc-stop-server)
  (tlc-stop-server)
  (tlc-stop-server)

  ;; Assert
  (assert-equal 0 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest kill-server-process-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal 1 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (setq root-path (tlc--root))
  (assert-equal t (stringp root-path))

  (setq pid (assert-tlc-info root-path "clangd"))

  ;; Act
  (shell-command (format "kill -9 %s" pid))

  ;; Assert
  (sleep-for 0.1) ;; Avoid race
  (assert-equal 0 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest restart-server-test ()
  ;; Arrange
  (setq num-before-hook-calls 0)
  (setq num-after-hook-calls 0)
  (setq hook-last-caller 'after)

  (add-hook 'tlc-before-start-server-hook 'before-hook)
  (add-hook 'tlc-after-start-server-hook 'after-hook)

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal 1 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 1 num-before-hook-calls)
  (assert-equal 1 num-after-hook-calls)

  (tlc-stop-server)
  (assert-equal 0 (length (tlc-info)))

  ;; Act
  (tlc-restart-server)

  ;; Assert
  (assert-equal 1 (length (tlc-info)))
  (assert-equal 2 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 2 num-before-hook-calls)
  (assert-equal 2 num-after-hook-calls)
  )

(tlc-deftest completion-at-point-end-to-end-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
  (assert-equal 0 (number-of-completion-requests))

  (re-search-forward "other_function" nil nil 2)
  (next-line)

  ;; Act
  ;; This is end-to-end, but not much verification except no crashes can be done
  (completion-at-point)

  ;; Assert
  (assert-equal 1 (number-of-completion-requests))
  (completion-at-point)
  (assert-equal 2 (number-of-completion-requests))
  )

(tlc-deftest capf-all-completions-test ()
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal 0 (number-of-completion-requests))

  (re-search-forward "last_variable")
  (next-line)

  ;; Sleep to let clangd have time to start and be able to return more
  ;; completions
  (sleep-for 0.5)
  (setq tlc-collection-fun (get-tlc-collection-fun))

  ;; Completions are lazily fetched
  (assert-equal 0 (number-of-completion-requests))

  (setq result1 nil)
  (let ((result (funcall tlc-collection-fun "" nil t)))
    (setq result1 result)
    ;; After first call, a request is sent
    (assert-equal 1 (number-of-completion-requests))

    (assert-equal t (>= (length result1) 100))
    (dolist (exp '("my_fun1" "my_fun2" "my_fun3" "my_fun4" "my_fun5"
                   "my_function1" "my_function2" "my_function3" "my_function4"
                   "my_function5"
                   "my_var1" "my_var2" "my_var3" "my_var4"
                   "my_variable1" "my_variable2" "my_variable3" "my_variable4"
                   "last_variable" "last_function"))
      (assert (cl-member exp result :test 'string-equal) exp))
    (assert-not (cl-member "junk" result :test 'string-equal))

    ;; Results are cached, so no new request due to the above
    (assert-equal 1 (number-of-completion-requests)))

  (let ((result (funcall tlc-collection-fun "" nil t)))
    ;; Still 1 thanks to cache, same tlc-collection-fun is being used
    (assert-equal 1 (number-of-completion-requests))
    (assert-equal result result1))

  ;; With prefix
  (let ((result (funcall tlc-collection-fun "my_f" nil t)))
    (assert-equal '("my_fun1" "my_fun2" "my_fun3" "my_fun4" "my_fun5"
                    "my_function1" "my_function2" "my_function3"
                    "my_function4" "my_function5")
                  result)
    )

  ;; With pred
  (setq pred (lambda (item)
               (string-match-p "y_function" item)
               ))
  (let ((result (funcall tlc-collection-fun "" pred t)))
    (assert-equal '("my_function1" "my_function2" "my_function3"
                    "my_function4" "my_function5" "__PRETTY_FUNCTION__")
                  result)
    )

  ;; With prefix and pred
  (let ((result (funcall tlc-collection-fun "my" pred t)))
    (assert-equal '("my_function1" "my_function2" "my_function3"
                    "my_function4" "my_function5")
                  result)
    )
  )

(tlc-deftest capf-test-completion-test ()
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal 0 (number-of-completion-requests))
  (re-search-forward "last_variable")
  (next-line)

  (sleep-for 0.5)
  (setq tlc-collection-fun (get-tlc-collection-fun))

  (assert-equal 0 (number-of-completion-requests))
  (assert-equal nil (funcall tlc-collection-fun "my_fun"    nil 'lambda))
  (assert-equal t   (funcall tlc-collection-fun "my_fun1" nil 'lambda))

  (setq pred (lambda (item)
               (not (string-match-p "1" item))
               ))
  ;; Note, as soon as pred passed, what used to be a match is no longer a match
  (assert     (funcall tlc-collection-fun "my_fun1" nil  'lambda))
  (assert-not (funcall tlc-collection-fun "my_fun1" pred 'lambda))
  (assert     (funcall tlc-collection-fun "my_var2" pred 'lambda) "my_var2")

  (assert-equal 1 (number-of-completion-requests))
  )

(tlc-deftest capf-try-completion-test ()
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal 0 (number-of-completion-requests))
  (re-search-forward "last_variable")
  (next-line)

  (sleep-for 0.5)
  (setq tlc-collection-fun (get-tlc-collection-fun))

  (assert-equal 0 (number-of-completion-requests))
  (assert-equal nil            (funcall tlc-collection-fun "junk"           nil nil))
  (assert-equal "my_"          (funcall tlc-collection-fun "my"             nil nil))
  (assert-equal "my_fun"       (funcall tlc-collection-fun "my_f"           nil nil))
  (assert-equal "my_function"  (funcall tlc-collection-fun "my_func"        nil nil))
  (assert-equal t              (funcall tlc-collection-fun "my_function1"   nil nil))
  (assert-equal 1 (number-of-completion-requests))

  (setq pred (lambda (item)
               (string-match-p "fun" item)
               ))
  ;; Note, as soon as pred passed, can complete longer because var no longer valid
  (assert-equal "my_"    (funcall tlc-collection-fun "my" nil nil))
  (assert-equal "my_fun" (funcall tlc-collection-fun "my" pred nil))
  )

(tlc-deftest capf-cache-test ()
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal
   "
void my_fun1() {}
short my_fun2() { return 1; }
int my_fun3() { return 1; }
long my_fun4() { return 1; }
char my_fun5() { return 1; }

void my_function1() {}
short my_function2() { return 1; }
int my_function3() { return 1; }
long my_function4() { return 1; }
char my_function5() { return 1; }

void last_function() {
    short my_var1 = 1;
    int my_var2 = 1;
    long my_var3 = 1;
    char my_var4 = 1;

    short my_variable1 = 1;
    int my_variable2 = 1;
    long my_variable3 = 1;
    char my_variable4 = 1;

    int last_variable = 2;

}
"
   (current-buffer-string))

  (re-search-forward "last_variable")
  (next-line)

  (sleep-for 0.5)
  (setq tlc-collection-fun (get-tlc-collection-fun))

  (assert-equal 0 (number-of-completion-requests))
  (let ((result (funcall tlc-collection-fun "my_variable" nil t)))
    (assert-equal '("my_variable1" "my_variable2" "my_variable3" "my_variable4")
                  result)
    )
  (assert-equal 1 (number-of-completion-requests))

  (insert "    int my_variable_new = 3;\n")

  (assert-equal
   "
void my_fun1() {}
short my_fun2() { return 1; }
int my_fun3() { return 1; }
long my_fun4() { return 1; }
char my_fun5() { return 1; }

void my_function1() {}
short my_function2() { return 1; }
int my_function3() { return 1; }
long my_function4() { return 1; }
char my_function5() { return 1; }

void last_function() {
    short my_var1 = 1;
    int my_var2 = 1;
    long my_var3 = 1;
    char my_var4 = 1;

    short my_variable1 = 1;
    int my_variable2 = 1;
    long my_variable3 = 1;
    char my_variable4 = 1;

    int last_variable = 2;
    int my_variable_new = 3;

}
"
   (current-buffer-string))

  ;; Since same collection fun, same result without the new variable
  (let ((result (funcall tlc-collection-fun "my_variable" nil t)))
    (assert-equal '("my_variable1" "my_variable2" "my_variable3" "my_variable4")
                  result)
    )

  (setq new-tlc-collection-fun (get-tlc-collection-fun))
  (assert-equal 1 (number-of-completion-requests))

  ;; With a new collection fun, the new variable is visible
  (let ((result (funcall new-tlc-collection-fun "my_variable" nil t)))
    (assert-equal '("my_variable1" "my_variable2" "my_variable3" "my_variable4"
                    "my_variable_new")
                  result)
    )
  (assert-equal 2 (number-of-completion-requests))

  )

;; test other servers too
;; investigate what company does
;; remove duplicates in lib.rs
