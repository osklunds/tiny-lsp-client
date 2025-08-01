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
(setq test-file-name "mode-test")
(common-setup)

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

(add-hook 'c++-mode-hook 'tlc-mode)

(define-derived-mode erlang-mode prog-mode "Erlang"
  "Fake erlang-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(add-hook 'erlang-mode-hook 'tlc-mode)

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
  (assert-equal t tlc-mode "mode")
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
  (assert tlc-mode)

  ;; Act1
  (tlc-mode -1)

  ;; Assert1
  (assert-not tlc-mode)

  (assert-equal 1 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  ;; Act2
  (tlc-mode t)

  ;; Assert2
  (assert tlc-mode)

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
  (assert-equal 0 (number-of-did-change))

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
  (assert-equal 1 (number-of-did-change))

  (insert "e")
  (insert "j")

  (assert-equal 3 (number-of-did-change))

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

  (assert-equal 9 (number-of-did-change))

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
  (setq non-existing-file (relative-repo-root "test" "clangd" "doesnt_exist_right.cpp"))
  (assert-equal nil (file-exists-p non-existing-file))
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (find-file non-existing-file)

  ;; Assert
  ;; Even if the file doesn't exist it should still be opened towards
  ;; the LSP server
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
  (cl-letf* ((entry (format "%s @ clangd" root-path))
             ((symbol-function 'completing-read)
              (lambda (_ collection _ _ _ _ default &rest _)
                (assert-equal (list entry) collection)
                (assert-equal entry default "default")
                (nth 0 collection))))
    (tlc-stop-server))

  ;; To see that spamming doesn't cause issues
  (tlc--stop-server)
  (tlc--stop-server)
  (tlc--stop-server)

  ;; To see that tlc-stop-server works also from a buffer not in tlc-mode
  (switch-to-buffer "some-buffer")
  (assert-not tlc-mode)
  (catch 'okay
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_ collection _ _ _ _ default &rest _)
                 (assert-not collection)
                 (assert-not default)
                 (throw 'okay "okay"))))
      (tlc-stop-server)))

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
  (run-until 10 0.1
    (assert-equal 0 (length (tlc-info))))
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

  (tlc--stop-server)
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
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
  (assert-equal 0 (number-of-completion-requests))

  (re-search-forward "last_variable")
  (next-line)

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
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
  (assert-equal 0 (number-of-completion-requests))
  (re-search-forward "last_variable")
  (next-line)

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
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
  (assert-equal 0 (number-of-completion-requests))
  (re-search-forward "last_variable")
  (next-line)

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
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
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

(defun has-had-max-num-of-timestamps-many-times ()
  (let* ((cmd (format
               "cat %s | grep \"send thread has '10' timestamps\" | wc -l"
               log-file-name))
         (count (string-to-number (shell-command-to-string cmd))))
    (> count 25)))

(tlc-deftest timestamp-overflow-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-not (has-had-max-num-of-timestamps-many-times) "before")

  ;; Act
  (let ((done nil))
    (while (not done)
      (dotimes (_ 1000)
        ;; need to call internal functions to be able to test async
        (tlc--request
         "textDocument/definition"
         (list (tlc--buffer-file-name) 0 0)
         (tlc--server-key)))
      (setq done (has-had-max-num-of-timestamps-many-times))))

  ;; Assert
  ;; Doesn't test that the latest were removed, just that the code is triggered.
  ;; I don't know a good way to test that oldest really is removed without
  ;; a fake LSP server
  ;; todo: maybe check the IO - receved (x ms) logs. That last 10 should
  ;; have numbers
  (assert (has-had-max-num-of-timestamps-many-times) "after")
  )

;; This test is a bit hacky and complicated, but when I refactored
;; tlc--wait-for-response it caught bugs, so it's important.
(tlc-deftest capf-interrupted-by-user-input-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)
  (re-search-forward "last_variable")
  (next-line)

  (setq tlc-collection-fun (get-tlc-collection-fun))
  (assert-equal 0 (number-of-completion-requests))

  (setq result 'none)
  (setq tlc--last-candidates '("previous candidate"))

  ;; Act
  ;; Trick from https://stackoverflow.com/a/32972563 used
  (let ((unread-command-events (listify-key-sequence (kbd "a")))
        (noninteractive nil)
        (tlc-interruptible-capf t))
    (setq result (funcall tlc-collection-fun "" nil t)))

  ;; Assert
  (assert-equal 1 (number-of-completion-requests))
  (assert-equal '("previous candidate") result "async1")

  ;; The candidates are not cached so different results might be returned.
  ;; Unclear if this is breaking the intended capf interface. But it works
  ;; well with company-mode.
  (setq next-result (funcall tlc-collection-fun "" nil t))
  (assert (cl-member "last_variable" next-result :test 'string-equal) "async2")
  (assert-equal 2 (number-of-completion-requests))

  ;; However, once a real result has been obtained, it's cached
  (assert-equal next-result (funcall tlc-collection-fun "" nil t))
  (assert-equal 2 (number-of-completion-requests))
  )

(tlc-deftest capf-bounds-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "completion.cpp"))
  (assert-equal '(tlc-completion-at-point t) completion-at-point-functions)

  (re-search-forward "last_variable")
  (assert-equal 539 (point) "arrange")
  (backward-char 5)
  (assert-equal 534 (point) "arrange")

  ;; Act
  (setq return (tlc-completion-at-point))

  ;; Assert
  (assert-equal 526 (nth 0 return))
  (assert-equal 539 (nth 1 return))
  )

(tlc-deftest file-name-to-uri-test ()
  (assert-equal "file://hello" (tlc--file-name-to-uri "hello"))
  (assert-equal "file:///usr/c++/hello" (tlc--file-name-to-uri "/usr/c++/hello"))
  (assert-equal "file://abc%C3%A5%C3%A4%C3%B6%E3%81%82%E6%97%A5"
                (tlc--file-name-to-uri "abcåäöあ日")))

(tlc-deftest uri-to-file-name-test ()
  (assert-equal "hello" (tlc--uri-to-file-name "file://hello"))
  (assert-equal "/usr/include/c++/15/iostream"
                (tlc--uri-to-file-name "file:///usr/include/c%2B%2B/15/iostream"))
  (assert-equal "abcåäöあ日"
                (tlc--uri-to-file-name "file://abc%C3%A5%C3%A4%C3%B6%E3%81%82%E6%97%A5")))

(tlc-deftest special-char-file-name-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "other.cpp"))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (re-search-forward "special")
  (re-search-forward "special")
  (assert-equal 8 (line-number-at-pos))
  (assert-equal 11 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; Assert
  (assert-equal "special++åäöあ本.cpp" (buffer-name))
  (assert-equal 2 (line-number-at-pos))
  (assert-equal 4 (current-column))

  (assert-equal 2 (number-of-did-open) "end")
  (assert-equal 0 (number-of-did-close)))

(tlc-deftest unicode-chars-content-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "other.cpp"))

  (re-search-forward "unicode" nil nil 3)
  (re-search-forward "は")
  (backward-char)
  (assert-equal 16 (line-number-at-pos))
  (assert-equal 26 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; ;; Assert
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 5 (current-column))
  )

(tlc-deftest xref-when-tlc-mode-disabled-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (assert-equal '(tlc-xref-backend t) xref-backend-functions)
  (tlc-mode -1)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act
  (should-error
   (non-interactive-xref-find-definitions)
   :type 'cl-no-applicable-method)

  ;; Assert
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))
  )

(tlc-deftest capf-when-tlc-mode-disabled-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert (tlc-completion-at-point) "enabled")

  (tlc-mode -1)

  (assert-not (tlc-completion-at-point) "disabled" )
  )

(tlc-deftest ask-start-server-yes-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (tlc--stop-server)
  (assert-equal 0 (length (tlc-info)))
  (assert tlc-mode)

  ;; Act
  (setq error-message
        (condition-case err
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
              (non-interactive-xref-find-definitions))
          (error (error-message-string err))))
  (assert-equal "No definitions found for: other_function" error-message)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  ;; Assert
  ;; tlc-mode still enabled
  (assert tlc-mode)
  ;; Server running
  (assert-equal 1 (length (tlc-info)))

  ;; xref works
  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  )

(tlc-deftest ask-start-server-no-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  (goto-char (point-min))
  (re-search-forward "other_function" nil nil 2)
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (assert-equal 1 (length (tlc-info)))
  (tlc--stop-server)
  (assert-equal 0 (length (tlc-info)))
  (assert tlc-mode)

  ;; Act
  (setq error-message
        (condition-case err
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
              (non-interactive-xref-find-definitions))
          (error (error-message-string err))))
  (assert-equal "You chose not the restart the LSP server. Disabling tlc-mode."
                error-message)

  ;; Assert
  ;; tlc-mode disabled
  (assert-not tlc-mode)
  ;; Server not running
  (assert-equal 0 (length (tlc-info)))
  )

(tlc-deftest multi-server-open-file-test ()
  ;; Open clangd file
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-equal 1 (length (tlc-info)))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal "clangd" (tlc--server-cmd))
  (setq root (tlc--root))
  (assert (string-suffix-p "test/clangd/" root))

  ;; Open erlang_ls file in same project
  (find-file (relative-repo-root "test" "clangd" "erlang_in_cpp.erl"))
  (assert-equal 2 (length (tlc-info)))
  (assert-equal 2 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal "erlang_ls" (tlc--server-cmd))
  (assert-equal root (tlc--root))

  ;; Open erlang_ls file in other project
  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))
  (assert-equal 3 (length (tlc-info)))
  (assert-equal 3 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal "erlang_ls" (tlc--server-cmd))
  (setq root2 (tlc--root))
  (assert-not (string= root root2))

  (setq infos (tlc-info))
  (assert-equal root (nth 0 (nth 0 infos)))
  (assert-equal "clangd" (nth 1 (nth 0 infos)))

  (assert-equal root (nth 0 (nth 1 infos)))
  (assert-equal "erlang_ls" (nth 1 (nth 1 infos)))

  (assert-equal root2 (nth 0 (nth 2 infos)))
  (assert-equal "erlang_ls" (nth 1 (nth 2 infos)))
  )

(tlc-deftest multi-server-xref-and-revert-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (find-file (relative-repo-root "test" "clangd" "erlang_in_cpp.erl"))
  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))
  (assert-equal 3 (length (tlc-info)) "infos")

  ;; Revert and then later check that the server still works
  (assert-equal 3 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (revert-buffer-quick)
  (assert-equal 4 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  (re-search-forward "my_function")
  (assert-equal 3 (line-number-at-pos))
  (assert-equal 20 (current-column))

  (non-interactive-xref-find-definitions)

  (assert-equal 11 (line-number-at-pos))
  (assert-equal 0 (current-column))
  (assert-equal "my_module.erl" (buffer-name))

  (switch-to-buffer "main.cpp")

  (re-search-forward "other_function")
  (re-search-forward "other_function")
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)

  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  (assert-equal "main.cpp" (buffer-name))

  (switch-to-buffer "erlang_in_cpp.erl")
  (re-search-forward "my_function")
  (assert-equal 3 (line-number-at-pos))
  (assert-equal 20 (current-column))

  (non-interactive-xref-find-definitions)

  (assert-equal 11 (line-number-at-pos))
  (assert-equal 0 (current-column))
  (assert-equal "erlang_in_cpp.erl" (buffer-name))
  )

(tlc-deftest multi-server-stop-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (find-file (relative-repo-root "test" "clangd" "erlang_in_cpp.erl"))
  (setq root (tlc--root))

  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))
  (setq root2 (tlc--root))

  (assert-equal 3 (length (tlc-info)) "infos")

  (assert-equal (list "/some/path" "some-cmd")
                (tlc--completion-to-server-key "/some/path @ some-cmd"))

  (cl-letf (((symbol-function 'completing-read)
             (lambda (_ collection _ _ _ _ default &rest _)
               (assert-equal (format "%s @ clangd" root) (nth 0 collection) "clangd1")
               (assert-equal (format "%s @ erlang_ls" root) (nth 1 collection) "erlang_ls1")
               (assert-equal (format "%s @ erlang_ls" root2) (nth 2 collection) "erlang_ls2")
               (assert-equal (format "%s @ erlang_ls" root2) default "default")
               (nth 0 collection))))
    (tlc-stop-server))

  (assert-equal 2 (length (tlc-info)) "infos")
  (assert-equal root (nth 0 (nth 0 (tlc-info))))
  (assert-equal "erlang_ls" (nth 1 (nth 0 (tlc-info))))

  (assert-equal root2 (nth 0 (nth 1 (tlc-info))))
  (assert-equal "erlang_ls" (nth 1 (nth 1 (tlc-info))))
  )

(tlc-deftest multiple-files-same-project-test ()
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 0 (length (tlc-info)))

  (find-file (relative-repo-root "test" "clangd" "other.cpp"))
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (setq info (tlc-info))
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 2 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal info (tlc-info))
  (assert-equal 1 (length info) "infos")

  (re-search-forward "other_function")
  (re-search-forward "other_function")
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 18 (current-column))

  (non-interactive-xref-find-definitions)

  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))

  ;; After killing a buffer of the same project, info is unchanged
  ;; and xref still works
  (kill-buffer)
  (assert-equal info (tlc-info))
  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))

  (re-search-forward "unicode" nil nil 3)
  (assert-equal 16 (line-number-at-pos))
  (assert-equal 11 (current-column))

  (non-interactive-xref-find-definitions)

  (assert-equal 11 (line-number-at-pos))
  (assert-equal 5 (current-column))
  )

(tlc-deftest cant-use-tlc-mode-test ()
  (find-file (relative-repo-root "test" "clangd" "CMakeLists.txt"))
  (setq msg nil)
  (cl-letf (((symbol-function 'message) (lambda (m) (setq msg m))))
    (tlc-mode))
  (assert-not tlc-mode)
  (assert-equal
   "tiny-lsp-client can only be used in buffers where a server-cmd can be found."
   msg)
  (assert-not (tlc-info))

  (switch-to-buffer (generate-new-buffer "hej.cpp"))
  (c++-mode)
  (cl-letf (((symbol-function 'message) (lambda (m &rest _) (setq msg m))))
    (tlc-mode))
  (assert-not tlc-mode)
  (assert-equal major-mode 'c++-mode)
  (assert-equal
   "tiny-lsp-client can only be used in file buffers."
   msg)

  (cl-letf (((symbol-function 'message) (lambda (m &rest _) (setq msg m)))
            (tlc-find-root-function (lambda () nil)))
    (find-file (relative-repo-root "test" "clangd" "main.cpp"))
    (tlc-mode))

  (assert-not tlc-mode)
  (assert-equal major-mode 'c++-mode)
  (assert-equal
   "tiny-lsp-client can only be used in buffers where root can be found."
   msg)
  )

(tlc-deftest tlc-change-not-nil-in-before-change-hook-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))
  (assert-equal 0 (number-of-did-close))

  (assert-not tlc--change)
  (tlc--before-change-hook 5 10)
  (assert tlc--change)
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))

  ;; Act
  (tlc--before-change-hook 6 7)

  ;; Assert
  (assert-not tlc--change)
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 1 (number-of-did-change-full))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest tlc-change-nil-in-after-change-hook-test ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))
  (assert-equal 0 (number-of-did-close))

  (assert-not tlc--change)

  ;; Act
  (tlc--after-change-hook 6 7 1)

  ;; Assert
  (assert-not tlc--change)
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 1 (number-of-did-change-full))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest tlc-full-change-to-something-else ()
  ;; Arrange
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  ;; Handle didChange manually in the test case
  (remove-hook 'before-change-functions 'tlc--before-change-hook t)
  (remove-hook 'after-change-functions 'tlc--after-change-hook t)

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))

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

  (setq new-content
        "
#include <iostream>

short hi(int a, int b, int c) {
    return a + b + c;
}

int main() {
    hi(456, 8, 9);
}
")

  (delete-region (point-min) (point-max))
  (assert-equal "" (current-buffer-string))
  (insert new-content)
  (assert-equal new-content (current-buffer-string))

  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))

  ;; Act
  ;; Manually change document to something completely different to see that
  ;; this full replace style works at all.
  (tlc--notify-text-document-did-change new-content)

  ;; Assert
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 1 (number-of-did-change-full))

  (beginning-of-buffer)
  (re-search-forward "return a")
  (forward-char 3)
  
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 15 (current-column))
  (non-interactive-xref-find-definitions)
  (assert-equal 4 (line-number-at-pos))
  (assert-equal 20 (current-column))

  (re-search-forward "hi")
  (assert-equal 9 (line-number-at-pos))
  (assert-equal 6 (current-column))
  (non-interactive-xref-find-definitions)
  (assert-equal 4 (line-number-at-pos))
  (assert-equal 6 (current-column))
  )

(tlc-deftest full-did-change-triggered-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))

  (setq initial-content
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
")
  (assert-equal initial-content (current-buffer-string))

  (re-search-forward "short ")
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  (setq start (point))

  ;; Note here how downcase-word causes tlc--change to change from nil
  ;; to non-nil, but buffer is not changed, and no didChange is sent
  (assert-not tlc--change)
  (call-interactively 'downcase-word)
  (assert tlc--change)
  (assert-equal initial-content (current-buffer-string))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 0 (number-of-did-change-full))

  ;; If called again, full didChange is done
  (goto-char start)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  (call-interactively 'downcase-word)
  (assert-not tlc--change)
  (assert-equal initial-content (current-buffer-string))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 1 (number-of-did-change-full))

  ;; Trigger inconsistent state again
  (goto-char start)
  (assert-not tlc--change)
  (call-interactively 'downcase-word)
  (assert tlc--change)
  (assert-equal initial-content (current-buffer-string))
  (assert-equal 0 (number-of-did-change-incremental))
  (assert-equal 1 (number-of-did-change-full))

  ;; Inconsistent state, now a well-behaved change is done
  (insert "x")
  (assert-not tlc--change)
  (assert-equal 0 (number-of-did-change-incremental))
  ;; 3, one because of tlc--change not nil in before, then set to nil, and
  ;; then nil in after
  (assert-equal 3 (number-of-did-change-full) "hej")

  (assert-equal
   "
#include <iostream>
#include \"other.hpp\"

short otherx_function(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {
    other_function(123);

    function_in_other_file();
}
"
   (current-buffer-string))

  (re-search-forward "other")
  (insert "x")
  (assert-equal
   "
#include <iostream>
#include \"other.hpp\"

short otherx_function(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {
    otherx_function(123);

    function_in_other_file();
}
"
   (current-buffer-string))

  ;; But for the second well-behaved incremental is used
  (assert-not tlc--change)
  (assert-equal 1 (number-of-did-change-incremental))
  (assert-equal 3 (number-of-did-change-full) "full")

  ;; In sync after the well-behaved change
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 10 (current-column))
  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 6 (current-column))
  )
