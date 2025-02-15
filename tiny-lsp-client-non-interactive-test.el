;; -----------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun std-message (format-string &rest args)
  (print (format (concat "[emacs]  " format-string) args) 'external-debugging-output))

(defun assert-equal (exp act)
  (unless (equal exp act)
    (std-message "Exp %s" exp)
    (std-message "Act %s" act)
    (cl-assert (equal exp act) 'show)))

(defun non-interactive-xref-find-definitions ()
  (let ((xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

(setq log-file-name (file-truename
                     (file-name-concat
                      user-emacs-directory
                      "tiny-lsp-client-test.log")))

(defun number-of-did-open ()
  (count-in-log-file "\"method\": \"textDocument/didOpen\","))

(defun number-of-did-close ()
  (count-in-log-file "\"method\": \"textDocument/didClose\","))

(defun count-in-log-file (pattern)
  (string-to-number (shell-command-to-string
   (format "cat %s | grep '%s' | wc -l" log-file-name pattern))))

;; -----------------------------------------------------------------------------
;; Preparation
;;------------------------------------------------------------------------------

(define-derived-mode rust-mode prog-mode "Rust"
  "Fake rust-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; -----------------------------------------------------------------------------
;; Loading tlc-mode
;;------------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio t)

(add-hook 'rust-mode-hook 'tlc-mode)

(delete-file log-file-name)

;; -----------------------------------------------------------------------------
;; Opening a file
;;------------------------------------------------------------------------------

(assert-equal 0 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(find-file "src/dummy.rs")

(assert-equal 'rust-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Xref find definition
;;------------------------------------------------------------------------------

(re-search-forward "second_function")
(assert-equal 5 (line-number-at-pos))
(assert-equal 19 (current-column))

(defun xref-find-definition-until-success ()
  (ignore-errors
    (non-interactive-xref-find-definitions))
  (if (equal 8 (line-number-at-pos))
      'ok
    (sleep-for 0.1)
    (xref-find-definition-until-success)))

(xref-find-definition-until-success)

(assert-equal 8 (line-number-at-pos))
(assert-equal 3 (current-column))

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Disable tlc-mode
;;------------------------------------------------------------------------------

(tlc-mode -1)
(assert-equal nil tlc-mode)
(assert-equal '(etags--xref-backend) xref-backend-functions)

(assert-equal 1 (number-of-did-open))
(assert-equal 1 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Enable tlc-mode again
;;------------------------------------------------------------------------------

(tlc-mode t)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 2 (number-of-did-open))
(assert-equal 1 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Toggle tlc-mode by changing major mode
;;------------------------------------------------------------------------------

(text-mode)
(assert-equal 'text-mode major-mode)
(assert-equal nil tlc-mode)
(assert-equal '(etags--xref-backend) xref-backend-functions)

(assert-equal 2 (number-of-did-open))
(assert-equal 2 (number-of-did-close))

(rust-mode)
(assert-equal 'rust-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 3 (number-of-did-open))
(assert-equal 2 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Revert buffer
;;------------------------------------------------------------------------------

;; Testing didOpen and didClose at revert. Note, I know no good way to
;; automatically test did. Need to inspect the log and see that no duplicate
;; error is printed.

(revert-buffer-quick)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 4 (number-of-did-open))
(assert-equal 3 (number-of-did-close)) ;; this is 4, which is a bug

(error "ok, early exit")

;; When preserve-modes is true
(revert-buffer nil t t)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------

(defun current-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(assert-equal 
 "
// Don't change this file. It is used in tests

fn first_function() {
    second_function();
}

fn second_function() {

}
"
 (current-buffer-string))

;; Re-position
(beginning-of-buffer)
(next-line)
(next-line)
(assert-equal 3 (line-number-at-pos))
(assert-equal 0 (current-column))

;; Some single-character edits
(insert "f")
(insert "n")
(insert " ")

;; Some multi-character edits
(insert "third_function")
(insert "() {")

;; newline
(insert "\n")

;; finishing the function
(insert "    first_function(); // call from third }")

(assert-equal 
 "
// Don't change this file. It is used in tests
fn third_function() {
    first_function(); // call from third }
fn first_function() {
    second_function();
}

fn second_function() {

}
"
 (current-buffer-string))

;; Re-position
(beginning-of-buffer)
(re-search-forward "    first_function")
(assert-equal 4 (line-number-at-pos))
(assert-equal 18 (current-column))

;; Find definition
(non-interactive-xref-find-definitions)
(assert-equal 5 (line-number-at-pos))
(assert-equal 3 (current-column))

;; Re-position
(beginning-of-buffer)
(re-search-forward "fn second_function")
(assert-equal 9 (line-number-at-pos))
(assert-equal 18 (current-column))

;; Edits
(previous-line)
(backward-delete-char 1)
(assert-equal 
 "
// Don't change this file. It is used in tests
fn third_function() {
    first_function(); // call from third }
fn first_function() {
    second_function();
}
fn second_function() {

}
"
 (current-buffer-string))

;; Re-position
(beginning-of-buffer)
(re-search-forward "second_function")

(assert-equal 6 (line-number-at-pos))
(assert-equal 19 (current-column))

;; Find definition
(non-interactive-xref-find-definitions)
(assert-equal 8 (line-number-at-pos))
(assert-equal 3 (current-column))

;; Re-position
(beginning-of-buffer)
(re-search-forward "second_function")
(assert-equal 6 (line-number-at-pos))
(assert-equal 19 (current-column))

;; Edits
(backward-delete-char 1)
(backward-delete-char 1)
(backward-delete-char 1)

;; Re-position
(beginning-of-buffer)
(re-search-forward "fn second_function")
(assert-equal 8 (line-number-at-pos))
(assert-equal 18 (current-column))

;; Edits
(backward-delete-char 1)
(backward-delete-char 1)
(backward-delete-char 1)
(previous-line)
(insert "\n")
(insert "\n")
(insert "\n")
(insert "\n")

(assert-equal 
 "
// Don't change this file. It is used in tests
fn third_function() {
    first_function(); // call from third }
fn first_function() {
    second_funct();
}




fn second_funct() {

}
"
 (current-buffer-string))

;; Re-position
(beginning-of-buffer)
(re-search-forward "second_funct")
(assert-equal 6 (line-number-at-pos))
(assert-equal 16 (current-column))

;; Find definition
(non-interactive-xref-find-definitions)
(assert-equal 12 (line-number-at-pos))
(assert-equal 3 (current-column))

;; -----------------------------------------------------------------------------
;; Kill buffer
;;------------------------------------------------------------------------------

(kill-buffer)
