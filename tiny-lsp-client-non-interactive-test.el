
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

(add-hook 'rust-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Opening a file
;;------------------------------------------------------------------------------

(find-file "src/dummy.rs")

(assert-equal 'rust-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

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

;; -----------------------------------------------------------------------------
;; Disable tlc-mode
;;------------------------------------------------------------------------------

;; todo: body isn't called when major-mode disabled
(tlc-mode -1)
(assert-equal nil tlc-mode)
(assert-equal '(etags--xref-backend) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Enable tlc-mode again
;;------------------------------------------------------------------------------

(tlc-mode t)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Revert buffer
;;------------------------------------------------------------------------------

;; Testing didOpen and didClose at revert. Note, I know no good way to
;; automatically test did. Need to inspect the log and see that no duplicate
;; error is printed.

(revert-buffer-quick)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

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

;; -----------------------------------------------------------------------------
;; Kill buffer
;;------------------------------------------------------------------------------

(kill-buffer)
