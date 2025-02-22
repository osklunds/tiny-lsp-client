
;; -----------------------------------------------------------------------------
;; Load common test functions
;; -----------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(load "test/common.el")

;; -----------------------------------------------------------------------------
;; Preparation
;;------------------------------------------------------------------------------

(define-derived-mode rust-mode prog-mode "Rust"
  "Fake rust-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; -----------------------------------------------------------------------------
;; Loading tlc-mode
;;------------------------------------------------------------------------------
(test-case-message "loading tlc-mode")

;; Manually add tlc-rust to get debug version
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'rust-mode-hook 'tlc-mode)

(delete-file log-file-name)

;; -----------------------------------------------------------------------------
;; Opening a file
;;------------------------------------------------------------------------------
(test-case-message "opening a file")

(defvar num-before-hook-calls 0)
(defvar num-after-hook-calls 0)

(defvar hook-last-caller 'after)

(defun before-hook ()
  (assert-equal (tlc--root) default-directory)
  (cl-incf num-before-hook-calls)
  (assert-equal 'after hook-last-caller)
  (setq hook-last-caller 'before)
  (test-case-message "before-hook called"))

(defun after-hook ()
  (assert-equal (tlc--root) default-directory)
  (cl-incf num-after-hook-calls)
  (assert-equal 'before hook-last-caller)
  (setq hook-last-caller 'after)
  (test-case-message "after-hook called"))

(add-hook 'tlc-before-start-server-hook 'before-hook)
(add-hook 'tlc-after-start-server-hook 'after-hook)

(assert-equal 0 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(find-file "src/dummy.rs")

(assert-equal 'rust-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(assert-equal 1 num-before-hook-calls)
(assert-equal 1 num-after-hook-calls)

;; -----------------------------------------------------------------------------
;; Xref find definition
;;------------------------------------------------------------------------------
(test-case-message "xref find definition")

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
(test-case-message "Disable tlc-mode")

(tlc-mode -1)
(assert-equal nil tlc-mode)
(assert-equal '(etags--xref-backend) xref-backend-functions)

(assert-equal 1 (number-of-did-open))
(assert-equal 1 (number-of-did-close))

(test-case-message "disable tlc")

;; -----------------------------------------------------------------------------
;; Enable tlc-mode again
;;------------------------------------------------------------------------------
(test-case-message "Enable tlc-mode again")

(tlc-mode t)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 2 (number-of-did-open))
(assert-equal 1 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Toggle tlc-mode by changing major mode
;;------------------------------------------------------------------------------
(test-case-message "Toggle tlc-mode by changing major mode")

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

(assert-equal 3 (number-of-did-open) "after enable rust-mode")
(assert-equal 2 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Revert buffer
;;------------------------------------------------------------------------------
(test-case-message "revert buffer")

;; Testing didOpen and didClose at revert. Note, I know no good way to
;; automatically test did. Need to inspect the log and see that no duplicate
;; error is printed.

(revert-buffer-quick)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 4 (number-of-did-open))
(assert-equal 3 (number-of-did-close) "after revert-buffer-quick") 

;; When preserve-modes is true
(revert-buffer nil 'no-confirm 'preserve-modes)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 5 (number-of-did-open))
(assert-equal 4 (number-of-did-close) "after revert with preserve-modes") 

;; -----------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------
(test-case-message "Editing")

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

(assert-equal 5 (number-of-did-open))
(assert-equal 4 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Edit with restriction
;; -----------------------------------------------------------------------------
(test-case-message "Edit with restriction")

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

(beginning-of-buffer)
(re-search-forward "first_function")

(let* ((p (point))
       (source-buffer (current-buffer)))
  (with-temp-buffer
    (insert "abc")
    (let ((temp-buffer (current-buffer)))
      (with-current-buffer source-buffer
        (narrow-to-region (- p 14) p)
        (replace-buffer-contents temp-buffer)))))

(widen)

(assert-equal 
 "
// Don't change this file. It is used in tests
fn third_function() {
    abc(); // call from third }
fn first_function() {
    second_funct();
}




fn second_funct() {

}
"
 (current-buffer-string))

(beginning-of-buffer)
(re-search-forward "first_function")
(replace-string "first_function" "abc" nil (point-min) (point))

(previous-line)
(end-of-line)
(insert "\n\n")

(assert-equal 
 "
// Don't change this file. It is used in tests
fn third_function() {
    abc(); // call from third }


fn abc() {
    second_funct();
}




fn second_funct() {

}
"
 (current-buffer-string))

;; Re-position
(beginning-of-buffer)
(re-search-forward "abc")
(assert-equal 4 (line-number-at-pos))
(assert-equal 7 (current-column) "before xref def")

;; Find definition
(non-interactive-xref-find-definitions)
(assert-equal 7 (line-number-at-pos) "after xref def")
(assert-equal 3 (current-column))


;; -----------------------------------------------------------------------------
;; Kill buffer
;;------------------------------------------------------------------------------
(test-case-message "Kill buffer")

(kill-buffer)

(assert-equal 5 (number-of-did-open))
(assert-equal 5 (number-of-did-close))

(find-file "src/dummy.rs")

(assert-equal 6 (number-of-did-open))
(assert-equal 5 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Open non-existing file
;; -----------------------------------------------------------------------------
(test-case-message "Open non-existing file")

(setq non-existing-file "doesnt_exist_right.rs")
(assert-equal nil (file-exists-p non-existing-file))

(find-file non-existing-file)

;; tiny-lsp-client doesn't save anymore
(assert-equal nil (file-exists-p non-existing-file))

(assert-equal 7 (number-of-did-open))
(assert-equal 5 (number-of-did-close))

(kill-buffer)

(assert-equal 7 (number-of-did-open))
(assert-equal 6 (number-of-did-close))

(assert-equal "dummy.rs" (buffer-name))

(delete-file non-existing-file)

;; -----------------------------------------------------------------------------
;; Info and stop
;; -----------------------------------------------------------------------------
(test-case-message "Info and stop")

(setq root-path (tlc--root))
(assert-equal t (stringp root-path))

(defun assert-tlc-info ()
  (pcase (tlc-info)
    (`((,r ,command ,process-id))
     (assert-equal root-path r)
     (assert-equal "rust-analyzer" command)
     (assert-equal t (integerp process-id))
     process-id
     )
    (x
     (error "unexpected return: %s" x))))

(setq old-pid (assert-tlc-info))

(tlc-stop-server)

;; avoid race
(sleep-for 1)

(assert-equal nil (tlc-info))

(assert-equal 1 num-before-hook-calls)
(assert-equal 1 num-after-hook-calls)

;; To see that spamming doesn't cause issues
(tlc-stop-server)
(tlc-stop-server)
(tlc-stop-server)

(assert-equal 7 (number-of-did-open) "didOpen before restart")
(assert-equal 6 (number-of-did-close))

(tlc-restart-server)
;; avoid race
(sleep-for 1)

(assert-equal 8 (number-of-did-open) "didOpen after restart")
(assert-equal 6 (number-of-did-close))

(assert-equal 2 num-before-hook-calls)
(assert-equal 2 num-after-hook-calls)

(setq new-pid (assert-tlc-info))

(test-case-message "New: %s Old: %s" new-pid old-pid)
(assert-equal nil (equal new-pid old-pid))

(test-case-message "After stop restart info")

(assert-equal 0 (number-of-STDERR))
