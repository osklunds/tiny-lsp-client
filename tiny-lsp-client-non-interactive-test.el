
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

(defun non-interactive-xref-find-definitions ()
  (let ((xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

(sleep-for 10)

(non-interactive-xref-find-definitions)

(assert-equal 8 (line-number-at-pos))
(assert-equal 3 (current-column))

