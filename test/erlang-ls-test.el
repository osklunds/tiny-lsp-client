
;; -----------------------------------------------------------------------------
;; Load common test functions
;; -----------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(load "test/common.el")

;; -----------------------------------------------------------------------------
;; Preparation
;; -----------------------------------------------------------------------------

(define-derived-mode erlang-mode prog-mode "Erlang"
  "Fake erlang-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio t)

(add-hook 'erlang-mode-hook 'tlc-mode)

(delete-file log-file-name)

(assert-equal 0 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(find-file "test/erlang_ls/my_module.erl")

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(assert-equal 'erlang-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Xref find definition
;;------------------------------------------------------------------------------

(re-search-forward "my_function")
(assert-equal 3 (line-number-at-pos))
(assert-equal 20 (current-column))

(defun xref-find-definition-until-success ()
  (ignore-errors
    (non-interactive-xref-find-definitions))
  (if (equal 8 (line-number-at-pos))
      'ok
    (sleep-for 0.1)
    (xref-find-definition-until-success)))

(xref-find-definition-until-success)

(assert-equal 10 (line-number-at-pos))
(assert-equal 0 (current-column))

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))
