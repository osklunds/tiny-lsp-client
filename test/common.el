
(require 'cl-lib)

(defun assert-equal (exp act &optional msg)
  (unless (equal exp act)
    (when msg
      (std-message msg))
    (std-message "Exp %s" exp)
    (std-message "Act %s" act)
    (cl-assert (equal exp act) 'show)))

(defun std-message (format-string &rest args)
  (print (format (concat "[emacs]  " format-string) args) 'external-debugging-output))

(defun kill-server ()
  (interactive)
  (pcase-let ((`((,r ,c ,i)) (tlc--rust-all-server-info)))
    (shell-command (format "kill %s" i))))
