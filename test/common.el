
(require 'cl-lib)

(defun assert-equal (exp act &optional msg)
  (unless (equal exp act)
    (when msg
      (std-message msg))
    (std-message "Exp %s" exp)
    (std-message "Act %s" act)
    (cl-assert (equal exp act) 'show)))


(defun kill-server ()
  (interactive)
  (pcase-let ((`((,r ,c ,i)) (tlc--rust-all-server-info)))
    (shell-command (format "kill %s" i))))
(defun std-message (format-string &rest format-args)
  (let* ((new-format-string (concat "[emacs]  " format-string))
         (new-format-args (cons new-format-string format-args)))
    (print (apply 'format new-format-args)
           'external-debugging-output)))
