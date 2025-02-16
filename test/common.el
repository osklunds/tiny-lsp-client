
(require 'cl-lib)

(setq log-file-name (file-truename
                     (file-name-concat
                      user-emacs-directory
                      "tiny-lsp-client-test.log")))

(defun assert-equal (exp act &optional msg)
  (unless (equal exp act)
    (when msg
      (std-message msg))
    (std-message "Expected '%S'" exp)
    (std-message "Actual '%S'" act)
      ;; Sleep to have time for uncluttered printout
    (sleep-for 0.5)
    (cl-assert (equal exp act) 'show)))

(defun std-message (format-string &rest format-args)
  (let* ((new-format-string (concat "[emacs]  " format-string))
         (new-format-args (cons new-format-string format-args)))
    (print (apply 'format new-format-args)
           'external-debugging-output)))

(defun non-interactive-xref-find-definitions ()
  (let ((xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

;; Since this should always be 0, it's hard to know if it's working
;; properly
(defun number-of-STDERR ()
  (count-in-log-file "STDERR"))

(defun number-of-did-open ()
  (count-in-log-file "\"method\": \"textDocument/didOpen\","))

(defun number-of-did-close ()
  (count-in-log-file "\"method\": \"textDocument/didClose\","))

(defun count-in-log-file (pattern)
  (string-to-number (shell-command-to-string
   (format "cat %s | grep '%s' | wc -l" log-file-name pattern))))

(defun current-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))
