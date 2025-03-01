
;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(setq debug-on-error t)

(defun assert-equal (exp act &optional label)
  (when (not (equal exp act))
    (message "")
    (message "")
    (message "")
    (message "-----------------------------------------------------------------------------")
    (message "Assert failed. label: '%s'" label)
    (message "Exp: '%s'" exp)
    (message "Act: '%s'" act)
    (message "-----------------------------------------------------------------------------")
    (message "")
    (message "")
    (message "")
    (sleep-for 1)
    )
  (should (equal exp act)))

(defun assert-not (act &optional label)
  (assert-equal nil act label))

(defun assert (act &optional label)
  (assert-equal t (not (not act)) label))

(defun run-shell-command (command &rest components)
  (message "-----------------------------------------------------------------------------")
  (message "Running command '%s'" command)
  (let* ((default-directory (apply 'relative-repo-root components))
         (code (with-temp-buffer
                 (let ((code (call-process-shell-command command nil t)))
                   (message (string-replace "%" "%%" (buffer-string)))
                   code)))
         (label (format "Command %s" command)))
    (assert-equal 0 code label))
  (message "Finished command '%s'" command)
  (message "-----------------------------------------------------------------------------")
  )

(defun non-interactive-xref-find-definitions ()
  (let ((xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

(defun get-tlc-collection-fun ()
  (pcase (tlc-completion-at-point)
    (`(,start ,end ,collection . ,props)
     ;; todo: add tests that check bounds
     collection
     )
    (_ (error "bad match"))
    ))

;; Since this should always be 0, it's hard to know if it's working
;; properly
(defun number-of-STDERR ()
  (count-in-log-file "STDERR"))

(defun number-of-did-open ()
  (count-in-log-file "\"method\": \"textDocument/didOpen\","))

(defun number-of-did-close ()
  (count-in-log-file "\"method\": \"textDocument/didClose\","))

(defun number-of-completion-requests ()
  (count-in-log-file "\"method\": \"textDocument/completion\","))

(defun count-in-log-file (pattern)
  (string-to-number (shell-command-to-string
                     (format "cat %s | grep '%s' | wc -l" log-file-name pattern))))

(defun current-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

;; -----------------------------------------------------------------------------
;; Test case framework
;; -----------------------------------------------------------------------------

(defvar log-file-name 'not-set
  "Set before running a test case, because each test case has its own log file
  name.")

(defvar test-file-name 'not-set
  "Name of the .el file with test cases. Needs to be set when loading
this common file. Is used to differentiate log file names.")

(defun before-each-test (test-case-name)
  (assert-equal t (stringp test-file-name) "test-file-name")
  (setq log-file-name (relative-repo-root
                       "test"
                       "logs"
                       (format "%s-%s.log" test-file-name test-case-name)))
  (delete-file log-file-name)
  (customize-set-variable 'tlc-log-file log-file-name))

(defun after-each-test ()
  ;; One drawback of running in the same emacs instance with ERT is that
  ;; this clean up in the end is needed.

  ;; Only kill buffers visiting a file
  (let ((buffers (cl-remove-if-not 'buffer-file-name (buffer-list))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        ;; First disable tlc-mode so that kill-buffer doesn't trigger
        ;; didClose
        (tlc-mode -1)
        ;; Stop server in a buffer where root path is known
        (tlc-stop-server))
      ;; Then kill the buffer too so that didOpen is sent if the same file
      ;; is used in many tests
      (kill-buffer buffer))
    (customize-set-variable 'tlc-before-start-server-hook nil)
    (customize-set-variable 'tlc-after-start-server-hook nil)
    ))

(cl-defmacro tlc-deftest (name () &rest body)
  (declare (indent defun))
  `(progn
     (ert-deftest ,name ()
       (before-each-test ',name)
       ,@body
       (after-each-test)
       )))
