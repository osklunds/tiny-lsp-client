
;; Run with "emacs -batch -l ert -l new-test.el -f ert-run-tests-batch-and-exit"

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

;; -----------------------------------------------------------------------------
;; Common helpers
;; -----------------------------------------------------------------------------

(defun my-assert-equal (exp act &optional label)
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
    (message ""))
  (should (equal exp act)))

(defun run-shell-command (command &rest components)
  (message "-----------------------------------------------------------------------------")
  (message "Running command '%s'" command)
  (let* ((default-directory (apply 'relative-repo-root components))
         (code (with-temp-buffer
                 (let ((code (call-process-shell-command command nil t)))
                   (message (string-replace "%" "%%" (buffer-string)))
                   code)))
         (label (format "Command %s" command)))
    (my-assert-equal 0 code label))
  (message "Finished command '%s'" command)
  (message "-----------------------------------------------------------------------------")
  )

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

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cargo build")
(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

;; Manually require tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'c++-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Test case framework
;; -----------------------------------------------------------------------------

(defvar log-file-name 'not-set
  "Set before running a test case, because each test case has its own log file
  name.")

(defun before-each-test (test-case-name)
  ;; todo would be better to not hard code test file name
  (setq log-file-name (relative-repo-root
                       "test"
                       "logs"
                       (format "%s-%s.log" "new-test" test-case-name)))
  (delete-file log-file-name)
  (customize-set-variable 'tlc-log-file log-file-name))

(defun after-each-test ()
  (tlc-stop-server)
  )

(cl-defmacro tlc-deftest (name () &rest body)
  (declare (indent defun))
  `(progn
     (ert-deftest ,name ()
       (before-each-test ',name)
       ,@body
       (after-each-test)
       )))

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open ()
  (my-assert-equal 0 (number-of-did-open))
  (my-assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "main.cpp"))

  (my-assert-equal 1 (number-of-did-open))
  (my-assert-equal 0 (number-of-did-close) "after")
  )

(tlc-deftest other-test ()
  (my-assert-equal 0 (number-of-did-open))
  (my-assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "clangd" "other.cpp"))
  )
