
;; Wrapper to be able to run "cargo test" together with the other lisp tests.

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "new-common.el"))

(ert-deftest cargo-test-test ()
  (run-shell-command "cargo test -- --show-output --test-threads 1"))
