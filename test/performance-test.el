;;; tiny-lsp-client.el --- Tiny LSP Client  -*- lexical-binding: t; -*-

;; This file is part of tiny-lsp-client.

;; tiny-lsp-client is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; tiny-lsp-client is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; tiny-lsp-client. If not, see <https:;;www.gnu.org/licenses/>.

;;; Commentary:

;; Test cases can only be run one at a time since eglot isn't cleaned up
;; Also, they take a long time to run, so excluded from run_tests.sh

;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "performance-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(run-shell-command "cargo build --release")
(run-shell-command "cmake ." "test" "clangd")
(run-shell-command "make" "test" "clangd")

(require 'tlc-rust (release-rust-module))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))
(require 'eglot)

;; Make it low so that gc is triggered more often
(setq gc-cons-threshold 8000)

(customize-set-variable 'tlc-log-stderr nil)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun native-comp-p (function)
  (native-comp-function-p (symbol-function function)))

(defun ensure-native-compiled ()
  (find-file (relative-repo-root "tiny-lsp-client.el"))
  (emacs-lisp-native-compile-and-load)

  ;; To see that native-comp-p works
  (assert-not (native-comp-p 'native-comp-p) "native-comp-p")

  ;; Checking that eglot and some other built-in are indeed native compiled
  ;; todo: not a perfect check. For some reason, e.g. describe-function
  ;; is not considered native compiled.
  (assert (native-comp-p 'eglot--connect) "eglot--connect")
  (assert (native-comp-p 'jsonrpc-async-request))
  (assert (native-comp-p 'jsonrpc--event))
  (assert (native-comp-p 'url-unhex))

  ;; Check that tlc is native compiled
  (assert (native-comp-p 'tlc--sync-request) "tlc--sync-request")
  )

(ensure-native-compiled)

(defun dummy-xref-backend () 'xref-dummy)

;; Same as the one for (eql xref-tlc)
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-dummy)))
  (when tlc-mode
    (propertize (or (thing-at-point 'symbol) "")
                'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-dummy)) _identifier)
  nil)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

;; Number of garbage collections: 2
;; Reference test to see how much garbage just the loop causes.
(tlc-deftest xref-no-lsp-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (add-hook 'xref-backend-functions 'dummy-xref-backend nil t)

  (assert-not tlc-mode)
  (assert-not eglot--managed-mode)

  (xref-test)
  )
;; Number of garbage collections: 8
;; tlc itself causes 8-2=6 garbage collections
(tlc-deftest xref-tlc-only-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (tlc-mode)
  (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)

  (assert tlc-mode)
  (assert-not eglot--managed-mode)

  (xref-test)
  )

;; Number of garbage collections: 17
;; eglot itself causes 17-2=15 garbage collections
;; eglot causes 15/6=2.5 times as many garbage collections compared to tlc
(tlc-deftest xref-eglot-only-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (call-interactively #'eglot)

  (assert-not tlc-mode)
  (assert eglot--managed-mode)

  (xref-test)
  )

;; This test is just to see that tlc and eglot return the same positions
;; in the xref-test loop.
(tlc-deftest xref-compare-test ()
  (let* ((tlc-result nil)
         (eglot-result nil))
    (find-file (relative-repo-root "test" "clangd" "main.cpp"))

    ;; First test with tlc
    (tlc-mode)
    (assert-not eglot--managed-mode)
    (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)
    (assert-equal xref-backend-functions '(tlc-xref-backend t))
    (setq tlc-result (xref-test))

    ;; Disable tlc
    (tlc-mode -1)
    (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
    (assert-not tlc-mode)

    ;; Then test with eglot
    (call-interactively #'eglot)
    (assert-equal xref-backend-functions '(eglot-xref-backend t))
    (assert eglot--managed-mode)
    (setq eglot-result (xref-test))

    ;; Check that tlc and eglot gave the same results
    (dotimes (i (length tlc-result))
      (let* ((tlc-entry (nth i tlc-result))
             (eglot-entry (nth i eglot-result)))
        (assert-equal tlc-entry eglot-entry)))
    ;; so that also length is checked
    (assert-equal tlc-result eglot-result)
    )
  )

(defun xref-test ()
  (garbage-collect)
  (let* ((result nil)
         (gcs-done-start gcs-done))
    (dotimes (_ 100)
      (dotimes (i (point-max))
        (goto-char i)
        (save-excursion
          (ignore-errors
            (non-interactive-xref-find-definitions))
          (push (list i (buffer-file-name) (point)) result))))
    ;; (dolist (elt result)
    ;;   (message "%s" elt))
    (message "Number of garbage collections: %s" (- gcs-done gcs-done-start))
    result))

;; Number of garbage collections: 88
;; Reference test to see how much garbage just the loop causes.
(tlc-deftest edit-no-lsp-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (assert-not tlc-mode)
  (assert-not eglot--managed-mode)

  (edit-test)
  )

;; Number of garbage collections: 95
;; tlc itself causes 95-88=7 garbage collections
(tlc-deftest edit-tlc-only-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (tlc-mode)
  (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)

  (assert tlc-mode)
  (assert-not eglot--managed-mode)

  (edit-test)
  )

;; Number of garbage collections: 202
;; eglot itself causes 202-88=114 garbage collections
;; eglot causes 114/7=16 times as many garbage collections compared to tlc
(tlc-deftest edit-eglot-only-test ()
  (find-file (relative-repo-root "test" "clangd" "main.cpp"))
  (call-interactively #'eglot)

  (assert-not tlc-mode)
  (assert eglot--managed-mode)

  (edit-test)
  )

(defun edit-test ()
  (garbage-collect)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (gcs-done-start gcs-done))
    (dotimes (_ 100)
      (dotimes (i (length content))
        (goto-char (point-max))
        (insert (aref content i))
        ;; It's a bit unclear when eglot sends changes. Also, eglot has a mighty
        ;; fine accumulation of changes, which I also aim to implement. For
        ;; these reasons, force eglot to signal changes towards the LSP after
        ;; every emacs change, like tlc does.
        (when eglot--managed-mode
          (eglot--signal-textDocument/didChange))
        ))
    (message "Number of garbage collections: %s" (- gcs-done gcs-done-start)))
  )
