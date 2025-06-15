;; -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Oskar Lundström

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


;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." ".git"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "jdtls-test")

;; Manually require tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(require 'cc-mode)

(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'java-mode-hook #'tlc-mode)

(defun find-src-file-jdtls (file-name)
  (find-file (relative-repo-root "test" "jdtls" "my-app" "src" "main" "java"
                                 "com" "mycompany" "app" file-name)))

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open-a-file-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-not tlc-mode)

  ;; Act
  (find-src-file-jdtls "App.java")

  ;; Assert
  (assert-equal 'java-mode major-mode "java-mode")
  (assert tlc-mode "tlc-mode")

  (assert-equal 1 (number-of-did-open) "open after")
  (assert-equal 0 (number-of-did-close) "close after")
  )

(tlc-deftest find-definition-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-src-file-jdtls "App.java")

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (re-search-forward "other" nil nil 2)
  (assert-equal 12 (line-number-at-pos))
  (assert-equal 13 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; Assert
  (assert-equal 7 (line-number-at-pos))
  (assert-equal 23 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )
