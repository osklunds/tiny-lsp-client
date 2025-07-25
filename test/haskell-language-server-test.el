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
(setq test-file-name "haskell-langauge-server-test")
(common-setup)

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(define-derived-mode haskell-mode prog-mode "Haskell"
  "Fake haskell-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(add-hook 'haskell-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open-a-file-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (find-file (relative-repo-root "test" "haskell_language_server" "app" "Main.hs"))

  ;; Assert
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'haskell-mode major-mode)
  (assert tlc-mode)
  )

(tlc-deftest find-definition-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "haskell_language_server" "app" "Main.hs"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (re-search-forward "myFunction")
  (assert-equal 4 (line-number-at-pos))
  (assert-equal 25 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; Assert
  (assert-equal 6 (line-number-at-pos))
  (assert-equal 0 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

(tlc-deftest edit-test ()
  (find-file (relative-repo-root "test" "haskell_language_server" "app" "Main.hs"))

  (assert-equal 
   "module Main where

main :: IO ()
main = print $ myFunction 1 2

myFunction arg1 arg2 = arg1 + arg2

other = my

"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "myFunction")
  (insert "_")
  (insert "h")
  (insert "e")
  (insert "j")
  (previous-line)
  (end-of-line)
  (insert "\n")

  (re-search-forward "myFunction" nil nil 2)
  (insert "_hej")
  (end-of-line)
  (previous-line)
  (insert "\n")
  (insert "\n")

  (assert-equal 
   "module Main where

main :: IO ()

main = print $ myFunction_hej 1 2



myFunction_hej arg1 arg2 = arg1 + arg2

other = my

"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "myFunction")
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 25 (current-column))

  ;; Sometimes no definition is found
  (run-until 100 0.1 (non-interactive-xref-find-definitions))

  (assert-equal 9 (line-number-at-pos))
  (assert-equal 0 (current-column))
  )

(tlc-deftest capf-test ()
  (find-file (relative-repo-root "test" "haskell_language_server" "app" "Main.hs"))

  (re-search-forward "my" nil nil 3)
  (setq tlc-collection-fun (get-tlc-collection-fun))

  (run-until 100 0.1
    (let ((result (funcall tlc-collection-fun "" nil t)))
      (dolist (exp '("myFunction" "myThreadId"))
        (assert (cl-member exp result :test 'string-equal) exp))
      ))
  )
