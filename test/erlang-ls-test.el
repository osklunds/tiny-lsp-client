
;; -----------------------------------------------------------------------------
;; Loading common functions
;; -----------------------------------------------------------------------------

;; Moment 22: can't be in common.el because then common.el can't be found
(defun relative-repo-root (&rest components)
  (let* ((repo-root (file-truename (locate-dominating-file "." "Cargo.toml"))))
    (apply 'file-name-concat repo-root components)))

(load (relative-repo-root "test" "common.el"))
(setq test-file-name "erlang-ls-test")

;; -----------------------------------------------------------------------------
;; Setup before running test cases
;; -----------------------------------------------------------------------------

(define-derived-mode erlang-mode prog-mode "Erlang"
  "Fake erlang-mode for testing.")

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;; Manually add tlc-rust to get debug version
(require 'tlc-rust (relative-repo-root "target" "debug" "libtiny_lsp_client.so"))
(require 'tiny-lsp-client (relative-repo-root "tiny-lsp-client"))

(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'erlang-mode-hook 'tlc-mode)

;; -----------------------------------------------------------------------------
;; Test cases
;; -----------------------------------------------------------------------------

(tlc-deftest open-a-file-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  ;; Act
  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))

  ;; Assert
  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'erlang-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)
  )

(tlc-deftest find-definition-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (re-search-forward "my_function")
  (assert-equal 3 (line-number-at-pos))
  (assert-equal 20 (current-column))

  ;; Act
  (non-interactive-xref-find-definitions)

  ;; Assert
  (assert-equal 10 (line-number-at-pos))
  (assert-equal 0 (current-column))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))
  )

;; todo: need newlines here too
(tlc-deftest edit-test ()
  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))

  (assert-equal 
   "-module(my_module).

-export([my_function/1]).

other_function(Arg) ->
    io:format(\"~p~n\", [Arg]).



my_function(Arg) ->
    other_function({arg, Arg}).
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "other_function")
  (insert "_")
  (insert "h")
  (insert "e")
  (insert "j")

  (re-search-forward "other_function")
  (insert "_hej")

  (assert-equal 
   "-module(my_module).

-export([my_function/1]).

other_function_hej(Arg) ->
    io:format(\"~p~n\", [Arg]).



my_function(Arg) ->
    other_function_hej({arg, Arg}).
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "other")
  (re-search-forward "other")
  (assert-equal 11 (line-number-at-pos))
  (assert-equal 9 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 5 (line-number-at-pos))
  (assert-equal 0 (current-column))

  (beginning-of-buffer)
  (re-search-forward "my_fun")
  (backward-delete-char 1)
  (backward-delete-char 2)
  (re-search-forward "my_fun")
  (beginning-of-line)
  (replace-string "my_function" "    my_ction")

  (assert-equal 
   "-module(my_module).

-export([my_ction/1]).

other_function_hej(Arg) ->
    io:format(\"~p~n\", [Arg]).



    my_ction(Arg) ->
    other_function_hej({arg, Arg}).
"
   (current-buffer-string))

  (beginning-of-buffer)
  (re-search-forward "my_c")
  (assert-equal 3 (line-number-at-pos))
  (assert-equal 13 (current-column))

  (non-interactive-xref-find-definitions)
  (assert-equal 10 (line-number-at-pos))
  (assert-equal 4 (current-column))
  )

(tlc-deftest revert-buffer-test ()
  ;; Arrange
  (assert-equal 0 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (find-file (relative-repo-root "test" "erlang_ls" "my_module.erl"))

  (assert-equal 1 (number-of-did-open))
  (assert-equal 0 (number-of-did-close))

  (assert-equal 'erlang-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  ;; Act
  (revert-buffer nil 'no-confirm 'preserve-modes)

  ;; Assert
  (assert-equal 'erlang-mode major-mode)
  (assert-equal t tlc-mode)
  (assert-equal '(tlc-xref-backend t) xref-backend-functions)

  (assert-equal 2 (number-of-did-open))
  (assert-equal 1 (number-of-did-close))
  )
