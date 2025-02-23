
;; -----------------------------------------------------------------------------
;; Load common test functions
;; -----------------------------------------------------------------------------

(add-to-list 'load-path default-directory)

(load "test/common.el")

;; -----------------------------------------------------------------------------
;; Preparation
;; -----------------------------------------------------------------------------

(let ((default-directory (file-name-concat default-directory "test" "clangd")))
  (assert-equal 0 (call-process-shell-command "cmake .") "cmake")
  (assert-equal 0 (call-process-shell-command "make") "make")
  )

;; Manually add tlc-rust to get debug version
(require 'tlc-rust "target/debug/libtiny_lsp_client.so")
(require 'tiny-lsp-client)

(customize-set-variable 'tlc-log-file log-file-name)
(customize-set-variable 'tlc-log-io t)
(customize-set-variable 'tlc-log-stderr t)
(customize-set-variable 'tlc-log-rust-debug t)
(customize-set-variable 'tlc-log-emacs-debug t)
(customize-set-variable 'tlc-log-to-stdio nil)

(add-hook 'c++-mode-hook 'tlc-mode)

(delete-file log-file-name)

;; -----------------------------------------------------------------------------
;; Opening a file
;;------------------------------------------------------------------------------

(assert-equal 0 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(find-file "test/clangd/main.cpp")

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

(assert-equal 'c++-mode major-mode)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

;; -----------------------------------------------------------------------------
;; Xref find definition
;;------------------------------------------------------------------------------

(re-search-forward "other_function")
(re-search-forward "other_function")
(assert-equal 11 (line-number-at-pos))
(assert-equal 18 (current-column))

(non-interactive-xref-find-definitions)

(assert-equal 5 (line-number-at-pos))
(assert-equal 6 (current-column))

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close))

;; -----------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; Revert buffer
;; -----------------------------------------------------------------------------

(assert-equal 1 (number-of-did-open))
(assert-equal 0 (number-of-did-close)) 

(revert-buffer nil 'no-confirm 'preserve-modes)
(assert-equal t tlc-mode)
(assert-equal '(tlc-xref-backend t) xref-backend-functions)

(assert-equal 2 (number-of-did-open))
(assert-equal 1 (number-of-did-close)) 

(assert-equal 0 (number-of-STDERR))
