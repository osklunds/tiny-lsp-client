;;; tiny-lsp-client.el --- Tiny LSP Client  -*- lexical-binding: t; -*-

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

;; @credits: This file as a whole was inspired a lot by
;; https://github.com/zbelial/lspce and eglot

(require 'tlc-rust "libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)
(require 'project)
(require 'url-util)

(eval-and-compile
  (cl-defmacro tlc--widen (&rest body)
    `(save-excursion (save-restriction (widen) ,@body))))

;; -----------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

(defun tlc--set-rust-based-option (symbol value)
  ;; Would be better to avoid this lisp variable since duplicates what's stored
  ;; in Rust, but customize shows state uninitialized without it.
  (set symbol value)
  (tlc--rust-set-option symbol value))

(defgroup tiny-lsp-client nil
  "tiny-lsp-client: a minor mode for LSP."
  :group 'tools)

(defcustom tlc-server-cmds '(
                             (rust-mode . "rust-analyzer")
                             (erlang-mode . "erlang_ls")
                             (c++-mode . "clangd")
                             )
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client
  :type 'sexp) ;; todo: better type

(defcustom tlc-find-root-function 'tlc-find-root-default-function
  "Function used for finding the root path of a project."
  :group 'tiny-lsp-client
  :type 'function)

(defcustom tlc-log-io nil
  "Whether JSON messages between tiny-lsp-client and the LSP server should be
logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-log-stderr t
  "Whether stderr output from the LSP server should be logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-log-rust-debug nil
  "Whether debug logging (in Rust code) should be enabled. Probably mainly
useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-log-emacs-debug nil
  "Whether debug logging (in Emacs lisp code) should be enabled. Probably mainly
useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean)

(defcustom tlc-log-to-stdio nil
  "In addition to logging to file, if logging should also happen to standard
output. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-log-file (file-truename
                         (file-name-concat
                          user-emacs-directory
                          "tiny-lsp-client.log"))
  "File name of the log file where tiny-lsp-client stores logs. If the file
doesn't exist, it's created. If the file already exists, the contents are copied
to a file with the same name but with .old as suffix, as a simple log
rotation. Note that the contents of .old are not preserved."
  :group 'tiny-lsp-client
  :type 'file
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-stop-server-on-stderr nil
  "If there is any output on stderr from the LSP server, stop it. This is to
make developing and debugging of tiny-lsp-client easier by making it obvious
when stderr happens, so that it can be debugged immediately and the situation
when it happened is known."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :set 'tlc--set-rust-based-option)

(defcustom tlc-before-start-server-hook nil
  "List of functions to be called before an LSP server is started for a root
path. When an existing LSP server is connected to, this hook is not run."
  :type 'hook
  :group 'tiny-lsp-client)

(defcustom tlc-after-start-server-hook nil
  "List of functions to be called after an LSP server is started for a root
  path. When an existing LSP server is connected to, this hook is not run."
  :type 'hook
  :group 'tiny-lsp-client)

(defcustom tlc-interruptible-capf nil
  "Whether `tlc-completion-at-point' should exit at user input. If e.g. corfu
is used as front end, this can and should be left at nil since corfu itself
can interrupt the capf using `while-no-input'."
  :type 'boolean
  :group 'tiny-lsp-client)

(defcustom tlc-debug-on-error nil
  "Whether some debug `error' calls should show a stacktrace so that it's more
obvious that they happen."
  :type 'boolean
  :group 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Minor mode
;;------------------------------------------------------------------------------

;;;###autoload
(define-minor-mode tlc-mode
  "tiny-lsp-client: a minor mode for LSP."
  :lighter " tlc-mode"
  :group 'tiny-lsp-client

  ;; Clear cached root so that toggling mode (e.g. through reverting buffer)
  ;; can be used as a way to change it.
  (setq tlc--cached-root nil)
  (cond
   (tlc-mode
    (cond
     ((not (tlc--buffer-file-name-unchecked))
      (message "tiny-lsp-client can only be used in file buffers.")
      (tlc-mode -1))
     ((not (tlc--initial-get-root))
      (message "tiny-lsp-client can only be used in buffers where root can be found.")
      (tlc-mode -1))
     (t
      (tlc--start-server)
      (add-hook 'kill-buffer-hook 'tlc--kill-buffer-hook nil t)
      (add-hook 'before-revert-hook 'tlc--before-revert-hook nil t)
      (add-hook 'after-revert-hook 'tlc--after-revert-hook nil t)
      (add-hook 'before-change-functions 'tlc--before-change-hook nil t)
      (add-hook 'after-change-functions 'tlc--after-change-hook nil t)
      (add-hook 'change-major-mode-hook 'tlc--change-major-mode-hook nil t))))
   (t
    ;; disable can be sent for buffers where enabling is not appropriate,
    ;; so only send close if possible.
    (when (and (tlc--initial-get-root) (tlc--buffer-file-name-unchecked))
      (tlc--notify-text-document-did-close))
    (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
    (remove-hook 'completion-at-point-functions 'tlc-completion-at-point t)
    (remove-hook 'completion-at-point-functions 'tlc-async-cached-completion-at-point t)
    (remove-hook 'kill-buffer-hook 'tlc--kill-buffer-hook t)
    (remove-hook 'before-revert-hook 'tlc--before-revert-hook t)
    (remove-hook 'after-revert-hook 'tlc--after-revert-hook t)
    (remove-hook 'before-change-functions 'tlc--before-change-hook t)
    (remove-hook 'after-change-functions 'tlc--after-change-hook t)
    (remove-hook 'change-major-mode-hook 'tlc--change-major-mode-hook t)
    )))

(defun tlc--start-server ()
  (let* ((server-cmd (if-let ((r (alist-get major-mode tlc-server-cmds)))
                         r
                       (user-error
                        "No server command found for major mode: %s"
                        major-mode))))
    (if (cl-member (tlc--root) (tlc--all-root-paths) :test 'string-equal)
        (message "Connected to already started server in '%s'" (tlc--root))
      (tlc--run-hooks 'tlc-before-start-server-hook)
      (let* ((result (tlc--rust-start-server (tlc--root) server-cmd)))
        (tlc--log "Start server result: %s" result)
        (pcase result
          ;; normal case
          ('started (message "Started '%s' in '%s'" server-cmd (tlc--root)))

          ;; alternative but valid case
          ('start-failed (error
                          "Failed to start '%s' in '%s'. Check log for details."
                          server-cmd (tlc--root)))

          ;; bug case
          (_ (error "bad result tlc--start-server %s" result))
          )
        (tlc--run-hooks 'tlc-after-start-server-hook))
      )
    (tlc--notify-text-document-did-open)
    ))

(defun tlc--run-hooks (hooks)
  "Wrapper around `run-hooks' that runs them with root path as
`default-directory'."
  (let ((default-directory (tlc--root)))
    (run-hooks hooks)))

(defun tlc--kill-buffer-hook ()
  (tlc--log "tlc--kill-buffer-hook called. tlc-mode: %s" tlc-mode)
  (when tlc-mode
    (tlc--notify-text-document-did-close)))

(defun tlc--before-revert-hook ()
  (tlc--log "tlc--before-revert-hook called. tlc-mode: %s" tlc-mode)
  (tlc--notify-text-document-did-close))

;; To silence byte-compiler warning. It seems to only exist during revert.
;; Maybe that's a sign it shouldn't be used?
(defvar revert-buffer-preserve-modes)

(defun tlc--after-revert-hook ()
  (tlc--log "tlc--after-revert-hook called. tlc-mode: %s. revert-buffer-preserve-modes: %s"
            tlc-mode
            revert-buffer-preserve-modes)
  ;; If revert-buffer-preserve-modes is nil (default), it means that tlc-mode is
  ;; run and didOpen is called from there, and it would result in duplicate
  ;; didOpen calls. See
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Reverting.html
  ;; todo: but it seems all relevant tests passed even without this
  (when revert-buffer-preserve-modes
    (tlc--notify-text-document-did-open)))

(defun tlc--notify-text-document-did-open ()
  (let* ((revert revert-buffer-in-progress-p)
         (content (tlc--widen
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (tlc--log "didOpen. File: %s Revert in progress: %s."
              (tlc--buffer-file-name)
              revert
              )
    ;; todo: Why not if revert in progress? Related to vdiff. If uncommited changes
    ;; it lead to that those changes were overwritten.

    ;; lisp side now needs to use send the content, otherwise tlc creates deleted
    ;; files when running vidff on them. Note, that it *seems* rust-mode has
    ;; the same bug, but not lisp or erlang-mode. Useful to know when testing.
    ;; todo: could consider to let rust side send if possible, i.e. if file
    ;; exists and is saved
    (tlc--send-notification "textDocument/didOpen"
                            (list (tlc--buffer-uri) content)
                            )))

(defun tlc--send-notification (method params &optional dont-ask-if-no-server)
  (let ((return (tlc--rust-send-notification
                 (tlc--root)
                 method
                 params)))
    (tlc--log "Send notification return: %s" return)
    (pcase return
      ;; normal case - could send the notifcation
      ('ok nil)

      ;; alternative but valid case - server crashed or not started
      ('no-server (unless dont-ask-if-no-server
                    (tlc--ask-start-server)))

      ;; bug case - some other return
      (_ (error "bad return")))))

(defun tlc--notify-text-document-did-close ()
  (tlc--send-notification
   "textDocument/didClose"
   (list (tlc--buffer-uri))
   ;; If there's no server, there's no point in starting one to send didClose.
   'dont-ask-if-no-server))

;; todo: lsp-mode becomes disabled if rust-mode becomes disabled, but something
;; like this wasn't needed. Why? Eglot has it too though, but doesn't check
;; revert-buffer-in-progress-p
(defun tlc--change-major-mode-hook ()
  (tlc--log "tlc--change-major-mode-hook called. tlc-mode: %s. Revert in progress: %s"
            tlc-mode
            revert-buffer-in-progress-p)
  (unless revert-buffer-in-progress-p
    (tlc-mode -1)))

;; -----------------------------------------------------------------------------
;; didChange
;;------------------------------------------------------------------------------

(defvar-local tlc--change nil)

(defun tlc--before-change-hook (beg end)
  (tlc--log "tlc--before-change-hook called (%s %s). revert-buffer-in-progress-p: %s. tlc--change: %s."
            beg
            end
            revert-buffer-in-progress-p
            tlc--change)
  (when tlc--change
    ;; I know this is overly simplified, but when this case happens, I fix it
    ;; one idea could be send full document since these cases should hopefully
    ;; be rare
    (tlc--error "tlc--change is not-nil in before-change"))
  ;; if revert in progress, it can happen that didChange is sent before didOpen,
  ;; when discarding changes in magit
  (unless revert-buffer-in-progress-p
    (setq tlc--change (append (tlc--pos-to-lsp-pos beg) (tlc--pos-to-lsp-pos end)))))

;; @credits: Heavily inspired by eglot
;; nil pos means current point
(defun tlc--pos-to-lsp-pos (&optional pos)
  ;; Need to save restriction and widen to handle e.g. lsp-rename from
  ;; lsp-mode, and I guess other cases where restriction is used.
  (tlc--widen
   (let* ((line (- (line-number-at-pos pos) 1))
          (character (progn (when pos
                              (goto-char pos))
                            (tlc--utf-16-linepos))))
     (list line character))))

;; @credits: copied/inspired/then modified from eglot
(defun tlc--utf-16-linepos ()
  (/ (- (length (encode-coding-region (pos-bol)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun tlc--after-change-hook (beg end _pre-change-length)
  (tlc--log "tlc--after-change-hook called (%s %s). revert-buffer-in-progress-p: %s. tlc--change: %s."
            beg
            end
            revert-buffer-in-progress-p
            tlc--change)
  (unless tlc--change
    (error "tlc--change is nil in after-change"))
  (let* ((start-line      (nth 0 tlc--change))
         (start-character (nth 1 tlc--change))
         (end-line        (nth 2 tlc--change))
         (end-character   (nth 3 tlc--change))
         (text (tlc--widen
                (buffer-substring-no-properties beg end)))
         )
    (setq tlc--change nil)
    ;; if revert in progress, it can happen that didChange is sent before didOpen
    ;; when discarding changes in magit
    (unless revert-buffer-in-progress-p
      (tlc--notify-text-document-did-change start-line
                                            start-character
                                            end-line
                                            end-character
                                            text))))

(defun tlc--notify-text-document-did-change (start-line
                                             start-character
                                             end-line
                                             end-character
                                             text)
  (tlc--send-notification
   "textDocument/didChange"
   (list (tlc--buffer-uri)
         `((,start-line ,start-character ,end-line ,end-character ,text))
         )))

;; -----------------------------------------------------------------------------
;; Request/response
;;------------------------------------------------------------------------------

;; @credits: Reqeust/response mechanism inspired by
;; https://github.com/zbelial/lspce
(defun tlc--sync-request (method arguments)
  (let ((request-id (tlc--request method arguments (tlc--root))))
    ;; Use 100ms as Rust timeout to avoid too fast busy-wait loop, but 100ms
    ;; is still responsive enough to C-g aborts.
    (tlc--wait-for-response request-id (tlc--root) 100 0 nil)))

;; tlc--request might be called from unexpected buffers due to async
;; completion, so can't call (tlc--root) inside, so pass root-path
(defun tlc--request (method arguments root-path)
  (let ((return (tlc--rust-send-request root-path method arguments)))
    (tlc--log "Send request return: %s" return)
    (cond
     ;; normal case - request sent and request-id returned
     ((integerp return) return)

     ;; alternative but valid case - server crashed or not started
     ((equal 'no-server return) (progn
                                  (tlc--ask-start-server)
                                  ;; if not error, then xref says incorrect type
                                  (error "")))

     ;; bug case - bad return
     (t (error "bad return")))))

;; tlc--wait-for-response might be called from unexpected buffers due to async
;; completion, so can't call (tlc--root) inside, so pass root-path
(defun tlc--wait-for-response (request-id root-path rust-timeout
                                          emacs-timeout interruptible)
  "Wait for a response to REQUEST-ID from the server managing ROOT-PATH.
RUST-TIMEOUT is the non-interruptible time between each wait call. The type is
integer, unit milliseconds. EMACS-TIMEOUT is the interruptible time between each
wait call, interruptible both by C-g and any user input. The type is float, unit
seconds. INTERRUPTIBLE means exit on user input. Otherwise, only exists on C-g
as usual."
  (let ((return (tlc--rust-recv-response root-path rust-timeout))
        (continue (lambda ()
                    (tlc--wait-for-response
                     request-id root-path rust-timeout emacs-timeout interruptible))))
    (tlc--log "tlc--rust-recv-response return: %s" return)
    (pcase return
      ;; normal case - response has arrived
      (`(response ,id ,has-result ,params)
       (cond
        ;; alternative but valid case - response to old request
        ((< id request-id) (funcall continue))

        ;; bug case - response to request id not yet sent
        ((> id request-id) (error "too big id"))

        ;; normal case - response to current request
        ;; todo: for now, has-result=nil is re-interpreted as params=nil which
        ;; happens to work for textDocument/definition and
        ;; textDocument/completion but it might not be the case in the future
        ;; for all respones
        (t                 (when has-result params))))

      ;; normal case - no response yet
      ('no-response
       ;; todo: consider exponential back-off
       ;; If interruptible, use sit-for so that user-input immediately exits.
       ;; But if sit-for returns t it means no user-input so continue to loop.
       ;; If not interruptible, use sleep-for to avoid unecesseary recursion
       ;; if the user type while waiting for a response.
       (if (and interruptible (not (sit-for emacs-timeout)))
           'interrupted
         ;; Be careful about paranthesis and indentation so that 'interrupted
         ;; is indeed returned
         (unless interruptible
           (sleep-for emacs-timeout))
         (funcall continue)))

      ;; alternative but valid case - some error response
      ;; For now, just print a message, because so far I've only encountered it
      ;; for temporary issues. In the future, consider passing code and msg.
      ('error-response (user-error
                        (concat "Got error response from LSP server."
                                "It might be a temporary issue."
                                "But if it keeps happening, you can check the IO logs")))

      ;; alternative but valid case - server crashed/stopped while waiting
      ;; for response. After server maybe restarted, exit.
      ('no-server (progn
                    (tlc--ask-start-server)
                    (error "")))

      ;; bug case - bad return
      (_ (error "bad return"))
      )))

;; -----------------------------------------------------------------------------
;; xref
;;------------------------------------------------------------------------------

(defun tlc-use-xref ()
  (interactive)
  (when tlc-mode
    (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)))

(defun tlc-xref-backend () 'xref-tlc)

;; @credits: Inspired from https://github.com/emacs-lsp/lsp-mode 
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tlc)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-tlc)) _identifier)
  (let* ((uri (tlc--buffer-uri))
         (pos (tlc--pos-to-lsp-pos))
         (line (nth 0 pos))
         (character (nth 1 pos))
         (response (tlc--sync-request
                    "textDocument/definition"
                    (list uri line character))))
    (mapcar (lambda (location)
              (pcase-let ((`(,uri-target ,line-start ,character-start) location))
                (let* ((line-target (+ line-start 1))
                       (file-target (tlc--uri-to-file-name uri-target)))
                  (xref-make
                   file-target
                   (xref-make-file-location file-target line-target character-start)))))
            response)))

;; -----------------------------------------------------------------------------
;; capf
;; -----------------------------------------------------------------------------

(defun tlc-use-capf ()
  (interactive)
  (when tlc-mode
    (add-hook 'completion-at-point-functions 'tlc-completion-at-point nil t)))

;; For company integration, can consider clearing this on start completion
(defvar tlc--last-candidates nil)

;; note, due to null returning immediately, it was much faster to type with
;; async capf. Also, when null resp fixed, and spamming, emacs froze completely.

;; to simplify, capf is now always async. lsp-mode and eglot are async
;; in similar ways, so hopefully OK.
;; @credits: Inspired by eglot
(defun tlc-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (uri (tlc--buffer-uri))
         (pos (tlc--pos-to-lsp-pos))
         (line (car pos))
         (character (cadr pos))
         (cached-candidates 'none)
         (root (tlc--root))
         (response-fun (lambda ()
                         (if (listp cached-candidates) cached-candidates
                           (let* ((request-id (tlc--request
                                               "textDocument/completion"
                                               (list uri line character)
                                               root))
                                  (result
                                   ;; Use 0ms rust timeout since for capf want
                                   ;; to interrupt as soon as possible. Use
                                   ;; 0.005s as emacs timeout because if too
                                   ;; long, it means we wait too long before
                                   ;; checking again if a response has arrived.
                                   (tlc--wait-for-response request-id root
                                                           0 0.005
                                                           tlc-interruptible-capf)))
                             (cond
                              ((eq result 'interrupted)
                               tlc--last-candidates)
                              ;; Finished (todo: or C-g, need to think about that)
                              (t
                               (setq tlc--last-candidates result)
                               (setq cached-candidates result)))))))
         )
    (list
     (or (car bounds) (point))
     (or (cdr bounds) (point))
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata) (progn
                                 '(metadata . nil)))

        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall response-fun) probe pred))))
     :annotation-function
     (lambda (_item)
       (concat "tlc")
       )
     )
    )
  )

;; -----------------------------------------------------------------------------
;; Async cached capf (experimental)
;; -----------------------------------------------------------------------------

(defun tlc-use-async-cached-capf ()
  (interactive)
  (when tlc-mode
    (add-hook 'completion-at-point-functions 'tlc-async-cached-completion-at-point nil t)))

(defvar tlc--async-current-timer nil)
(defvar tlc--async-reqeust-id nil)
(defvar tlc--async-cached-candidates nil)
(defvar tlc--async-cache-start-point nil)
(defvar tlc--async-cache-symbol nil)
(defvar tlc--async-root-path nil)

(defun tlc-async-cached-completion-at-point ()
  "As soon as the user starts typing in a new location, fetch completion
candidates from the LSP server in the background. Whenever the user is not
typing, emacs waits for a response. Once a response is received, cache it, and
re-use whenever the user completes in the same
area. `tlc-async-cached-completion-at-point' works well when the LSP server is
slow (1000-2000ms). It works under the assumption that new candidates are the
same as the once already fetched as long as the prefix of what the user has
typed stays the same. todo: this can be improved by re-fetching all the time
and always using the latest result."
  (let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (list
     (or (car bounds) (point))
     (or (cdr bounds) (point))
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata) (progn
                                 '(metadata . nil)))

        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (tlc--async-collection-fun) probe pred))))
     :annotation-function
     (lambda (_item)
       (concat "tlc cached async") ;; temporary, to see that completion comes from tlc
       )
     )
    )
  )

(defun tlc--async-collection-fun ()
  (interactive)
  (cond
   ;; Can use cache
   ((tlc--async-can-use-cache-p) tlc--async-cached-candidates)

   ;; Refresh already in progress
   ((integerp tlc--async-reqeust-id) (tlc--async-refresh-cache-1))

   ;; Need to start a new refresh
   (t (tlc--async-refresh-cache))))

(defun tlc--async-can-use-cache-p ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point)))
         (current-symbol (buffer-substring-no-properties start end)))
    (and tlc--async-cache-symbol
         tlc--async-cache-start-point
         (string-prefix-p tlc--async-cache-symbol current-symbol)
         (eq start tlc--async-cache-start-point))))

(defun tlc--async-refresh-cache ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point)))
         (uri (tlc--buffer-uri))
         (pos (tlc--pos-to-lsp-pos))
         (line (car pos))
         (character (cadr pos))
         (request-id (tlc--sync-request
                      "textDocument/completion"
                      (list uri line character))))
    (when tlc--async-current-timer
      (cancel-timer tlc--async-current-timer))
    (setq tlc--async-reqeust-id request-id)
    (setq tlc--async-cache-start-point start)
    (setq tlc--async-root-path (tlc--root))
    (setq tlc--async-cache-symbol (buffer-substring-no-properties start end))
    (tlc--async-refresh-cache-1)))

(defun tlc--async-refresh-cache-1 ()
  (let* ((while-result
          (while-no-input
            (tlc--wait-for-response tlc--async-reqeust-id tlc--async-root-path))))
    (cond
     ((eq while-result t)
      (message "Interrupted.")
      (when tlc--async-current-timer
        (cancel-timer tlc--async-current-timer))
      (setq tlc--async-current-timer
            (run-with-idle-timer
             0 nil 'tlc--async-refresh-cache-1)))
     (t
      (message "Got result")
      (setq tlc--async-reqeust-id nil)
      (setq tlc--async-current-timer nil)
      (setq tlc--async-cached-candidates while-result)
      ))))

;; One bug: after second char typed in company, interrupted spams, and nothing
;; happens until something is typed again. Also happens with built-in cap
;; maybe when deleting text and then spamming cap

;; -----------------------------------------------------------------------------
;; Company (experimental)
;; -----------------------------------------------------------------------------

(defun company-async-tlc (command &optional _arg &rest _args)
  (interactive (list 'interactive))
  (let* (
         (capf-info (tlc-completion-at-point))
         (start (nth 0 capf-info))
         (end (nth 1 capf-info))
         (collection-fun (nth 2 capf-info))
         (probe (buffer-substring-no-properties start end))
         )
    (pcase command
      (`prefix probe)
      (`candidates
       (cons :async
             (lambda (callback)
               (let* ((candidates (funcall collection-fun probe nil t)))
                 (funcall callback candidates))))))))

;; -----------------------------------------------------------------------------
;; The "control room"
;;------------------------------------------------------------------------------

(defun tlc-open-log-file ()
  "Open tiny-lsp-client's log file."
  (interactive)
  (find-file tlc-log-file))

(defun tlc-info ()
  "Show information about running servers."
  (interactive)
  (let ((infos (tlc--rust-all-server-info)))
    (with-help-window (get-buffer-create "*tiny-lsp-client-server-info*")
      (dolist (info infos)
        (pcase-let ((`(,root-path ,command ,pid) info))
          (insert (format "Root path: %s\n" root-path))
          (insert (format "Server command: %s\n" command))
          (insert (format "Process id: %s\n" pid))
          (insert "\n")
          )
        )
      )
    infos
    )
  )

(defun tlc--all-root-paths ()
  (mapcar (lambda (info)
            (nth 0 info)
            )
          (tlc--rust-all-server-info)))

;; todo: make sure the passed root-path begings with / to make the rust code not
;; crash
;; todo: Internl tlc--stop-server
(defun tlc-stop-server (&optional root-path nowarn-not-found)
  "Stop an LSP server."
  (interactive
   (list
    (completing-read "Choose root path of server to stop: "
                     (tlc--all-root-paths)
                     nil
                     'require-match
                     nil
                     'tlc-stop-server
                     (tlc--root-unchecked))))
  (let* ((root-path (or root-path (tlc--root-unchecked))))
    (unless root-path
      (user-error "No root path specified"))
    (let* ((result (tlc--rust-stop-server root-path)))
      (tlc--log "Stop server result: %s for root-path: %s" result root-path)
      (pcase result
        ('ok nil)
        ('no-server (unless nowarn-not-found
                      (message "No server at root path '%s' could be found" root-path)))
        (_ (error "bad result tlc-stop-server %s" result))))))

(defun tlc-restart-server ()
  "Restart the LSP server of the current root path."
  (interactive)
  (tlc-stop-server nil 'nowarn-not-found)
  ;; Avoid race where tlc--start-server thinks the server is still alive
  (sleep-for 0.1)
  (tlc--start-server))

;; -----------------------------------------------------------------------------
;; Misc helpers
;;------------------------------------------------------------------------------

(defun tlc--buffer-file-name ()
  ;; In after-revert-hook, if there was a change, buffer-file-name is nil,
  ;; so use buffer-file-truename instead
  (let ((name (tlc--buffer-file-name-unchecked)))
    (cl-assert name)
    name))

(defun tlc--buffer-file-name-unchecked ()
  (let* ((bft buffer-file-truename)
         (result (and bft (file-truename bft))))
    (tlc--log "tlc--buffer-file-name-unchecked. buffer-name: '%s'\nbuffer-file-name: '%s'\nbuffer-file-truename: '%s'\nReturn: '%s'" (buffer-name) buffer-file-name bft result)
    result))

(defun tlc--ask-start-server ()
  (if (y-or-n-p "The LSP server has crashed since it was started. Want to restart it?")
      (tlc--start-server)
    (error "No LSP server running")))

(defun tlc--log (format-string &rest objects)
  (when tlc-log-emacs-debug
    (tlc--rust-log-emacs-debug (apply 'format format-string objects))))

;; An alternative could be to do URI conversion in rust with a special type
;; to make encoding and decoding centralized. But since emacs has URI
;; conversion built-in I chose to do it here. Decoding a URI is not trivial
;; due to variable length UTF8.

(defun tlc--buffer-uri ()
  "To be used when sending the current buffer file name as a URI to the LSP
server. Note that it's hard to test this in a good way since at least clangd
seems to accept URIs that are not encoded properly."
  (tlc--file-name-to-uri (tlc--buffer-file-name)))

(defun tlc--file-name-to-uri (file-name)
  ;; todo: understand better. Also, what happens if a directory name contains
  ;; / ?
  (concat "file://" (url-hexify-string file-name url-path-allowed-chars)))

(defun tlc--uri-to-file-name (uri)
  (let* ((prefix (substring uri 0 7))
         (suffix (substring uri 7)))
    (cl-assert (string= prefix "file://"))
    (decode-coding-string (url-unhex-string suffix) 'utf-8)))

(defun tlc--error (msg)
  (let ((debug-on-error tlc-debug-on-error))
    (error msg)))

;; -----------------------------------------------------------------------------
;; Root
;; -----------------------------------------------------------------------------

(defvar-local tlc--cached-root nil)

(defun tlc--initial-get-root ()
  "The initial fetch of the root for this buffer. Cache the root since if it
changes for a buffer, the server needs to be restarted anyway."
  (setq tlc--cached-root
        (when-let ((root (funcall tlc-find-root-function)))
          (file-truename root))))

(defun tlc--root ()
  "Wrapper for getting the root path of the buffer. Also checks that it's
non-nil."
  (let ((root tlc--cached-root))
    (cl-assert root)
    root))

(defun tlc--root-unchecked ()
  "Like `tlc--root' but don't check that root is non-nil. Useful for situations
a nil root is OK."
  tlc--cached-root)

(defun tlc-find-root-default-function ()
  "Get root directory using project.el."
  (when-let ((project (project-current)))
    (project-root project)))

(defun tlc-dev-find-root-function ()
  "Special root finder used for developing tiny-lsp-client itself. Finds the
nested projects inside the test directory as separate projects."
  (if (string-match-p "erlang_ls" (buffer-file-name))
      (file-name-directory (buffer-file-name))
    (if (string-match-p "clangd" (buffer-file-name))
        (file-name-directory (buffer-file-name))
      (if (string-match-p "rust_analyzer" (buffer-file-name))
          (file-name-parent-directory (file-name-directory (buffer-file-name)))
        (tlc-find-root-default-function)))))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
