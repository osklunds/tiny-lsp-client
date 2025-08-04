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
(require 'eldoc)

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
                             (java-mode . "jdtls")
                             (haskell-mode . "haskell-language-server-wrapper --lsp")
                             )
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client
  :type 'sexp) ;; todo: better type
;; Note: convention is space separates argument. lib::get_server_key is on
;; the critical path and converting lisp list to vec everytime is too
;; expensive.

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

  ;; Clear cached root and server cmd so that toggling mode (e.g. through
  ;; reverting buffer) can be used as a way to change them.
  (setq tlc--root nil)
  (setq tlc--server-cmd nil)
  (cond
   (tlc-mode
    (cond
     ((not (tlc--can-use 'print-message))
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
    ;; disable can be called for buffers where tlc-mode can't be used. So only
    ;; if met try to send close. Need to send close since mode is disabled and
    ;; thus file is no longer managed by the LSP client.
    (when (tlc--can-use)
      (tlc--notify-text-document-did-close))
    (remove-hook 'kill-buffer-hook 'tlc--kill-buffer-hook t)
    (remove-hook 'before-revert-hook 'tlc--before-revert-hook t)
    (remove-hook 'after-revert-hook 'tlc--after-revert-hook t)
    (remove-hook 'before-change-functions 'tlc--before-change-hook t)
    (remove-hook 'after-change-functions 'tlc--after-change-hook t)
    (remove-hook 'change-major-mode-hook 'tlc--change-major-mode-hook t)
    )))

(defun tlc--can-use (&optional print-message-p)
  (let ((conditions
         '((tlc--buffer-file-name-unchecked
            "tiny-lsp-client can only be used in file buffers.")
           (tlc--initial-get-root
            "tiny-lsp-client can only be used in buffers where root can be found.")
           (tlc--initial-get-server-cmd
            "tiny-lsp-client can only be used in buffers where a server-cmd can be found.")))
        (can-use t))
    (while conditions
      (let* ((current (car conditions))
             (test (car current))
             (text (cadr current)))
        (setq conditions (cdr conditions))
        (unless (funcall test)
          (when print-message-p
            (message text))
          (setq conditions nil)
          (setq can-use nil))))
    can-use))

(defun tlc--start-server ()
  (if (cl-member (tlc--server-key) (tlc--all-server-keys) :test 'equal)
      (message "Connected to already started '%s' in '%s'" (tlc--server-cmd) (tlc--root))
    (tlc--run-hooks 'tlc-before-start-server-hook)
    (let* ((result (tlc--rust-start-server (tlc--server-key))))
      (tlc--log "Start server result: %s" result)
      (pcase result
        ;; normal case
        ('started (message "Started '%s' in '%s'" (tlc--server-cmd) (tlc--root)))

        ;; alternative but valid case
        ('start-failed (error
                        "Failed to start '%s' in '%s'. Check log for details."
                        (tlc--server-cmd) (tlc--root)))

        ;; bug case
        (_ (error "bad result tlc--start-server %s" result))
        )
      (tlc--run-hooks 'tlc-after-start-server-hook))
    )
  (tlc--notify-text-document-did-open))

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
                 (tlc--server-key)
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
  ;; If revert in progress, it can happen that didChange is sent before didOpen,
  ;; when discarding changes in magit
  (unless revert-buffer-in-progress-p
    (if tlc--change
        ;; If there already is a tlc--change it means before-change and
        ;; after-change were not called as a balanced pair. So send full
        ;; document to get out of this situation.
        (progn
          (tlc--log "tlc--before-change-hook called with non-nil tlc--change, full change")
          (setq tlc--change nil)
          (tlc--notify-text-document-did-change-full))
      (setq tlc--change (append (tlc--pos-to-lsp-pos beg) (tlc--pos-to-lsp-pos end))))))

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
  ;; if revert in progress, it can happen that didChange is sent before didOpen
  ;; when discarding changes in magit
  (unless revert-buffer-in-progress-p
    (if tlc--change
        (let* ((start-line      (nth 0 tlc--change))
               (start-character (nth 1 tlc--change))
               (end-line        (nth 2 tlc--change))
               (end-character   (nth 3 tlc--change))
               (text (tlc--widen
                      (buffer-substring-no-properties beg end)))
               )
          (setq tlc--change nil)
          (tlc--notify-text-document-did-change text
                                                start-line
                                                start-character
                                                end-line
                                                end-character))
      ;; If there is no tlc--change it means before-change and
      ;; after-change were not called as a balanced pair. So send full
      ;; document to get out of this situation.
      (tlc--log "tlc--after-change-hook called with nil tlc--change, full change")
      (tlc--notify-text-document-did-change-full))))

(defun tlc--notify-text-document-did-change (text &optional
                                                  start-line
                                                  start-character
                                                  end-line
                                                  end-character)
  (let ((content-change (if start-line
                            `(,text ,start-line ,start-character ,end-line ,end-character)
                          `(,text))))
    (tlc--send-notification
     "textDocument/didChange"
     (list (tlc--buffer-uri)
           (list content-change)))))

(defun tlc--notify-text-document-did-change-full ()
  (tlc--notify-text-document-did-change
   (tlc--widen
    (buffer-substring-no-properties (point-min) (point-max)))))

;; -----------------------------------------------------------------------------
;; Request/response
;;------------------------------------------------------------------------------

;; @credits: Reqeust/response mechanism inspired by
;; https://github.com/zbelial/lspce
(defun tlc--request (method arguments rust-timeout emacs-timeout interruptible)
  (when-let ((request-id (tlc--send-request method arguments (tlc--server-key))))
    (tlc--wait-for-response request-id (tlc--server-key)
                            rust-timeout emacs-timeout interruptible)))

;; tlc--request might be called from unexpected buffers due to async
;; completion, so can't call (tlc--server-key) inside, so pass server-key
(defun tlc--send-request (method arguments server-key)
  (let ((return (tlc--rust-send-request server-key method arguments)))
    (tlc--log "Send request return: %s" return)
    (cond
     ;; normal case - request sent and request-id returned
     ((integerp return) return)

     ;; alternative but valid case - server crashed or not started
     ((equal 'no-server return) (progn
                                  (tlc--ask-start-server)
                                  ;; if no, above throws error
                                  nil))

     ;; bug case - bad return
     (t (error "bad return")))))

;; tlc--wait-for-response might be called from unexpected buffers due to async
;; completion, so can't call (tlc--root) inside, so pass server-key
(defun tlc--wait-for-response (request-id server-key rust-timeout
                                          emacs-timeout interruptible)
  "Wait for a response to REQUEST-ID from the server managing SERVER-KEY.
RUST-TIMEOUT is the non-interruptible time between each wait call. The type is
integer, unit milliseconds. EMACS-TIMEOUT is the interruptible time between each
wait call, interruptible both by C-g and any user input. The type is float, unit
seconds. INTERRUPTIBLE means exit on user input. Otherwise, only exists on C-g
as usual."
  (let ((return (tlc--rust-recv-response server-key rust-timeout))
        (continue (lambda ()
                    (tlc--wait-for-response
                     request-id server-key rust-timeout emacs-timeout interruptible))))
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
        ;; for all responses
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

(defun tlc-xref-backend () 'xref-tlc)

;; @credits: Inspired from https://github.com/emacs-lsp/lsp-mode 
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tlc)))
  (when tlc-mode
    (propertize (or (thing-at-point 'symbol) "")
                'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-tlc)) _identifier)
  (when tlc-mode
    (let* ((uri (tlc--buffer-uri))
           (pos (tlc--pos-to-lsp-pos))
           (line (nth 0 pos))
           (character (nth 1 pos))
           ;; Use 100ms as Rust timeout to avoid too fast busy-wait loop, but
           ;; 100ms is still responsive enough to abort with C-g.
           (response (tlc--request
                      "textDocument/definition"
                      (list uri line character)
                      100 0 nil)))
      (mapcar (lambda (location)
                (pcase-let ((`(,uri-target ,line-start ,character-start) location))
                  (let* ((line-target (+ line-start 1))
                         (file-target (tlc--uri-to-file-name uri-target)))
                    (xref-make
                     file-target
                     (xref-make-file-location file-target line-target character-start)))))
              response))))

;; -----------------------------------------------------------------------------
;; capf
;; -----------------------------------------------------------------------------

;; For company integration, can consider clearing this on start completion
(defvar tlc--last-candidates nil)

;; note, due to null returning immediately, it was much faster to type with
;; async capf. Also, when null resp fixed, and spamming, emacs froze completely.

;; to simplify, capf is now always async. lsp-mode and eglot are async
;; in similar ways, so hopefully OK.
;; @credits: Inspired by eglot
(defun tlc-completion-at-point ()
  (when tlc-mode
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (uri (tlc--buffer-uri))
           (pos (tlc--pos-to-lsp-pos))
           (line (car pos))
           (character (cadr pos))
           (cached-candidates 'none)
           (response-fun (lambda ()
                           (if (listp cached-candidates) cached-candidates
                             (let* ((result (tlc--request
                                             "textDocument/completion"
                                             (list uri line character)
                                             ;; Use 0ms rust timeout since for
                                             ;; capf want to interrupt as soon
                                             ;; as possible. Use 0.005s as emacs
                                             ;; timeout because if too long, it
                                             ;; means we wait too long before
                                             ;; checking again if a response has
                                             ;; arrived.
                                             0 0.005 tlc-interruptible-capf)))
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
  )

;; -----------------------------------------------------------------------------
;; eldoc
;; -----------------------------------------------------------------------------

(defun tlc-eldoc-function (_callback)
  (let* ((uri (tlc--buffer-uri))
         (pos (tlc--pos-to-lsp-pos))
         (line (nth 0 pos))
         (character (nth 1 pos))
         ;; As a simplification, don't have hover requests "in the background".
         ;; If cursor moves, abort.
         (response (tlc--request
                    "textDocument/hover"
                    (list uri line character)
                    ;; Use 0ms rust timeout since want to be able to interrupt
                    ;; as soon as the user moves. Use 0.1s as emacs timeout
                    ;; because unlike capf, eldoc is not timing critical.
                    0 0.1 'interruptible)))
    (tlc--log "eldoc response: %s" response)
    (unless (eq response 'interrupted)
      response))
  )

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

(defun tlc--all-server-keys ()
  (mapcar (lambda (info)
            (list (nth 0 info) (nth 1 info)))
          (tlc--rust-all-server-info)))

;; todo: make sure the passed server-key begings with / to make the rust code
;; not crash
(defun tlc-stop-server ()
  "Stop an LSP server."
  (interactive)
  (tlc--stop-server
   (tlc--completion-to-server-key
    (completing-read "Choose server to stop: "
                     (tlc--server-key-completions)
                     nil
                     'require-match
                     nil
                     'tlc-stop-server
                     (when-let ((key (and tlc-mode (tlc--server-key))))
                       (tlc--server-key-to-completion key))))))

(defun tlc--stop-server (&optional server-key nowarn-not-found)
  (let* ((server-key (or server-key (tlc--server-key))))
    (unless server-key
      (error "No server specified"))
    (let* ((result (tlc--rust-stop-server server-key)))
      (tlc--log "Stop server result: %s for server-key: %s" result server-key)
      (pcase result
        ('ok nil)
        ('no-server (unless nowarn-not-found
                      (message "No server with key '%s' could be found" server-key)))
        (_ (error "bad result tlc--stop-server %s" result))))))

(defun tlc--server-key-completions ()
  (mapcar #'tlc--server-key-to-completion (tlc--all-server-keys)))

(defun tlc--server-key-to-completion (key)
  (format "%s @ %s" (car key) (cadr key)))

(defun tlc--completion-to-server-key (completion)
  (if (string-match "\\([^@]+\\) @ \\([^@]+\\)" completion)
      (let ((root (match-string 1 completion))
            (server-key (match-string 2 completion)))
        (list root server-key))
    (error "Could not convert to server key")))

(defun tlc-restart-server ()
  "Restart the LSP server of the current buffer."
  (interactive)
  (tlc--stop-server nil 'nowarn-not-found)
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
    (tlc-mode -1)
    (user-error "You chose not the restart the LSP server. Disabling tlc-mode.")))

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
  (if tlc-debug-on-error
      (let ((debug-on-error t))
        (error msg))
    (message msg)))

;; -----------------------------------------------------------------------------
;; Server key
;; -----------------------------------------------------------------------------

(defun tlc--server-key ()
  (list (tlc--root) (tlc--server-cmd)))

;;;; ---------------------------------------------------------------------------
;;;; Root
;;;; ---------------------------------------------------------------------------

(defvar-local tlc--root nil)

(defun tlc--initial-get-root ()
  "The initial fetch of the root for this buffer. Cache the root since if it
changes for a buffer, the server needs to be restarted anyway."
  (setq tlc--root
        (when-let ((root (funcall tlc-find-root-function)))
          (file-truename root))))

(defun tlc--root ()
  "Wrapper for getting the root path of the buffer. Also checks that it's
non-nil."
  (let ((root tlc--root))
    (cl-assert root)
    root))

(defun tlc--root-unchecked ()
  "Like `tlc--root' but don't check that root is non-nil. Useful for situations
a nil root is OK."
  tlc--root)

(defun tlc-find-root-default-function ()
  "Get root directory using project.el."
  (tlc--log "tlc-find-root-default-function '%s' '%s'"
            default-directory (project-current))
  (when-let ((project (project-current)))
    (project-root project)))

;;;; ---------------------------------------------------------------------------
;;;; Server cmd
;;;; ---------------------------------------------------------------------------

(defvar-local tlc--server-cmd nil)

(defun tlc--initial-get-server-cmd ()
  (setq tlc--server-cmd (alist-get major-mode tlc-server-cmds)))

(defun tlc--server-cmd ()
  (let ((server-cmd tlc--server-cmd))
    (cl-assert server-cmd)
    server-cmd))

(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
