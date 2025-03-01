;;; tiny-lsp-client.el --- Tiny LSP Client  -*- lexical-binding: t; -*-

(require 'tlc-rust "libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)

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
  :group 'tiny-lsp-client)

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
      (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)
      (add-hook 'completion-at-point-functions 'tlc-completion-at-point nil t)
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

(defun tlc--after-revert-hook ()
  (tlc--log "tlc--after-revert-hook called. tlc-mode: %s. revert-buffer-preserve-modes: %s"
            tlc-mode
            revert-buffer-preserve-modes)
  ;; If revert-buffer-preserve-modes is nil (default), it means that tlc-mode is
  ;; run and didOpen is called from there, and it would result in duplicate
  ;; didOpen calls. See
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Reverting.html
  (when revert-buffer-preserve-modes
    (tlc--notify-text-document-did-open)))

(defun tlc--notify-text-document-did-open ()
  (let* ((file (tlc--buffer-file-name))
         (revert revert-buffer-in-progress-p)
         (content (tlc--widen
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (tlc--log "didOpen. File: %s Revert in progress: %s."
              file
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
                            (list (tlc--buffer-file-name) content)
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
   (list (tlc--buffer-file-name))
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
  (if tlc--change
      ;; I know this is overly simplified, but when this case happens, I fix it
      ;; one idea could be send full document since these cases should hopefully
      ;; be rare
      (error "tlc--change is not-nil in before-change")
    ;; if revert in progress, it can happen that didChange is sent before didOpen,
    ;; when discarding changes in magit
    (unless revert-buffer-in-progress-p
      (setq tlc--change (append (tlc--pos-to-lsp-pos beg) (tlc--pos-to-lsp-pos end))))))

;; Heavily inspired by eglot
;; nil pos means current point
(defun tlc--pos-to-lsp-pos (&optional pos)
  ;; Need to save restriction and widen to handle e.g. lsp-rename from
  ;; lsp-mode, and I guess other cases where restriction is used.
  (tlc--widen
   (let* ((line (- (line-number-at-pos pos) 1))
          (character (progn (when pos
                              (goto-char pos))
                            (current-column))))
     (list line character))))

(defun tlc--after-change-hook (beg end _pre-change-length)
  (tlc--log "tlc--after-change-hook called (%s %s). revert-buffer-in-progress-p: %s. tlc--change: %s."
            beg
            end
            revert-buffer-in-progress-p
            tlc--change)
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
   (list (tlc--buffer-file-name)
         `((,start-line ,start-character ,end-line ,end-character ,text))
         )))

;; -----------------------------------------------------------------------------
;; Request/response
;;------------------------------------------------------------------------------

(defun tlc--sync-request (method arguments)
  (let ((return (tlc--rust-send-request (tlc--root) method arguments)))
    (tlc--log "Send request return: %s" return)
    (cond
     ;; normal case - request sent and request-id returned
     ((integerp return) (tlc--wait-for-response return))

     ;; alternative but valid case - server crashed or not started
     ((equal 'no-server return) (progn
                                  (tlc--ask-start-server)
                                  ;; if not error, then xref says incorrect type
                                  (error "")))

     ;; bug case - bad return
     (t (error "bad return")))))

(defun tlc--wait-for-response (request-id)
  ;; todo: consider exponential back-off
  (sleep-for 0.01)
  (let ((return (tlc--rust-recv-response (tlc--root))))
    (tlc--log "tlc--rust-recv-response return: %s" return)
    (pcase return
      ;; normal case - response has arrived
      (`(ok-response ,id ,params)
       (cond
        ;; alternative but valid case - response to old request
        ((< id request-id) (tlc--wait-for-response request-id))

        ;; bug case - response to request id not yet sent
        ((> id request-id) (error "too big id"))

        ;; normal case - response to current request
        (t                 params)))

      ;; normal case - no response yet
      ('no-response (tlc--wait-for-response request-id))

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
;; Xref
;;------------------------------------------------------------------------------

(defun tlc-xref-backend () 'xref-tlc)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tlc)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-tlc)) identifier)
  (let* ((file (tlc--buffer-file-name))
         (pos (tlc--pos-to-lsp-pos))
         (line (nth 0 pos))
         (character (nth 1 pos))
         (response (tlc--sync-request "textDocument/definition" (list file line character))))
    (mapcar (lambda (location)
              (pcase-let ((`(,file-target ,line-start ,character-start) location))
                (let ((line-target (+ line-start 1)))
                  (xref-make
                   "todo"
                   (xref-make-file-location file-target line-target character-start)))))
            response)))

;; -----------------------------------------------------------------------------
;; Capf
;; -----------------------------------------------------------------------------

;; Inspired by eglot
(defun tlc-completion-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (file (tlc--buffer-file-name))
         (pos (tlc--pos-to-lsp-pos))
         (line (car pos))
         (character (cadr pos))
         (cached-response 'none)
         (response-fun (lambda ()
                         (if (listp cached-response) cached-response
                           (setq cached-response
                                 (tlc--sync-request
                                  "textDocument/completion"
                                  (list file line character))))))
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
     ))
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
  (let* ((root-path (or root-path (tlc--root))))
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

;; -----------------------------------------------------------------------------
;; Root
;; -----------------------------------------------------------------------------

(defvar-local tlc--cached-root nil)

(defun tlc--initial-get-root ()
  "The initial fetch of the root for this buffer. Cache the root since if it
changes for a buffer, the server needs to be restarted anyway."
  (setq tlc--cached-root (funcall tlc-find-root-function)))

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
  "Default function for find the root path of a buffer. First tries Projectile,
and if that fails, tries using \"git rev-parse --show-toplevel\"."
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (if-let ((root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
        (unless (string-match-p "fatal:" root)
          root))))

(defun tlc-dev-find-root-function ()
  "Special root finder used for developing tiny-lsp-client itself. Finds the
nested projects inside the test directory as separate projects."
  (if (string-match-p "erlang_ls" (buffer-file-name))
      (file-name-directory (buffer-file-name))
    (if (string-match-p "clangd" (buffer-file-name))
        (file-name-directory (buffer-file-name))
      (tlc-find-root-default-function))))

(cl-defmacro tlc--widen (&rest body)
  `(save-excursion (save-restriction (widen) ,@body)))


(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
