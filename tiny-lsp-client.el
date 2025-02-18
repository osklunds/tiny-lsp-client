
(require 'tlc-rust "target/release/libtiny_lsp_client.so")
(require 'subr-x)
(require 'xref)

;; -----------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

;; todo: specify types

(defcustom tlc-server-cmds
  '(
    (rust-mode . "rust-analyzer")
    (erlang-mode . "erlang_ls")
    )
  "Which server command to use for various major modes."
  :group 'tiny-lsp-client)

(defcustom tlc-find-root-function
  'tlc-find-root-default-function
  "Function used for finding the root path of a project.

Default to `tlc-find-root-default-function' which first tries Projectile,
and if that fails, tries using \"git rev-parse --show-toplevel\"." 
  :group 'tiny-lsp-client)

(defcustom tlc-log-io nil
  "Whether JSON messages between tiny-lsp-client and the LSP server should be logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-stderr t
  "Whether stderr output from the LSP server should be logged."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-rust-debug nil
  "Whether debug logging (in Rust code) should be enabled. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-emacs-debug nil
  "Whether debug logging (in Emacs lisp code) should be enabled. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean)

(defcustom tlc-log-to-stdio nil
  "In addition to logging to files, if logging should also happen to standard output. Probably mainly useful for developing tiny-lsp-client."
  :group 'tiny-lsp-client
  :type 'boolean
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-log-file (file-truename
                         (file-name-concat
                          user-emacs-directory
                          "tiny-lsp-client.log"))
  "Directory in which log files are placed."
  :group 'tiny-lsp-client
  :type 'string
  :initialize 'custom-initialize-set
  :get 'tlc--rust-get-log-option
  :set 'tlc--rust-set-log-option)

(defcustom tlc-before-start-server-hook nil
  "List of functions to be called before an LSP server is started for a root path. When an existing LSP server is connected to, this hook is not run."
  :type 'hook
  :group 'tiny-lsp-client)

(defcustom tlc-after-start-server-hook nil
  "List of functions to be called after an LSP server is started for a root path. When an existing LSP server is connected to, this hook is not run."
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
   ((not buffer-file-name)
    (message "tiny-lsp-client can only be used in file buffers.")
    (setq tlc-mode nil))
   ((not (tlc--initial-get-root))
    (message "tiny-lsp-client can only be used in buffers where root can be found.")
    (setq tlc-mode nil))
   (t
    (cond
     (tlc-mode
      (tlc--start-server)
      (add-hook 'xref-backend-functions 'tlc-xref-backend nil t)
      (add-hook 'kill-buffer-hook 'tlc--kill-buffer-hook nil t)
      (add-hook 'before-revert-hook 'tlc--before-revert-hook nil t)
      (add-hook 'after-revert-hook 'tlc--after-revert-hook nil t)
      (add-hook 'before-change-functions 'tlc--before-change-hook nil t)
      (add-hook 'after-change-functions 'tlc--after-change-hook nil t)
      (add-hook 'change-major-mode-hook 'tlc--change-major-mode-hook nil t)
      )
     (t
      ;; todo: if last buffer, stop the server
      (tlc--notify-text-document-did-close)
      (remove-hook 'xref-backend-functions 'tlc-xref-backend t)
      (remove-hook 'kill-buffer-hook 'tlc--kill-buffer-hook t)
      (remove-hook 'before-revert-hook 'tlc--before-revert-hook t)
      (remove-hook 'after-revert-hook 'tlc--after-revert-hook t)
      (remove-hook 'before-change-functions 'tlc--before-change-hook t)
      (remove-hook 'after-change-functions 'tlc--after-change-hook t)
      (remove-hook 'change-major-mode-hook 'tlc--change-major-mode-hook t)
      )))))

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
  ;; Use root path as default-directory to make it predictable and useful for
  ;; the hooks
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
         (modified (buffer-modified-p))
         (exists (file-exists-p file)))
    (tlc--log "didOpen. File: %s Revert in progress: %s. Modified: %s. Exists: %s."
              file
              revert
              modified
              exists
              )
    ;; todo: Why not if revert in progress? Related to vdiff. If uncommited changes
    ;; it lead to that those changes were overwritten.
    (when (and (or modified (not exists))
               (not revert))
      ;; todo: document this
      (tlc--log "Saving buffer due to didOpen")
      (message "tiny-lsp-client can only open saved buffers, so saving for you.")
      (save-buffer)
      )
    (tlc--send-notification "textDocument/didOpen" (list (tlc--buffer-file-name)))))

(defun tlc--send-notification (method params)
  (let ((return (tlc--rust-send-notification
                 (tlc--root)
                 method
                 params)))
    (tlc--log "Send notification return: %s" return)
    (pcase return
      ;; normal case - could send the notifcation
      ('ok nil)

      ;; alternative but valid case - server crashed or not started
      ('no-server (tlc--ask-start-server))

      ;; bug case - some other return
      (_ (error "bad return")))))

(defun tlc--notify-text-document-did-close ()
  (tlc--send-notification
   "textDocument/didClose"
   (list (tlc--buffer-file-name))))

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
  (let* ((line (- (line-number-at-pos pos) 1))
         (character (save-excursion
                      (when pos
                        (goto-char pos))
                      (current-column))))
    (list line character)))

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
         (text (buffer-substring-no-properties beg end))
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
;; The "control room"
;;------------------------------------------------------------------------------

(defun tlc-open-log-file ()
  (interactive)
  ;; todo: is this the correct way to get custom?
  (find-file (tlc--rust-get-log-option 'tlc-log-file)))

(defun tlc-info ()
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

(defun tlc-stop-server (&optional root-path nowarn-not-found)
  (interactive
   (list
    (completing-read "Choose root path of server to stop: "
                     (tlc--all-root-paths)
                     nil
                     'require-match
                     nil
                     'tlc-stop-server
                     (tlc--root))))
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
  ;; so use this instead
  (let ((name (file-truename buffer-file-truename)))
    (cl-assert name)
    name))

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

(defun tlc-find-root-default-function ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (if-let ((root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
        (unless (string-match-p "fatal:" root)
          root))))

(defun tlc-dev-find-root-default-function ()
  "Special root finder used for developing tiny-lsp-client itself. Finds the
nested projects inside the test directory as separate projects."
  (if (string-match-p "erlang_ls" (buffer-file-name))
      (file-name-directory (buffer-file-name))
    (tlc-find-root-default-function)))


(provide 'tiny-lsp-client)
;;; tiny-lsp-client.el ends here
