
(add-to-list 'load-path default-directory)

(require 'tiny-lsp-client)

(find-file "src/dummy.rs")

(text-mode)

(customize-set-variable 'tlc-server-cmds
                        '((text-mode . "rust-analyzer")))

(tlc-mode)

(setq xref-backend-functions '(tlc-xref-backend))
