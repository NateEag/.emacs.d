;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (23723 15707 107736
;;;;;;  914000))
;;; Generated autoloads from lsp-mode.el

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be openned in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("lsp-clients.el" "lsp-css.el" "lsp-intelephense.el"
;;;;;;  "lsp-mode-pkg.el" "lsp-pyls.el" "lsp-rust.el" "lsp-solargraph.el"
;;;;;;  "lsp-vetur.el" "lsp.el") (23723 15707 106350 194000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-mode-autoloads.el ends here
