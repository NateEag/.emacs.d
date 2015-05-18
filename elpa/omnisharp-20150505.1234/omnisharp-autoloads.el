;;; omnisharp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "omnisharp" "omnisharp.el" (21850 30521 0 0))
;;; Generated autoloads from omnisharp.el

(autoload 'omnisharp-mode "omnisharp" "\
Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend.

\(fn &optional ARG)" t nil)

(autoload 'omnisharp-fix-code-issue-at-point "omnisharp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "omnisharp-auto-complete-actions" "omnisharp-auto-complete-actions.el"
;;;;;;  (21850 30521 0 0))
;;; Generated autoloads from omnisharp-auto-complete-actions.el

(autoload 'company-omnisharp "omnisharp-auto-complete-actions" "\
`company-mode' completion back-end using OmniSharp.

\(fn COMMAND &optional ARG &rest IGNORED)" nil nil)

;;;***

;;;### (autoloads nil "omnisharp-server-actions" "omnisharp-server-actions.el"
;;;;;;  (21850 30521 0 0))
;;; Generated autoloads from omnisharp-server-actions.el

(autoload 'omnisharp-start-omnisharp-server "omnisharp-server-actions" "\
Starts an OmniSharpServer for a given path to a solution file or a directory

\(fn PATH-TO-SOLUTION)" t nil)

(autoload 'omnisharp-check-alive-status "omnisharp-server-actions" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified.

\(fn)" t nil)

(autoload 'omnisharp-check-ready-status "omnisharp-server-actions" "\
Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("example-config-for-evil-mode.el" "omnisharp-pkg.el"
;;;;;;  "omnisharp-utils.el") (21850 30521 376671 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; omnisharp-autoloads.el ends here
