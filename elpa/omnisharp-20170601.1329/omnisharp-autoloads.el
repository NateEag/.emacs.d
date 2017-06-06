;;; omnisharp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "omnisharp" "omnisharp.el" (22838 44460 0 0))
;;; Generated autoloads from omnisharp.el

(autoload 'omnisharp-mode "omnisharp" "\
Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "omnisharp-auto-complete-actions" "omnisharp-auto-complete-actions.el"
;;;;;;  (22838 44460 0 0))
;;; Generated autoloads from omnisharp-auto-complete-actions.el

(autoload 'company-omnisharp "omnisharp-auto-complete-actions" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil "omnisharp-http-utils" "omnisharp-http-utils.el"
;;;;;;  (22838 44460 0 0))
;;; Generated autoloads from omnisharp-http-utils.el

(autoload 'omnisharp-post-http-message "omnisharp-http-utils" "\
Post http request to server. Return result.

\(fn URL CALLBACK &optional PARAMS ASYNC)" nil nil)

;;;***

;;;### (autoloads nil "omnisharp-server-actions" "omnisharp-server-actions.el"
;;;;;;  (22838 44460 0 0))
;;; Generated autoloads from omnisharp-server-actions.el

(autoload 'omnisharp-start-omnisharp-server "omnisharp-server-actions" "\
Starts an OmniSharp server for a given path to a project file or a directory

\(fn PATH-TO-PROJECT)" t nil)

(autoload 'omnisharp-stop-server "omnisharp-server-actions" "\
Stops Omnisharp server if running.

\(fn)" t nil)

(autoload 'omnisharp-reload-solution "omnisharp-server-actions" "\
Restarts omnisharp server on solution last loaded

\(fn)" t nil)

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

;;;### (autoloads nil nil ("omnisharp-current-symbol-actions.el"
;;;;;;  "omnisharp-format-actions.el" "omnisharp-helm-integration.el"
;;;;;;  "omnisharp-navigation-actions.el" "omnisharp-pkg.el" "omnisharp-server-management.el"
;;;;;;  "omnisharp-settings.el" "omnisharp-solution-actions.el" "omnisharp-utils.el")
;;;;;;  (22838 44460 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; omnisharp-autoloads.el ends here
