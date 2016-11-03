(defun tern-complete-fallback-to-tab ()
  "If tern finds completions at point, try them. Else, usual tab behavior."
  )

(defun js-mode-init ()
  "My mode hook for JS editing modes."

  (interactive)

  (if (eq major-mode 'js2-mode)
      (set (make-local-variable 'normal-auto-fill-function) 'c-do-auto-fill))
  (skewer-mode)

  ;; Since I've bound C-r to regex searching, I'll use C-M-r to mean
  ;; 'refactor'.
  (js2r-add-keybindings-with-prefix "C-M-r")

  (if (not (eq major-mode 'json-mode))
      ;; I prefer eslint to jshint. If a project uses jshint, I use
      ;; .dir-locals.el to override this setting.
      (setq flycheck-checker 'javascript-eslint))

  (setq ac-sources '(ac-source-yasnippet))

  (tern-mode t))
