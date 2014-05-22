(defun tern-complete-fallback-to-tab ()
  "If tern finds completions at point, try them. Else, usual tab behavior."
  )

(defun js-mode-init ()
  "My mode hook for JS editing modes."

  (interactive)
  (comment-auto-fill)
  (smart-dash-mode t)
  (smartparens-mode t)
  (yas-minor-mode t)
  (auto-complete-mode t)
  (setq ac-sources '(ac-source-yasnippet))
  (skewer-mode)
  (flycheck-mode)

  (tern-mode 't)
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)
       ;; Override Tern's completion-at-point keybinding with auto-complete.
       ;; I really wish I could just press Tab for this, but the current
       ;; tern-auto-complete package does not have a general-case ac-source.
       (define-key tern-mode-keymap (kbd "M-TAB") 'tern-ac-complete))))
