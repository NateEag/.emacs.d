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
       (tern-ac-setup))))
