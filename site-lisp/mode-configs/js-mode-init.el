(defun js-mode-init ()
  "My mode hook for JS editing modes."

  (interactive)
  (smart-dash-mode t)
  (comment-auto-fill)
  (smartparens-mode)
  (auto-complete-mode t)
  (skewer-mode)
  (flycheck-mode)

  (tern-mode 't)
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))
