(defun css-mode-init ()
  "Set up css-mode the way I like it."

  (smartparens-mode)
  (skewer-reload-stylesheets-mode)
  (rainbow-mode)
  (auto-complete-mode)
  (ac-css-mode-setup)
  (flycheck-mode)
  (emmet-mode))
