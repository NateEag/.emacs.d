(defun css-mode-init ()
  "Set up css-mode the way I like it."

  (my-prog-mode-init)

  (skewer-reload-stylesheets-mode)
  (rainbow-mode)
  (ac-css-mode-setup)
  (emmet-mode)

  (setq ac-sources '(ac-source-css-property-names ac-source-css-property)))
