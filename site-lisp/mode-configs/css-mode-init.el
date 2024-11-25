(defun css-mode-init ()
  "Set up css-mode the way I like it."

  (skewer-reload-stylesheets-start-editing)

  (rainbow-mode)
  (diminish 'rainbow-mode)

  (ac-css-mode-setup)
  (emmet-mode)

  (setq ac-sources '(ac-source-css-property-names ac-source-css-property)))
