;; Configure web-mode.
(defun web-mode-init ()
  "My web-mode config."

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-prefer-server-commenting t)

  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 0)

  (skewer-html-mode)

  (comment-auto-fill)

  (yas-minor-mode)
  (yas-activate-extra-mode 'php-mode)

  (smartparens-mode)

  (auto-complete-mode)
  (emmet-mode))

(provide 'web-mode-init)
