;; Configure web-mode.

(defun web-mode-smart-dash-insert ()
  "A wrapper around smart-dash-mode for use with web-mode."
  ;; Only be smart about dashes in languages where it makes sense.
  (if (member (web-mode-language-at-pos) (list "html" "css"))
      (self-insert-command)
    (smart-dash-do-insert)))

(defun web-mode-install-smart-dash-insert ()
  "When called, override smart-dash-mode's usual keybinding for '-'."
  (let ((map smart-dash-mode-keymap))
    (make-local-variable 'smart-dash-mode-keymap)
    (setq smart-dash-mode-keymap (copy-keymap map)))
  (define-key smart-dash-mode-keymap "-" 'web-mode-smart-dash-insert))

(defun web-mode-init ()
  "My web-mode config."

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-comment-style 2)

  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 0)

  (skewer-html-mode)

  (comment-auto-fill)

  (yas-minor-mode)
  (yas-activate-extra-mode 'php-mode)

  (smartparens-mode)

  (auto-complete-mode)

  (emmet-mode)

  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (tagedit-mode 1)

  (setq smart-dash-c-modes (cons 'web-mode smart-dash-c-modes))
  (smart-dash-mode)
  (web-mode-install-smart-dash-insert))

(provide 'web-mode-init)
