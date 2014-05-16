;;; web-mode-init.el -- Configure web-mode for my personal use.

;;; Commentary:

;; web-mode is a major-mode for editing HTML templates. Here, I define my
;; mode-hook for it.

;;; Code:
(defun web-mode-smart-dash-insert ()
  "A wrapper around smart-dash-mode for use with web-mode."
  ;; Only be smart about dashes in languages where it makes sense.
  (if (member (web-mode-language-at-pos) (list "html" "css"))
      (self-insert-command)
    (smart-dash-insert)))

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

  ;; See if we can hack in support for tagedit at all... web-mode seems like it
  ;; must already have most of these covered in some way.
  (set (make-local-variable 'te/skip-tag-forward-fn)
       'web-mode-element-end)

  (set (make-local-variable 'te/skip-tag-backward-fn)
       'web-mode-element-beginning)

  (set (make-local-variable 'te/empty-tag-p-fn)
       'web-mode-element-is-void)

  (set (make-local-variable 'te/current-tag-fn)
       'web-mode-te-current-tag-fn)

  (setq smart-dash-c-modes (cons 'web-mode smart-dash-c-modes))
  (smart-dash-mode)
  (web-mode-install-smart-dash-insert))

(defun web-mode-te-current-tag-fn ()
  ""
  ;; (ignore-errors
  ;;   (save-excursions
  ;;    (let* (ctx (web-mode-tags-pos))
  ;;      )
  ;;    `((:beg . ,(web-mode-tag-beginning-position))
  ;;      (:end . ,(web-mode-tag-end-position))
  ;;      ;; String from beginning position + 1 to first space.
  ;;      ;; Regexp search for [ >] should do it, in conjunction with
  ;;      ;; buffer-substring-no-properties.
  ;;      (:name . ,)
  ;;      ;; Check whether character two before (web-mode-tag-end-position) is /.
  ;;      (:self-closing . ,self-closing))))
  )

(provide 'web-mode-init)
;;; web-mode-init.el ends here
