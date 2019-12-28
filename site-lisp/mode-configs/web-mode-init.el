;;; web-mode-init.el -- Configure web-mode for my personal use.

;;; Commentary:

;; web-mode is a major-mode for editing HTML templates. Here, I define my
;; mode-hook for it, and several supporting functions for libraries that
;; don't work with it seamlessly out-of-the-box.

;;; Code:

(defun web-mode-smart-dash-insert ()
  "A wrapper around smart-dash-mode for use with web-mode."
  ;; Only be smart about dashes in languages where it makes sense.
  (interactive)
  (if (member (web-mode-language-at-pos) (list "html" "css"))
      (self-insert-command 1)
    (smart-dash-insert)))

(defun web-mode-install-smart-dash-insert ()
  "When called, override smart-dash-mode's usual keybinding for '-'."
  (set-minor-mode-key 'smart-dash-mode "-" 'web-mode-smart-dash-insert)
  (set-minor-mode-key 'smart-dash-mode (kbd "<kp-subtract>") 'web-mode-smart-dash-insert))

(defun web-mode-init ()
  "My web-mode config."

  ;; web-mode specific config, to make indentation and comments behave the way
  ;; I expect.
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-comment-style 2)

  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 0)

  ;; I want this for at least HTML comments, but generally I prefer it.
  (setq-local comment-multi-line t)

  ;; Start with the usual prog-mode setup. Things'll get bumpier, though,
  ;; because web-mode is an odd beast.
  (my-prog-mode-init)

  ;; Following is the mountain of web-mode-specific setup.

  (skewer-html-mode)

  (yas-activate-extra-mode 'php-mode)

  (emmet-mode)

  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "javascript")
                     (yas-activate-extra-mode 'js2-mode)
                   (yas-deactivate-extra-mode 'js2-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))
                 )))

  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
          ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("javascript" . (ac-source-yasnippet))
          ("css" . (ac-source-css-property
                    ac-source-css-property-names
                    ac-source-emmet-css-snippets))))

  ;; Use evil-matchit-mode to jump between tags.
  ;;
  ;; TODO Enable evil-matchit-mode in my evil setup. The problem is that it
  ;; gives significantly worse results in at least sh-mode, where it breaks
  ;; jumping between () and [].
  (evil-matchit-mode t)

  (auto-rename-tag-mode t)

  ;; Trying this out instead of tagedit (which I only had half-working anyway).
  ;;
  ;; TODO If it works well, update tagedit issue to reflect the fact that I'm
  ;; no longer going to work on it.
  (web-mode-edit-element-minor-mode t)

  (web-mode-install-smart-dash-insert))

(provide 'web-mode-init)
;;; web-mode-init.el ends here
