;; Configure web-mode.

(defvar web-mode-before-auto-complete-hooks nil
  "List of functions to run before triggering the auto-complete library.

Auto-complete sources will sometimes need some tweaking to work
nicely with web-mode. This hook gives users the chance to adjust
the environment as needed for ac-sources, right before they're used.")

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (plist-get (web-mode-point-context (point)) :language)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil))
               )))

(defvar web-mode-ac-sources-alist nil
  "alist mapping language names as string to auto-complete sources for that language.")

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-php-auto-yasnippets ac-source-yasnippet))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ;; DEBUG ac-source-css-property doesn't seem to add anything to my
        ;; completions. How is it supposed to work?
        ("css" . (ac-source-emmet-css-snippets ac-source-css-property))))

(defun web-mode-trigger-ac ()
  "Adjust ac-sources based on current context."
  (interactive)
  (run-hooks 'web-mode-before-auto-complete-hooks)
  (let ((new-web-mode-ac-sources
         (assoc (plist-get (web-mode-point-context (point)) :language)
                web-mode-ac-sources-alist)))
    (setq ac-sources (cdr new-web-mode-ac-sources)))
  (ac-start))

(defun web-mode-init ()
  "My web-mode config."

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-prefer-server-commenting t)

  (yas-minor-mode)

  (skewer-html-mode)

  (comment-auto-fill)

  (autopair-init)
  (push ?{ (getf autopair-dont-pair :code))

  (auto-complete-init)
  (define-key web-mode-map (kbd "<C-tab>") 'web-mode-trigger-ac))

(provide 'web-mode-init)
