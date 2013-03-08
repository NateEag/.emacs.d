;; Emacs config for editing PHP.

(add-to-list 'load-path "~/.emacs.d/php-mode")
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "PHP editing mode." t)

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

(autoload 'comment-auto-fill "comment-auto-fill.el")

(autoload 'hs-minor-mode-init "hs-minor-mode-init.el")

(autoload 'yasnippet-init "yasnippet-init.el")

(autoload 'autopair-init "autopair-init.el")

(setq php-sql-mmm-submode-enabled nil)
(defun php-sql-mmm-submode ()
  "Provides a very minimal embedding of SQL in PHP, via mmm-mode."
  (when (not php-sql-mmm-submode-enabled)
    (set-face-background 'mmm-default-submode-face nil)
    (mmm-add-classes
     '((embedded-sql
        :submode sql-mode
        :front "$\\(sql\\|query\\) = \""
        :back "\";"
        :face mmm-code-submode-face)))
    (mmm-add-mode-ext-class 'php-mode "\\.php$" 'embedded-sql)
    (setq php-sql-mmm-submode-enabled t)))

(defun load-php-mode-accessories ()
  "Load my particular tweaks for php-mode."
  (interactive)

  ;; Initialize my php-sql submode.
  (php-sql-mmm-submode)

  ;; Everyone loves smart-dash mode.
  (require 'smart-dash)
  (setq smart-dash-c-modes (cons 'php-mode smart-dash-c-modes))
  (smart-dash-mode t)

  ;; Auto-fill comments, because life without auto-filled comments is annoying.
  (comment-auto-fill)

  ;; Everyone loves code folding.
  (hs-minor-mode-init)

  ;; Who doesn't like yasnippet?
  (yasnippet-init)
  (yas-minor-mode)

  ;; auto-pairs ftw
  (autopair-init))
(add-hook 'php-mode-hook 'load-php-mode-accessories)

;; Make this requireable.
(provide 'php-mode-init)
