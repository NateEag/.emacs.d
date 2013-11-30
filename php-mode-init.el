;; Emacs config for editing PHP.

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)

(setq php-sql-mmm-submode-enabled nil)
(defun php-sql-mmm-submode ()
  "Provides a very minimal embedding of SQL in PHP, via mmm-mode."
  (interactive)
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
  ;; Commented, because it breaks even worse than it used to under Emacs 24.
  ;; :(
  ;; (php-sql-mmm-submode)

  ;; Everyone loves smart-dash mode.
  (require 'smart-dash)
  (setq smart-dash-c-modes (cons 'php-mode smart-dash-c-modes))
  (smart-dash-mode t)

  (comment-auto-fill)

  ;; Everyone loves code folding.
  (hs-minor-mode-init)

  ;; Who doesn't like yasnippet?
  (yasnippet-init)

  ;; auto-pairs ftw
  (autopair-init)

  ;; Autocompletion for everyone!
  (auto-complete-init)
  (add-to-list 'ac-sources 'ac-source-yasnippet)

  ;; Yay for syntax checking/linting!
  (flymake-php-setup))

;; Make this requireable.
(provide 'php-mode-init)
