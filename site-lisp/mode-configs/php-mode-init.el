;; Emacs config for editing PHP.

;; Used to use this for highlighting SQL in PHP string, but that hasn't worked
;; in a long time, so there's no point wasting time on loading it.
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)

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

(defun php-mode-init ()
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

  ;; auto-pairs ftw
  ;; I'm giving smartparens a try.
  ;;(autopair-init)
  (smartparens-init)
  (smartparens-mode)

  ;; Who doesn't like yasnippet?
  (yasnippet-init)

  ;; Autocompletion for everyone!
  (auto-complete-init)
  (setq ac-sources nil)
  (payas/ac-setup)
  (add-to-list 'ac-sources 'ac-source-yasnippet)

  ;; w00t for auto-yasnippets!
  (require 'php-auto-yasnippets)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

  ;; Yay for syntax checking/linting!
  ; Trying flycheck instead of flymake.
  ;(flymake-php-setup)
  (flycheck-mode t)
  (setq flycheck-phpcs-standard "NateEag")
  (setq flycheck-php-phpcs-executable "phpcs"))

;; Make this requireable.
(provide 'php-mode-init)
