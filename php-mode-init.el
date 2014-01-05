;; Emacs config for editing PHP.

;; GRIPE Should I get this working, it probably doesn't belong here.
(defun insert-func-and-auto-yasnippet ()
  "Insert the selected function name then insert its auto-snippet."
  (ac-expand)
  ;; DEBUG Do we need to handle classes? For the moment, I don't think it's
  ;; necessary.
  (yas/create-php-snippet nil))

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

  ;; Who doesn't like yasnippet?
  (yasnippet-init)

  ;; auto-pairs ftw
  ;; I'm giving smartparens a try.
  ;;(autopair-init)
  (smartparens-mode)

  ;; Autocompletion for everyone!
  (auto-complete-init)
  (ac-define-source php-auto-yasnippets
                    ;; DEBUG not sure what 'depends' does - it's used in the
                    ;; yasnippet ac source, though, so I just mirrored it
                    ;; blindly.
                    '((depends yasnippet)
                      (depends php-auto-yasnippets)
                      ;; DEBUG This will need to change - people will want to
                      ;; customize their list of PHP callables, since the
                      ;; auto-complete dictionary is not very up-to-date (and
                      ;; also contains keywords, which we don't want).
                      (candidates . ac-buffer-dictionary)
                      (action . insert-func-and-auto-yasnippet)
                      ;; Since these trigger yasnippet, I think it makes sense
                      ;; to use the yasnippet face.
                      (candidate-face . ac-yasnippet-candidate-face)
                      (selection-face . ac-yasnippet-selection-face)
                      ;; For 'PHP', and to distinguish from regular yasnippet
                      ;; functions.
                      (symbol . "p")))

  (add-to-list 'ac-sources 'ac-source-php-auto-yasnippets)
  (add-to-list 'ac-sources 'ac-source-yasnippet)


  (require 'php-auto-yasnippets)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

  ;; Yay for syntax checking/linting!
  ; Trying flycheck instead of flymake.
  ;(flymake-php-setup)
  (flycheck-mode t)
  (setq flycheck-phpcs-standard "PSR2")
  (setq flycheck-php-phpcs-executable "phpcs")
  )

;; Make this requireable.
(provide 'php-mode-init)
