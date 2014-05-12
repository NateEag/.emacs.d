;; Emacs config for editing PHP.

;; Used to use this for highlighting SQL in PHP string, but that hasn't worked
;; in a long time, so there's no point wasting time on loading it.
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (setq php-sql-mmm-submode-enabled nil)
;; (defun php-sql-mmm-submode ()
;;   "Provides a very minimal embedding of SQL in PHP, via mmm-mode."
;;   (interactive)
;;   (when (not php-sql-mmm-submode-enabled)
;;     (set-face-background 'mmm-default-submode-face nil)
;;     (mmm-add-classes
;;      '((embedded-sql
;;         :submode sql-mode
;;         :front "$\\(sql\\|query\\) = \""
;;         :back "\";"
;;         :face mmm-code-submode-face)))
;;     (mmm-add-mode-ext-class 'php-mode "\\.php$" 'embedded-sql)
;;     (setq php-sql-mmm-submode-enabled t)))

(defun php-mode-init ()
  "Load my particular tweaks for php-mode."
  (interactive)

  ;; Initialize my php-sql submode.
  ;; Commented, because it breaks even worse than it used to under Emacs 24.
  ;; :(
  ;; (php-sql-mmm-submode)

  ;; Activate programming settings.
  (my-prog-mode-init)

  ;; Everyone loves code folding.
  (hs-minor-mode-init)

  ;; w00t for auto-yasnippets!
  ;; (In principle the autocomplete setup should usually handle the job, but
  ;; if I ever use this with end-user code, this might be necessary)
  (require 'php-auto-yasnippets)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

  ;; Set ac-sources.
  (setq ac-sources nil)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-php-auto-yasnippets t)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers t)

  ;; Yay for squiggly red lines!
  (setq flycheck-phpcs-standard "NateEag")
  (setq flycheck-php-phpcs-executable "phpcs"))

;; Make this requireable.
(provide 'php-mode-init)
