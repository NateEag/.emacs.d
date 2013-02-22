;; My auto-complete.el settings.

(defun ac-personal-setup ()
  "My basic configuration for autocomplete.el. Call this to activate it."
  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (setq ac-trigger-key "TAB")
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (auto-complete-mode t)
  (setq ac-initialized 't))

(provide 'auto-complete-init)
