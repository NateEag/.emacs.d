;; My auto-complete.el settings.

(defun auto-complete-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")

(defun auto-complete-init ()
  "My basic configuration for autocomplete.el. Call this to activate it."
  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)
  (if (not ac-initialized)
      (progn
        (global-auto-complete-mode t)
        (setq ac-auto-start nil)
        (custom-set-variables
         '(ac-trigger-key "TAB"))
        ;(add-to-list 'ac-sources 'ac-source-yasnippet)
        (define-key ac-completing-map "\C-n" 'ac-next)
        (define-key ac-completing-map "\C-p" 'ac-previous)

        (setq ac-initialized t)))
  (auto-complete-mode t))
