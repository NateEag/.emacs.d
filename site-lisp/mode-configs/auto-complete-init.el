;; My auto-complete.el settings.

(defun auto-complete-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")

(setq ac-initialized nil)
(defun auto-complete-init ()
  "My basic configuration for autocomplete.el."

  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)
  ;; (ac-config-default)
  (if (not ac-initialized)
      (progn
        (setq ac-auto-start nil)
        (custom-set-variables
         '(ac-trigger-key "TAB"))
        (define-key ac-completing-map "\C-n" 'ac-next)
        (define-key ac-completing-map "\C-p" 'ac-previous)
        (setq ac-dwim t)

        (setq ac-initialized t)))
  (auto-complete-tab-noconflict))
