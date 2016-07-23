;; My auto-complete.el settings.

;; Auto-complete source for CSS property names, from
;; https://github.com/fxbois/web-mode/issues/116#issuecomment-46450381
(defvar ac-source-css-property-names
'((candidates . (loop for property in ac-css-property-alist
                      collect (car property)))))

(setq ac-initialized nil)
(defun auto-complete-init ()
  "My basic configuration for autocomplete.el."

  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)

  ;; (ac-config-default)
  (if (not ac-initialized)
      (progn
        (ac-flyspell-workaround)
        (setq ac-auto-start nil)

        (custom-set-variables
         '(ac-trigger-key "TAB"))

        (define-key ac-completing-map "\C-n" 'ac-next)
        (define-key ac-completing-map "\C-p" 'ac-previous)
        (setq ac-dwim t)

        (setq ac-initialized t))))
