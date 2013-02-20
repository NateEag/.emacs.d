;; Emacs config for editing PHP.

(require 'smart-dash)
(setq smart-dash-c-modes (cons 'php-mode smart-dash-c-modes))

;; Trying to get sql-mode working inside php-mode, because that would be handy.
;; There are many issues with it, but the current one is that sql-mode appears
;; to be seeing the single quotes as defining a string.
(require 'mmm-auto)
(set-face-background 'mmm-default-submode-face nil)
(mmm-add-classes
 '((embedded-sql
    :submode sql-mode
    :front "$\\(sql\\|query\\) = \""
    :back "\";"
    :face mmm-code-submode-face)))
(mmm-add-mode-ext-class 'php-mode "\\.php$" 'embedded-sql)

;; PHP Mode.
(add-to-list 'load-path "~/.emacs.d/php-mode")
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "PHP editing mode." t)

(defun load-php-mode-accessories ()
  (smart-dash-mode t))
(add-hook 'php-mode-hook 'load-php-mode-accessories)
