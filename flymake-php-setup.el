;; Set up flymake for PHP.
(autoload 'flymake-init "flymake-init.el")

;; Something like the following must be in php.ini to make syntax errors not
;; crash Flymake's PHP support.
; error_reporting = E_ERROR|E_COMPILE_ERROR|E_CORE_ERROR|E_PARSE

(defun flymake-php-setup ()
  (interactive)

  (flymake-init)
  ;; Add the path to my phpcs install to my emacs environment's PATH.
  (setenv "PATH" (concat (getenv "PATH") ":~/.emacs.d/php-cs-1.4.4/scripts"))

  (setq flymake-phpcs-command "~/.emacs.d/flymake-phpcs/bin/flymake_phpcs")
  (require 'flymake-phpcs))
