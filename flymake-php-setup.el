;; Set up flymake for PHP.

;; Something like the following must be in php.ini to make syntax errors not
;; crash Flymake's PHP support.
; error_reporting = E_ERROR|E_COMPILE_ERROR|E_CORE_ERROR|E_PARSE

(defun flymake-php-setup ()
  (interactive)

  (flymake-init)

  ;; Add my phpcs install dir to my emacs environment's PATH.
  (setenv "PATH" (concat (getenv "PATH") ":~/.emacs.d/php-cs-1.4.4/scripts"))

  (setq flymake-phpcs-standard
        "~/.emacs.d/php-cs-1.4.4/CodeSniffer/Standards/PSR2")
  (setq flymake-phpcs-command "~/.emacs.d/site-lisp/flymake-phpcs/bin/flymake_phpcs")
  (require 'flymake-phpcs))
