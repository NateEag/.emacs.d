;;; package-conf.el --- My package configuration.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:
;;;
;;; Declare the packages I use and how I like them configured.
;;;
;;; The heart of my Emacs mortgage setup.
;;;
;;; Assumes elpaca-init.el has been loaded.

;;; Code:

(use-package evil :ensure t :demand t)

(use-package helpful :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(provide 'package-conf)
;;; package-conf.el ends here
