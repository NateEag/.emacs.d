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

;; Make emacs' built-in help functions more helpful.
;;
;; They're already way better than those of any other program I use to begin
;; with, but helpful does make them noticeably more useful.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; Many minor modes don't need to be documented in the precious space of the
;; modeline.
;;
;; ...and diminish.el is a lovely reflection on the nature of those things we
;; take for granted and thus no longer notice, if you take the time to read the
;; source.
(use-package diminish
  :ensure t)

;; I love project-specific custom shell environments.
;;
;; Direnv is the best tool I've encountered for that.
(use-package envrc
  :ensure t
  :diminish
  :init
  (envrc-global-mode))

(provide 'package-conf)
;;; package-conf.el ends here
