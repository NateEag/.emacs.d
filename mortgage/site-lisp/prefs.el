;;; prefs.el --- Some custom Emacs variable settings.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I spent many years using custom.el to store my customized variable values.
;;
;; I may yet return to it.
;;
;; For the moment, I want to try just keeping them in a code file, to see if I
;; like that better.

;;; Code:

(setq
 ;; I've been using Emacs since 2003.
 ;;
 ;; I don't need to see the splash screen or other displays of basic emacs
 ;; trivia.
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t)

;; I'd rather fit another few rows of buffer text than see menus and buttons.
(menu-bar-mode -1)
(tool-bar-mode -1)

(provide 'prefs)
;;; prefs.el ends here
