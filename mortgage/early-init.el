;;; early-init.el --- early initialization options.  -*- lexical-binding: t; -*-

;;; Author:

;;; Version:

;;; Commentary:

;;

;;; Code:

;; elpaca.el does not like to have package.el enabled.
(setq package-enable-at-startup nil)

;; If something goes wrong during startup, let's debug it.
(setq debug-on-error t)

;; Make evil stop whining at me about not configuring this soon enough.
(setq evil-want-keybinding nil)

;;; early-init.el ends here
