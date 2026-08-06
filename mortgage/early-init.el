;;; early-init.el --- early initialization options.

;;; Author:

;;; Version:

;;; Commentary:

;;

;;; Code:

;; elpaca.el does not like to have package.el enabled.
(setq package-enable-at-startup nil)

;; If something goes wrong during startup, let's debug it.
(setq debug-on-error t)

;;; early-init.el ends here
