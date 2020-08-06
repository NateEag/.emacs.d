;;; ne-package-init.el --- Initialize package.el for my preferred usage.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just some basic setup of package.el.
;;
;; I factored it out of bootstrap.el to make it easier to reproduce problems
;; and conflicts in packages without loading my personal setup, so it's easier
;; to verify whether it's some of my code causing the problem or if the
;; packages as-is conflict.

;;; Code:
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.org/packages/"))
(package-initialize)

(provide 'ne-package-init)
;;; ne-package-init.el ends here
