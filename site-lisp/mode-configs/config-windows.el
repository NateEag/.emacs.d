;;; config-windows.el --- miscellaneous Windows setup.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I dislike Windows very much, but in the end, software is about the users.
;; Sometimes you need to work there.
;;
;; Here I endeavor to make it less painful.

;;; Code:

(defun set-windows-env ()
  "Set any environment tweaks needed for running on Windows."

  (interactive)

  ;; Add git bash binaries to path.
  (if (file-directory-p "c:/Program Files (x86)/Git/bin")
      (add-to-list 'exec-path "c:/Program Files (x86)/Git/bin"))

  ;; This is where PHP lives on my work machine.
  (setq php-executable "/Program Files (x86)/iis express/PHP/v5.4/php")

  ;; Teach Windows about Cygwin paths.
  (cygwin-mount-activate)

  (setq tramp-default-method "plink"))

;;; config-windows.el ends here
