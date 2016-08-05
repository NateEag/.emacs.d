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

  ;; On Windows7, this is the shortcut for moving between apps in the taskbar.
  ;; I don't see much point in hearing about how it does nothing when I leave
  ;; Emacs, so I bind it to a no-op to suppress that message.
  (global-set-key (kbd "C-<rwindow>") 'ignore)
  (global-set-key (kbd "C-<lwindow>") 'ignore)

  ;; Teach Windows about Cygwin paths.
  (cygwin-mount-activate)

  (setq tramp-default-method "plink"))

(provide 'config-windows)
;;; config-windows.el ends here
