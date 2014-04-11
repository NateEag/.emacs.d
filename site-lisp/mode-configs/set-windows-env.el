;; Windows-specific tweaks to Emacs' environment.

(defun set-windows-env ()
  "Set any environment tweaks needed for running on Windows."

  (interactive)

  ;; Add git bash binaries to path.
  (if (file-directory-p "c:/Program Files (x86)/Git/bin")
      (add-to-list 'exec-path "c:/Program Files (x86)/Git/bin"))

  ;; This is where PHP lives on my work machine.
  (setq php-executable "/Program Files (x86)/iis express/PHP/v5.4/php"))
