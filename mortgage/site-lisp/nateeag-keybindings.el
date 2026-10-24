;;; nateeag-keybindings.el --- My custom keybindings. -*- lexical-binding: t; -*-

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; A very rough cut at using hydra to manage keybindings.
;;
;; It shows how to nest hydras, and that's all I'm looking for at the moment.

;;; Code:

(defhydra nateeag/keybindings (:exit t)
  "keybindings"
  ("f" find-file "Open file")
  ("w" save-buffer "Save current buffer")
  ("p" (nateeag/projects/body) "Projects")
)

(defhydra nateeag/projects ()
  "My keybindings for doing things in projects."
  ;; TODO: Figure out how I want to work with projects.
  ;;
  ;; For now, this is just meant to trigger some behaviors in a nested project...
  ("p" project-switch-project "Change project")
  ("b" project-switch-to-buffer "Change buffer in project"))

(provide 'nateeag-keybindings)
;;; nateeag-keybindings.el ends here
