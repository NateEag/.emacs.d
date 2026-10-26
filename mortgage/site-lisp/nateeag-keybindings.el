;;; nateeag-keybindings.el --- My custom keybindings. -*- lexical-binding: t; -*-

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; A very rough cut at using hydra to manage keybindings.
;;
;; It shows how to nest hydras, and that's all I'm looking for at the moment.

;;; Code:

(require 'hydra)

(defhydra nateeag/keybindings (:exit t)
  "Root hydra for my various custom keybindings.

Exists mainly because keybindings I don't use as often I sometimes
forget. Having a visual orientation to what's available helps me avoid
that."

  ("f" find-file "Open file")
  ("w" save-buffer "Save current buffer")
  ("p" (nateeag/projects/body) "Projects"))

(defhydra nateeag/projects (:exit t)
  "My keybindings for doing things in projects."
  ;; TODO: Figure out how I want to work with projects.
  ;;
  ;; For now, this is just meant to trigger some behaviors in a nested project...
  ("p" project-switch-project "Change project")
  ("b" project-switch-to-buffer "Change buffer in project"))

(provide 'nateeag-keybindings)
;;; nateeag-keybindings.el ends here
