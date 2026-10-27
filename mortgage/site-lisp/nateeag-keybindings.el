;;; nateeag-keybindings.el --- My custom keybindings. -*- lexical-binding: t; -*-

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; A very rough cut at using hydra to manage keybindings.
;;
;; It shows how to nest hydras, and that's all I'm looking for at the moment.

;;; Code:

(require 'hydra)

;; Exists mainly because keybindings I don't use as often I sometimes
;; forget. Having a visual orientation to what's available helps me
;; avoid that.
(defhydra nateeag/keybindings (:exit t)
  "My custom keybindings"


  ("f" (nateeag/hydras/buffers/body) "General buffer / file operations")
  ("e" (nateeag/hydras/elisp/body) "Emacs Lisp")
  ;; Bound to g for obvious historical reasons, but might someday grow
  ;; beyond strictly working with Git.
  ("g" (nateeag/hydras/vcs/body) "Version control")
  ("j" (nateeag/hydras/jump/body) "Visual jump")
  ("p" (nateeag/hydras/projects/body) "Projects"))

(defhydra nateeag/hydras/buffers (:exit t)
  "Buffer / file ops"
  ("f" find-file "Open file")
  ("w" save-buffer "Save current buffer"))

(defhydra nateeag/hydras/elisp (:exit t)
  "Elisp operations"
  ("e" eval-buffer "Eval full buffer")
  ("d" eval-defun "Eval current defun")
  ("s" eval-last-sexp "Eval preceding sexp"))

(defhydra nateeag/hydras/projects (:exit t)
  "Project ops"
  ;; TODO: Figure out how I want to work with projects.
  ;;
  ;; These are placeholders for some core operations, but I probably want Counsel or similar.
  ("p" project-switch-project "Change project")
  ("b" project-switch-to-buffer "Change buffer in project"))

(defhydra nateeag/hydras/vcs (:exit t)
  "Version control shortcuts"
  ("g" magit-status "Current project's status."))

(defhydra nateeag/hydras/jump (:exit t)
  "Jump!"
  ("w" evil-avy-goto-word-1 "Jump to word by initial character."))

(provide 'nateeag-keybindings)
;;; nateeag-keybindings.el ends here
