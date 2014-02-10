;; flymake gives on-the-fly syntax/lint/style checking, based on the output of
;; an external compiler.
;; I'm hoping to set up several of my major modes to use it, long-term.
(require 'flymake)

;; Extension that lets you see flymake's output without using a mouse.
(eval-after-load 'flymake '(require 'flymake-cursor))
(setq flymake-cursor-auto-enable t)

(defun flymake-init ()
  (interactive)
  ;; Put flymake working files in a single location.
  (setq temporary-file-directory "~/.emacs.d/tmp/")
  (setq flymake-run-in-place nil)

  ;; Keep it from running too aggressively.
  (setq flymake-no-changes-timeout 3)
  (setq flymake-initialized t)

  ;; Turn it on.
  (flymake-mode 1)
  (flymake-cursor-mode 1))
