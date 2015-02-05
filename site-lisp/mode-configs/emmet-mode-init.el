(defun emmet-mode-init ()
  "Configure emmet-mode the way I like it."

  ;; Don't override the built-in C-j binding, because I use it.
  ;; I set up my own keybindings for emmet in my keybindings file.
  (define-key emmet-mode-keymap (kbd "C-j") nil)

  ;; Use yasnippet for expansion.
  ;; Ideally, I'd bind some handle-it-all function to Tab that tries
  ;; emmet expansion if emmet is active, then yasnippet expansion if it's
  ;; active, then auto-complete if it's active, and indents if nothing else
  ;; succeeded.
  (yas-minor-mode t)

  (eval-after-load "auto-complete" '(require 'ac-emmet))

  (diminish 'emmet-mode))
