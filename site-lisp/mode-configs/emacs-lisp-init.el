(defun emacs-lisp-init ()
  "My emacs lisp editing config."

  (my-prog-mode-init)

  (turn-on-elisp-slime-nav-mode)
  (diminish 'elisp-slime-nav-mode)
  (ac-emacs-lisp-mode-setup)
  (eldoc-mode t))
