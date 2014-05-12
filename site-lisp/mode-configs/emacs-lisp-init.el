(defun emacs-lisp-init ()
  "My emacs lisp editing config."

  (my-prog-mode-init)

  (elisp-slime-nav-mode t)
  (diminish 'elisp-slime-nav-mode)
  (ac-emacs-lisp-mode-setup)
  (eldoc-mode t))
