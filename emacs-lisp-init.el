(defun emacs-lisp-init ()
  "My emacs lisp editing config."
  (autopair-init)
  (comment-auto-fill)
  (elisp-slime-nav-mode)
  (diminish 'elisp-slime-nav-mode))
