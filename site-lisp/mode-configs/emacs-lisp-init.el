(defun emacs-lisp-init ()
  "My emacs lisp editing config."

  (smartparens-init)
  (comment-auto-fill)
  (elisp-slime-nav-mode)
  (diminish 'elisp-slime-nav-mode)
  (auto-complete-init)
  (ac-emacs-lisp-mode-setup)
  (eldoc-mode t))
