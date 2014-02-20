(defun emacs-lisp-init ()
  "My emacs lisp editing config."

  (smartparens-mode t)
  (comment-auto-fill)
  (elisp-slime-nav-mode t)
  (diminish 'elisp-slime-nav-mode)
  (auto-complete-mode t)
  (ac-emacs-lisp-mode-setup)
  (eldoc-mode t))
