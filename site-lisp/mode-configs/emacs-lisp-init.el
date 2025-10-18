;;; emacs-lisp-init.el -- configure emacs-lisp-mode the way I like.

;;; Commentary:
;;;
;;; Defines a hook function suitable for use as an emacs-lisp-mode-hook.

;;; Code:
(defun emacs-lisp-init ()
  "Hook function for `emacs-lisp-mode'."

  (my-prog-mode-init)

  (setq mode-name "elisp")

  (turn-on-elisp-slime-nav-mode)
  (diminish 'elisp-slime-nav-mode)
  (ac-emacs-lisp-mode-setup)
  (setq-local ne-yas-auto-insert-snippet-name "package")

  (push '(?\` . ("`" . "'")) evil-surround-pairs-alist)

  (setq-local apheleia-formatter 'lisp-indent)

  (eldoc-mode t)
  (diminish 'eldoc-mode))
;;; emacs-lisp-init.el ends here
