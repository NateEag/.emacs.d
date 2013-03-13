(autoload 'autopair-init "autopair-init.el")

(autoload 'comment-auto-fill "comment-auto-fill.el")

(defun emacs-lisp-init ()
  "My emacs lisp editing config."
  (autopair-init)
  (comment-auto-fill))
