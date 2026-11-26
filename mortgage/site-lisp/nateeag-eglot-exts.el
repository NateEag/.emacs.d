;;; nateeag-eglot-exts --- My eglot extensions. -*- lexical-binding: t; -*-

;;; Author: Nate Eagleson

(defun nateeag/eglot-capf ()
  (setq-local completion-at-point-functions
	      (list (cape-capf-super
		     #'eglot-completion-at-point
		     #'yasnippet-capf))))

(provide 'nateeag-eglot-exts)
