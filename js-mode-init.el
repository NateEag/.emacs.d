;; My JavaScript mode setup.

(require 'smart-dash)

(defun js-mode-init ()
  (interactive)
  (smart-dash-mode t)
  (comment-auto-fill)
  (autopair-init)
  (auto-complete-init))
