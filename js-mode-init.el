;; My JavaScript mode setup.

(require 'smart-dash)

(add-to-list 'load-path "~/.emacs.d/lintnode")
(require 'flymake-jslint)

(autoload 'flymake-init "flymake-init.el")

;; DEBUG When this is uncommented, flymake-lintnode works the first time I load
;; it. When it isn't, it fails the first time. I suspect this means that the
;; first time, it's trying to call the node.js process before it's finished
;; starting up.
;;(setq flymake-log-level 3)
(setq lintnode-location "~/.emacs.d/lintnode")
(setq lintnode-autostart t)

(defun js-mode-init ()
  (interactive)
  (smart-dash-mode t)
  (comment-auto-fill)
  (autopair-init)
  (auto-complete-init)
  (flymake-init)
  (lintnode-hook))
