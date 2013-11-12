;; My JavaScript mode setup.

(require 'smart-dash)

(setq lintnode-location "~/.emacs.d/lintnode")
(setq lintnode-autostart t)

(add-to-list 'load-path "~/.emacs.d/lintnode")
(require 'flymake-jslint)

(add-to-list 'load-path "~/.emacs.d/site-lisp/tern-mode/emacs")
(autoload 'tern-mode "tern.el" nil t)

;; GRIPE Starting lintnode here is a lame hack and guaranteed to fail if
;; node.js is not installed, but it solves the problem of the lintnode server
;; not starting in time for the first JS buffer opened to work.
(lintnode-start)

(autoload 'flymake-init "flymake-init.el")

(defun js-mode-init ()
  (interactive)
  (smart-dash-mode t)
  (comment-auto-fill)
  (autopair-init)
  (auto-complete-init)
  (flymake-init)
  (lintnode-hook)
  (tern-mode 't)
  (skewer-mode)
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))
