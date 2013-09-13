;; Set up Python mode.
;; (This is rather involved, and in desperate need of an overhaul.)

;; I use python-mode.el, with the TQS-coloration patch applied.
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; When necessary, pull in auto-filling of comments.
;; I'd actually really like to get auto-filling of docstrings, wrapping at
;; 72 chars, but one step at a time.
(autoload 'comment-auto-fill "comment-auto-fill.el")

(autoload 'autopair-init "autopair-init.el")

(autoload 'auto-complete-init "auto-complete-init.el")

;; Load Jedi. Note that for this to work, you'll have to install Jedi in a
;; virtualenv, using the Makefile.
(autoload 'emacs-epc-init "emacs-epc-init.el") ;; Need epc for jedi.
(autoload 'jedi:setup "jedi" nil t)

;; Load my python-mode accessories only when python-mode kicks in.
(defun load-python-mode-accessories ()
  "Loads all the libraries/tools I want to have when I'm in python-mode."
  (smart-dash-mode t)
  (comment-auto-fill)
  (autopair-init)
  (auto-complete-init)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (emacs-epc-init)
  (setq jedi:setup-keys t)
  (jedi:setup)
  (setq jedi:complete-on-dot t))
(add-hook 'python-mode-hook 'load-python-mode-accessories)

(provide 'python-mode-init)
