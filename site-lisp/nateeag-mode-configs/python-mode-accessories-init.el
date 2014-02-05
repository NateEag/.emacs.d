;; Set up Python mode.

;; Load my python-mode accessories only when python-mode kicks in.
(defun python-mode-accessories-init ()
  "Loads all the libraries/tools I want to have when I'm in python-mode."
  (smart-dash-mode t)
  (comment-auto-fill)
  (autopair-init)
  (auto-complete-init)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))
  (emacs-epc-init)

  ;; Initialize Jedi. Note that for this to work, you'll have to install Jedi in a
  ;; virtualenv, using the Makefile.
  (setq jedi:setup-keys t)
  (jedi:setup)
  (setq jedi:complete-on-dot t))

(provide 'python-mode-accessories-init)
