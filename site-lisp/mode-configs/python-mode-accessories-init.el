;; Set up Python mode.

;; Load my python-mode accessories only when python-mode kicks in.
(defun python-mode-accessories-init ()
  "Loads all the libraries/tools I want to have when I'm in python-mode."

  (my-prog-mode-init)

  ;; Initialize Jedi. Note that for this to work, you'll have to install Jedi
  ;; in a virtualenv, using the Makefile.
  (setq jedi:use-shortcuts t)
  (setq jedi:complete-on-dot t))

(provide 'python-mode-accessories-init)
