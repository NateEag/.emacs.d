(defun nateeag-autoloads-init ()
  "Define Nate Eagleson's manually-maintained autoloads.

These are mostly for lazy-loading registration of mode hooks, but a few of them
are for modes that didn't come with autoloading."

  (autoload 'autopair-init "autopair-init.el")
  (autoload 'auto-complete-init "auto-complete-init.el")
  (autoload 'web-mode-init "web-mode-init.el")
  (autoload 'set-windows-env "set-windows-env.el")
  (autoload 'comment-auto-fill "comment-auto-fill.el")
  (autoload 'emacs-lisp-init "emacs-lisp-init.el")
  (autoload 'js-mode-init "js-mode-init.el")
  (autoload 'flymake-init "flymake-init.el")
  (autoload 'hs-minor-mode-init "hs-minor-mode-init.el")
  (autoload 'yasnippet-init "yasnippet-init.el")
  (autoload 'php-mode-init "php-mode-init.el")
  (autoload 'flymake-php-setup "flymake-php-setup.el")
  (autoload 'emacs-epc-init "emacs-epc-init.el") ;; Need epc for jedi.
  (autoload 'jedi:setup "jedi" nil t)

  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (autoload 'rst-mode "rst-mode.el")
  (autoload 'markdown-mode "markdown-mode.el")
  (autoload 'tern-mode "tern.el" nil t)
  ;; I use python-mode.el, with the TQS-coloration patch applied.
  ;; I should probably try installing the latest version and seeing how it
  ;; holds up.
  (autoload 'python-mode "python-mode" "Python editing mode." t))

(provide 'nateeag-autoloads-init)
