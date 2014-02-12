;; Set up yasnippet per my standard requirements.
;; Mostly exists to load yasnippet only on demand.

(defvar yasnippet-config-run nil)
(defun yasnippet-config ()
  "Once-and-done yasnippet configuration."

  (if (not yasnippet-config-run)
      (progn
        ;; Load all snippets.
        ;; GRIPE It would be nice to lazy-load these. I think there's a package
        ;; for that somewhere...
        (dolist (elt yas-snippet-dirs)
          (yas-load-directory elt))

        ;; GRIPE For reasons I don't understand, I need this invocation in
        ;; order to avoid a never-ending recursion of defining keybindings. I
        ;; think it's some interaction between yasnippet and auto-complete, but
        ;; I'm not really sure.
        (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
        (define-key yas-minor-mode-map [(tab)] nil)

        (setq yasnippet-config-run t))))

(defun yasnippet-init ()
  "Load yasnippet and get things set up the way I like."

  (yas-minor-mode)
  (diminish 'yas-minor-mode)
  (yasnippet-config))
