;; Set up yasnippet per my standard requirements.
;; Mostly exists to load yasnippet only on demand.

(defvar yasnippet-config-run nil)
(defun yasnippet-config ()
  "Once-and-done yasnippet configuration."

  (if (not yasnippet-config-run)
      (progn
        ;; Set up all snippet dirs to lazy-load.
        (yas-reload-all)
        ;; GRIPE For reasons I don't understand, I need this invocation in
        ;; order to avoid a never-ending recursion of defining keybindings. I
        ;; think it's some interaction between yasnippet and auto-complete, but
        ;; I'm not really sure.
        (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
        (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
        (define-key yas-minor-mode-map [(tab)] nil)

        (setq yasnippet-config-run t))))

(defun yasnippet-init ()
  "Get yasnippet set up the way I like."

  (diminish 'yas-minor-mode)
  (yasnippet-config))
