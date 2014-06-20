;; Set up yasnippet per my standard requirements.
;; Mostly exists to load yasnippet only on demand.

(defvar yasnippet-config-run nil)
(defun yasnippet-config ()
  "Once-and-done yasnippet configuration."

  (if (not yasnippet-config-run)
      (progn
        ;; Set up all snippet dirs to lazy-load.
        (yas-reload-all)

        ;; Give keys_with_underscores priority over non-underscored-keys.
        ;; This lets things like require_once in php-mode not be overridden by
        ;; 'once' from cc-mode, php-mode's parent mode.
        (setq yas-key-syntaxes (list "w_" "w" "w_." "w_.()" "^ "))

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
