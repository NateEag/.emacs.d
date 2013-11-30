;; Set up yasnippet per my standard requirements.
;; Mostly exists to load yasnippet only on demand.

(defvar yasnippet-config-run nil)
(defun yasnippet-config ()
  "Once-and-done yasnippet configuration."

  (if (not yasnippet-config-run)
      (progn
        ;; old-school snippets, because it has the php-mode snippets.
        (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
        (yas-load-directory "~/.emacs.d/snippets")

        (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets")
        (yas-load-directory "~/.emacs.d/my-snippets")

        ;; Change default keybinding, because I usually want to use YASnippet
        ;; through auto-complete.el.
        (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
        (define-key yas-minor-mode-map (kbd "TAB") nil)
        (define-key yas-minor-mode-map [(tab)] nil)

        (setq yasnippet-config-run t))))

(defun yasnippet-init ()
  "Load yasnippet and get things set up the way I like."

  (require 'yasnippet)

  (yasnippet-config)
  (yas-minor-mode)
  (diminish 'yas-minor-mode))
