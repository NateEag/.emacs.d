;; Set up yasnippet per my standard requirements.
;; Mostly exists to load yasnippet only on demand.

(defvar yasnippet-config-run nil)
(defun yasnippet-config ()
  "Once-and-done yasnippet configuration."

  (if (not yasnippet-config-run)
      (progn
        (setq yas-root-directory "~/.emacs.d/snippets")
        (add-to-list 'load-path
              "~/.emacs.d/yasnippet")
        (yas-load-directory yas-root-directory)
        (setq yasnippet-config-run t)
        (setq yas-trigger-symbol (kbd "C-c y"))
        ;(define-key yas-minor-mode-map (kbd "C-c y") 'yas-trigger-symbol)
        (define-key yas-minor-mode-map (kbd "TAB") nil)
        (define-key yas-minor-mode-map [(tab)] nil)
        )))

(defun yasnippet-init ()
  "Load yasnippet and get things set up the way I like."

  (require 'yasnippet)

  (yasnippet-config))
