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
        (setq yasnippet-config-run t))))

(defun yasnippet-init ()
  "Load yasnippet and get things set up the way I like."

  (require 'yasnippet)

  (yasnippet-config))
