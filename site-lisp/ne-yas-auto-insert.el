;;; ne-yas-auto-insert.el --- Auto-insert yasnippets on opening new files.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; A little wiring to hook up auto-insert-mode and yasnippet.
;;
;; I'm aware of yatemplate, as well as a few other approaches to this.
;;
;; Perhaps irrationally, I wanted to try my own approach, because it seems
;; simpler and more powerful to me, if it actually works out as I hope (it may
;; well not).
;;
;; The hope is that by using a variable to choose the targeted snippet, setting
;; skeletons can be as simple as setting the var in your mode hooks, while letting
;; you use .dir-locals.el to override the var per-folder, and still letting you
;; use auto-insert-mode in the standard way.

;;; Code:

(defun ne-yas-auto-insert-activate ()
  "Add an item that matches all files to `auto-insert-alist'.

TODO Consider activating `auto-insert-mode' in here, too?"

  (define-auto-insert "^.*$" '(lambda () (ne-yas-auto-insert-expand-snippet))))

(defun ne-yas-auto-insert-config ()
  "Set up auto-insert with the defaults I prefer.

If this became a real package, I should probably move this into my own
preferences."

  ;; Remove all existing entries, because I don't want the defaults.
  ;; TODO This is aggressive behavior. If I ever extract this into a proper
  ;; package, make this opt-in behavior.
  (setq auto-insert-alist nil))

(defvar ne-yas-auto-insert-snippet-name nil
  "TODO Explain this")

(defun ne-yas-auto-insert-expand-snippet ()
  "Expand the snippet specified by `ne-yas-auto-insert-snippet-name'.

If `ne-yas-auto-insert-snippet-name' is nil, do nothing."
  (if ne-yas-auto-insert-snippet-name
      (yas-expand-snippet
       (yas-lookup-snippet ne-yas-auto-insert-snippet-name))))

(provide 'ne-yas-auto-insert)
;;; ne-yas-auto-insert.el ends here
