;;; set-minor-mode-key.el --- Override minor-mode keys in specific major modes.

;;; Author: 'crowding' on Stack Overflow

;;; Version: 0.0.1

;;; Commentary:
;;;
;;; I just yanked this from here and wanted a convenient place to autoload it
;;; from, since I've discovered I want it in two places now:
;;;
;;; http://stackoverflow.com/a/14769115

;;

;;; Code:
(defun set-minor-mode-key (mode key def)
  "Override a minor mode keybinding for the local buffer.

Stores keymaps stored in buffer-local `minor-mode-overriding-map-alist'."

  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))

(provide 'set-minor-mode-key)
;;; set-minor-mode-key.el ends here
