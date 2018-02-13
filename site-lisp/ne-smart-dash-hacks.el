;;; ne-smart-dash-hacks.el --- Some hacky extensions to smart-dash-mode.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; smart-dash-mode is awesome, but sometimes you need just a little more smarts
;; in a mode about when it should be active.
;;
;; Hence this thing.

;;; Code:

(defun ne-smart-dash-hacks-sh-mode-should-use-smart-dash ()
  ;; Check if we're in a bash var using thing-at-point-looking-at. If we're
  ;; not, then there's no need to use smart-dash.
  ;;
  ;; For a robust solution you would need a bash parser. I'm just trying to get
  ;; by on hardcoding a few special cases.
  (let ((bash-var-chars "[a-zA-Z0-9_]+"))
    (or (thing-at-point-looking-at (concat "\\(export\\w+\\)?"
                                           bash-var-chars
                                           "="))
        (thing-at-point-looking-at (concat "\\${?" bash-var-chars)))))

(defun ne-smart-dash-hacks-sh-mode-insert ()
  (interactive)
  (if (ne-smart-dash-hacks-sh-mode-should-use-smart-dash)
      (smart-dash-insert)
    (self-insert-command 1)))

(defun ne-smart-dash-hacks-sh-mode-install ()
  (set-minor-mode-key 'smart-dash-mode "-" 'ne-smart-dash-hacks-sh-mode-insert)
  (set-minor-mode-key 'smart-dash-mode (kbd "<kp-subtract>") 'ne-smart-dash-hacks-sh-mode-insert))

(provide 'ne-smart-dash-hacks)
;;; ne-smart-dash-hacks.el ends here
