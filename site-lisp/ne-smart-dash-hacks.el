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

    ;; If it contains dashes already (especially if it starts with one), you
    ;; probably shouldn't underscore.
    (and (not (thing-at-point-looking-at "[a-zA-Z0-9]*--?[a-zA-Z0-9]+"))
         ;; However, if it doesn't, there may be several good reasons *to* use
         ;; underscores...
         (or
          ;; Export statements are for variable names, which should underscore.
          (thing-at-point-looking-at (concat "export[[:space:]]+"
                                             bash-var-chars))

          ;; Vanilla variable assignments should underscore.
          ;;
          ;; This only works for typing new vars if you start with '=' then
          ;; stick the varname in before it, which is why
          ;; ne-smart-dash-hacks-sh-mode-insert exists. You have to do
          ;; something like that, though, if you want dashes to be used in
          ;; command names, because otherwise you can't distinguish between "I
          ;; am typing a command" and "I am typing a variable name."
          (thing-at-point-looking-at (concat "[[:space:]]*" bash-var-chars "*="))

          ;; Return true when editing an established variable name. Alas,
          ;; smart-dash-mode itself seems to return a - in that case.
          (thing-at-point-looking-at (concat "[[:space:]]*" bash-var-chars "*=.*"))

          (thing-at-point-looking-at (concat "\\${?" bash-var-chars))
          ;; If thing-at-point has an underscore in it, we should probably keep
          ;; using underscores. Generally, bash commands and inputs lean
          ;; towards dashes, but sometimes they have underscores. I have seen a
          ;; convention where tab completion functions begin with an underscore
          ;; then use dashes everywhere else, so their names align with the
          ;; command they offer completion for, which is why this doesn't
          ;; accept an underscore as the first character.
          (thing-at-point-looking-at "[:space:]+[a-zA-Z0-9]+_+[a-zA-Z0-9_]*")))))

;; TODO Maybe snippetize this, so it's easy to write the assignment half once
;; you've typed your varname?
(defun ne-smart-dash-hacks-sh-mode-insert ()
  (interactive)
  (if (ne-smart-dash-hacks-sh-mode-should-use-smart-dash)
      (smart-dash-insert)
    (self-insert-command 1)))

(defun ne-smart-dash-hacks-sh-mode-install ()
  (set-minor-mode-key 'smart-dash-mode "-" 'ne-smart-dash-hacks-sh-mode-insert)
  (set-minor-mode-key 'smart-dash-mode (kbd "<kp-subtract>") 'ne-smart-dash-hacks-sh-mode-insert)

  ;; FIXME Add the hack function to ac-trigger-commands-on-completing, so
  ;; typing underscore with it doesn't terminate completion. For the moment I
  ;; just put it in custom.el.
  )

(provide 'ne-smart-dash-hacks)
;;; ne-smart-dash-hacks.el ends here
