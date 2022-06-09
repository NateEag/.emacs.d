;;; activity-watch-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "activity-watch-mode" "activity-watch-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from activity-watch-mode.el

(autoload 'activity-watch-refresh-project-name "activity-watch-mode" "\
Recompute the name of the project for the current file." t nil)

(autoload 'activity-watch-mode "activity-watch-mode" "\
Toggle Activity-Watch (Activity-Watch mode).

This is a minor mode.  If called interactively, toggle the
`activity-watch mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `activity-watch-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-activity-watch-mode 'globalized-minor-mode t)

(defvar global-activity-watch-mode nil "\
Non-nil if Global Activity-Watch mode is enabled.
See the `global-activity-watch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-activity-watch-mode'.")

(custom-autoload 'global-activity-watch-mode "activity-watch-mode" nil)

(autoload 'global-activity-watch-mode "activity-watch-mode" "\
Toggle Activity-Watch mode in all buffers.
With prefix ARG, enable Global Activity-Watch mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Activity-Watch mode is enabled in all buffers where `(lambda nil
\(activity-watch-mode 1))' would do it.

See `activity-watch-mode' for more information on Activity-Watch mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "activity-watch-mode" '("activity-watch-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; activity-watch-mode-autoloads.el ends here
