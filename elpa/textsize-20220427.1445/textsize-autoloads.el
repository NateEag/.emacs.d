;;; textsize-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "textsize" "textsize.el" (0 0 0 0))
;;; Generated autoloads from textsize.el

(autoload 'textsize-modify-manual-adjust "textsize" "\
Adjust FRAME's font-point adjustment by OFFSET persistently.

Add a custom fixed offset to the textsize point size calculation.

If OFFSET is nil, reset adjustment to zero.

\(fn FRAME OFFSET)" nil nil)

(autoload 'textsize-increment "textsize" "\
Increment the current frame's automatic text size." t nil)

(autoload 'textsize-decrement "textsize" "\
Decrement the current frame's automatic text size." t nil)

(autoload 'textsize-reset "textsize" "\
Reset the adjustment on the current frame's automatic text size." t nil)

(autoload 'textsize-fix-frame "textsize" "\
Set the default text size appropriately for FRAME display.

\(fn &optional FRAME)" t nil)

(defvar textsize-mode nil "\
Non-nil if Textsize mode is enabled.
See the `textsize-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `textsize-mode'.")

(custom-autoload 'textsize-mode "textsize" nil)

(autoload 'textsize-mode "textsize" "\
Adjusts the default text size for the size and pixel pitch of the display.

This is a minor mode.  If called interactively, toggle the
`Textsize mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='textsize-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "textsize" '("textsize-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; textsize-autoloads.el ends here
