;;; show-eol-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "show-eol" "show-eol.el" (0 0 0 0))
;;; Generated autoloads from show-eol.el

(autoload 'show-eol-mode "show-eol" "\
Minor mode `show-eol-mode'.

This is a minor mode.  If called interactively, toggle the
`Show-Eol mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `show-eol-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-show-eol-mode 'globalized-minor-mode t)

(defvar global-show-eol-mode nil "\
Non-nil if Global Show-Eol mode is enabled.
See the `global-show-eol-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-show-eol-mode'.")

(custom-autoload 'global-show-eol-mode "show-eol" nil)

(autoload 'global-show-eol-mode "show-eol" "\
Toggle Show-Eol mode in all buffers.
With prefix ARG, enable Global Show-Eol mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Show-Eol mode is enabled in all buffers where `show-eol-turn-on-show-eol-mode'
would do it.

See `show-eol-mode' for more information on Show-Eol mode.

\(fn &optional ARG)" t nil)

(autoload 'show-eol-get-current-system "show-eol" "\
Return the current system name." nil nil)

(autoload 'show-eol-get-eol-mark-by-system "show-eol" "\
Return the EOL mark string by system type." nil nil)

(register-definition-prefixes "show-eol" '("show-eol-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; show-eol-autoloads.el ends here
