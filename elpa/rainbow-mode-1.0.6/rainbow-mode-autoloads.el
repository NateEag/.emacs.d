;;; rainbow-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rainbow-mode" "rainbow-mode.el" (0 0 0 0))
;;; Generated autoloads from rainbow-mode.el

(autoload 'rainbow-mode "rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

This is a minor mode.  If called interactively, toggle the
`Rainbow mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rainbow-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rainbow-mode" '("rainbow-"))

;;;***

;;;### (autoloads nil nil ("rainbow-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-mode-autoloads.el ends here
