;;; tide-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tide" "tide.el" (0 0 0 0))
;;; Generated autoloads from tide.el

(autoload 'company-tide "tide" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'tide-format-before-save "tide" "\
Before save hook to format the buffer before each save." t nil)

(autoload 'tide-format "tide" "\
Format the current region or buffer." t nil)

(autoload 'tide-setup "tide" "\
Setup `tide-mode' in current buffer." t nil)

(autoload 'tide-mode "tide" "\
Minor mode for Typescript Interactive Development Environment.

This is a minor mode.  If called interactively, toggle the `tide
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tide-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{tide-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'tide-project-errors "tide" nil t nil)

(autoload 'tide-unhighlight-identifiers "tide" "\
Remove highlights from previously highlighted identifier." nil nil)

(autoload 'tide-hl-identifier "tide" "\
Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier." t nil)

(autoload 'tide-hl-identifier-mode "tide" "\
Highlight instances of the identifier at point after a short
timeout.

This is a minor mode.  If called interactively, toggle the
`Tide-Hl-Identifier mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `tide-hl-identifier-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "tide" '("tide-" "xref-tide-xref-backend"))

;;;***

;;;### (autoloads nil "tide-lv" "tide-lv.el" (0 0 0 0))
;;; Generated autoloads from tide-lv.el

(register-definition-prefixes "tide-lv" '("tide-lv-"))

;;;***

;;;### (autoloads nil nil ("tide-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tide-autoloads.el ends here
