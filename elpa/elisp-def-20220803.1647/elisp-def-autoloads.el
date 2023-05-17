;;; elisp-def-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-def" "elisp-def.el" (0 0 0 0))
;;; Generated autoloads from elisp-def.el

(autoload 'elisp-def "elisp-def" "\
Go to the definition of the symbol at point." t nil)

(autoload 'elisp-def-mode "elisp-def" "\
Minor mode for finding definitions with `elisp-def'.

This is a minor mode.  If called interactively, toggle the
`Elisp-Def mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `elisp-def-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{elisp-def-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "elisp-def" '("elisp-def-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-def-autoloads.el ends here
