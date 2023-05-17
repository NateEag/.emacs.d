;;; prettier-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prettier" "prettier.el" (0 0 0 0))
;;; Generated autoloads from prettier.el

(put 'prettier-mode-sync-config-flag 'safe-local-variable 'booleanp)

(put 'prettier-editorconfig-flag 'safe-local-variable 'booleanp)

(put 'prettier-infer-parser-flag 'safe-local-variable 'booleanp)

(put 'prettier-prettify-on-save-flag 'safe-local-variable 'booleanp)

(put 'prettier-diff-timeout-seconds 'safe-local-variable 'numberp)

(put 'prettier-diff-edit-cost 'safe-local-variable 'natnump)

(put 'prettier-parsers 'safe-local-variable 'listp)

(autoload 'prettier-prettify "prettier" "\
Prettify the whole current buffer, or the part it is narrowed to.

With prefix, ask for the parser to use" t nil)

(autoload 'prettier-prettify-region "prettier" "\
Prettify the current region.

With prefix, ask for the parser to use" t nil)

(autoload 'prettier-mode "prettier" "\
Sync Prettier settings and format on file save.

This is a minor mode.  If called interactively, toggle the
`Prettier mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `prettier-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

For more information see Info node `(prettier)Top'.

\(fn &optional ARG)" t nil)

(put 'global-prettier-mode 'globalized-minor-mode t)

(defvar global-prettier-mode nil "\
Non-nil if Global Prettier mode is enabled.
See the `global-prettier-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-prettier-mode'.")

(custom-autoload 'global-prettier-mode "prettier" nil)

(autoload 'global-prettier-mode "prettier" "\
Toggle Prettier mode in all buffers.
With prefix ARG, enable Global Prettier mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Prettier mode is enabled in all buffers where
`prettier--turn-on-if-appropriate' would do it.

See `prettier-mode' for more information on Prettier mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "prettier" '("prettier-"))

;;;***

;;;### (autoloads nil nil ("prettier-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prettier-autoloads.el ends here
