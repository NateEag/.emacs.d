;;; edebug-inline-result-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edebug-inline-result" "edebug-inline-result.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from edebug-inline-result.el

(defvar edebug-inline-result-mode nil "\
Non-nil if Edebug-Inline-Result mode is enabled.
See the `edebug-inline-result-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `edebug-inline-result-mode'.")

(custom-autoload 'edebug-inline-result-mode "edebug-inline-result" nil)

(autoload 'edebug-inline-result-mode "edebug-inline-result" "\
A minor mode that show Edebug result with inline style.

This is a minor mode.  If called interactively, toggle the
`Edebug-Inline-Result mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='edebug-inline-result-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "edebug-inline-result" '("edebug-inline-result-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edebug-inline-result-autoloads.el ends here
