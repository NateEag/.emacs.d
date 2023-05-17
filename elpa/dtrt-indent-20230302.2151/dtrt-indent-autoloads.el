;;; dtrt-indent-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dtrt-indent" "dtrt-indent.el" (0 0 0 0))
;;; Generated autoloads from dtrt-indent.el

(autoload 'dtrt-indent-mode "dtrt-indent" "\
Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

This is a minor mode.  If called interactively, toggle the
`dtrt-indent mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dtrt-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently.

\(fn &optional ARG)" t nil)

(put 'dtrt-indent-global-mode 'globalized-minor-mode t)

(defvar dtrt-indent-global-mode nil "\
Non-nil if Dtrt-Indent-Global mode is enabled.
See the `dtrt-indent-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dtrt-indent-global-mode'.")

(custom-autoload 'dtrt-indent-global-mode "dtrt-indent" nil)

(autoload 'dtrt-indent-global-mode "dtrt-indent" "\
Toggle Dtrt-Indent mode in all buffers.
With prefix ARG, enable Dtrt-Indent-Global mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dtrt-Indent mode is enabled in all buffers where `(lambda nil (when
\(derived-mode-p 'prog-mode 'text-mode 'javascript-mode) (dtrt-indent-mode)))'
would do it.

See `dtrt-indent-mode' for more information on Dtrt-Indent mode.

\(fn &optional ARG)" t nil)

(defvar dtrt-indent-mode nil "\
Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'.")

(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)

(register-definition-prefixes "dtrt-indent" '("dtrt-indent-"))

;;;***

;;;### (autoloads nil "dtrt-indent-diag" "dtrt-indent-diag.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from dtrt-indent-diag.el

(register-definition-prefixes "dtrt-indent-diag" '("dtrt-indent-" "save-buffer-state"))

;;;***

;;;### (autoloads nil nil ("dtrt-indent-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dtrt-indent-autoloads.el ends here
