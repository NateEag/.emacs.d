;;; sidecar-locals-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sidecar-locals" "sidecar-locals.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from sidecar-locals.el

(autoload 'sidecar-locals-report "sidecar-locals" "\
Report paths that are used to detect locals.

This creates a buffer with links that visit that file." t nil)

(defvar sidecar-locals-mode nil "\
Non-nil if Sidecar-Locals mode is enabled.
See the `sidecar-locals-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sidecar-locals-mode'.")

(custom-autoload 'sidecar-locals-mode "sidecar-locals" nil)

(autoload 'sidecar-locals-mode "sidecar-locals" "\
Toggle variable `sidecar-locals-mode' in the current buffer.

This is a minor mode.  If called interactively, toggle the
`Sidecar-Locals mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='sidecar-locals-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "sidecar-locals" '("sidecar-locals-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sidecar-locals-autoloads.el ends here
