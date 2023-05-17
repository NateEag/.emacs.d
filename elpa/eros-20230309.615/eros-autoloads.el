;;; eros-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eros" "eros.el" (0 0 0 0))
;;; Generated autoloads from eros.el

(autoload 'eros-eval-last-sexp "eros" "\
Wrapper for `eval-last-sexp' that overlays results.

\(fn EVAL-LAST-SEXP-ARG-INTERNAL)" t nil)

(autoload 'eros-eval-defun "eros" "\
Wrapper for `eval-defun' that overlays results.

\(fn EDEBUG-IT)" t nil)

(defvar eros-mode nil "\
Non-nil if Eros mode is enabled.
See the `eros-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eros-mode'.")

(custom-autoload 'eros-mode "eros" nil)

(autoload 'eros-mode "eros" "\
Display Emacs Lisp evaluation results overlays.

This is a minor mode.  If called interactively, toggle the `Eros
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='eros-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "eros" '("eros-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eros-autoloads.el ends here
