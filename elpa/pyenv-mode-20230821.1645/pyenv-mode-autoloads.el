;;; pyenv-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyenv-mode" "pyenv-mode.el" (0 0 0 0))
;;; Generated autoloads from pyenv-mode.el

(autoload 'pyenv-mode-set "pyenv-mode" "\
Set python shell VERSION.

\(fn VERSION)" t nil)

(autoload 'pyenv-mode-unset "pyenv-mode" "\
Unset python shell version." t nil)

(defvar pyenv-mode nil "\
Non-nil if Pyenv mode is enabled.
See the `pyenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyenv-mode'.")

(custom-autoload 'pyenv-mode "pyenv-mode" nil)

(autoload 'pyenv-mode "pyenv-mode" "\
Minor mode for pyenv interaction.

This is a minor mode.  If called interactively, toggle the `Pyenv
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyenv-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pyenv-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pyenv-mode" '("pyenv-mode-"))

;;;***

;;;### (autoloads nil nil ("pyenv-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyenv-mode-autoloads.el ends here
