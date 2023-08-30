;;; envrc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "envrc" "envrc.el" (0 0 0 0))
;;; Generated autoloads from envrc.el

(autoload 'envrc-mode "envrc" "\
A local minor mode in which env vars are set by direnv.

This is a minor mode.  If called interactively, toggle the `Envrc
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `envrc-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'envrc-global-mode 'globalized-minor-mode t)

(defvar envrc-global-mode nil "\
Non-nil if Envrc-Global mode is enabled.
See the `envrc-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `envrc-global-mode'.")

(custom-autoload 'envrc-global-mode "envrc" nil)

(autoload 'envrc-global-mode "envrc" "\
Toggle Envrc mode in all buffers.
With prefix ARG, enable Envrc-Global mode if ARG is positive; otherwise,
disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Envrc mode is enabled in all buffers where `(lambda nil (unless (or
\(minibufferp) (file-remote-p default-directory)) (envrc-mode 1)))' would do
it.

See `envrc-mode' for more information on Envrc mode.

\(fn &optional ARG)" t nil)

(autoload 'envrc-file-mode "envrc" "\
Major mode for .envrc files as used by direnv.
\\{envrc-file-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.envrc\\'" . envrc-file-mode))

(register-definition-prefixes "envrc" '("envrc-"))

;;;***

;;;### (autoloads nil nil ("envrc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; envrc-autoloads.el ends here
