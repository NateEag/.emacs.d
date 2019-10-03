;;; show-eol-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "show-eol" "show-eol.el" (0 0 0 0))
;;; Generated autoloads from show-eol.el

(autoload 'show-eol-mode "show-eol" "\
Minor mode 'show-eol-mode'.

If called interactively, enable Show-Eol mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar global-show-eol-mode nil "\
Non-nil if Global Show-Eol mode is enabled.
See the `global-show-eol-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-show-eol-mode'.")

(custom-autoload 'global-show-eol-mode "show-eol" nil)

(autoload 'global-show-eol-mode "show-eol" "\
Toggle Show-Eol mode in all buffers.
With prefix ARG, enable Global Show-Eol mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Eol mode is enabled in all buffers where
`show-eol-turn-on-show-eol-mode' would do it.
See `show-eol-mode' for more information on Show-Eol mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "show-eol" '("show-eol-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; show-eol-autoloads.el ends here
