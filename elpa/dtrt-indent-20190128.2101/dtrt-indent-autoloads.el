;;; dtrt-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dtrt-indent" "dtrt-indent.el" (23641 39403
;;;;;;  0 0))
;;; Generated autoloads from dtrt-indent.el

(autoload 'dtrt-indent-mode "dtrt-indent" "\
Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation offset
and `indent-tabs-mode' will be guessed for newly opened files and
adjusted transparently.

\(fn &optional ARG)" t nil)

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
With prefix ARG, enable Dtrt-Indent-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Dtrt-Indent mode is enabled in all buffers where
`(lambda nil (when (derived-mode-p (quote prog-mode) (quote text-mode)) (dtrt-indent-mode)))' would do it.
See `dtrt-indent-mode' for more information on Dtrt-Indent mode.

\(fn &optional ARG)" t nil)

(defvar dtrt-indent-mode nil "\
Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'.")

(custom-autoload 'dtrt-indent-mode "dtrt-indent" nil)

;;;***

;;;### (autoloads nil nil ("dtrt-indent-diag.el" "dtrt-indent-pkg.el")
;;;;;;  (23641 39403 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dtrt-indent-autoloads.el ends here
