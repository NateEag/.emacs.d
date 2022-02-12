;;; edebug-inline-result-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Edebug-Inline-Result mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edebug-inline-result" '("edebug-inline-result-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edebug-inline-result-autoloads.el ends here
