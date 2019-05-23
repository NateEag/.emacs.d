;;; auto-rename-tag-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-rename-tag" "auto-rename-tag.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from auto-rename-tag.el

(autoload 'auto-rename-tag-mode "auto-rename-tag" "\
Minor mode 'auto-rename-tag' mode.

\(fn &optional ARG)" t nil)

(defvar global-auto-rename-tag-mode nil "\
Non-nil if Global Auto-Rename-Tag mode is enabled.
See the `global-auto-rename-tag-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-rename-tag-mode'.")

(custom-autoload 'global-auto-rename-tag-mode "auto-rename-tag" nil)

(autoload 'global-auto-rename-tag-mode "auto-rename-tag" "\
Toggle Auto-Rename-Tag mode in all buffers.
With prefix ARG, enable Global Auto-Rename-Tag mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Rename-Tag mode is enabled in all buffers where
`auto-rename-tag-turn-on-auto-rename-tag-mode' would do it.
See `auto-rename-tag-mode' for more information on Auto-Rename-Tag mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto-rename-tag" '("auto-rename-tag-")))

;;;***

;;;### (autoloads nil nil ("auto-rename-tag-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-rename-tag-autoloads.el ends here
