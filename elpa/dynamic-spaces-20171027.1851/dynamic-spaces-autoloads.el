;;; dynamic-spaces-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dynamic-spaces" "dynamic-spaces.el" (23400
;;;;;;  19116 0 0))
;;; Generated autoloads from dynamic-spaces.el

(autoload 'dynamic-spaces-mode "dynamic-spaces" "\
Minor mode that adapts surrounding spaces when editing.

\(fn &optional ARG)" t nil)

(defvar dynamic-spaces-global-mode nil "\
Non-nil if Dynamic-Spaces-Global mode is enabled.
See the `dynamic-spaces-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dynamic-spaces-global-mode'.")

(custom-autoload 'dynamic-spaces-global-mode "dynamic-spaces" nil)

(autoload 'dynamic-spaces-global-mode "dynamic-spaces" "\
Toggle Dynamic-Spaces mode in all buffers.
With prefix ARG, enable Dynamic-Spaces-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Dynamic-Spaces mode is enabled in all buffers where
`dynamic-spaces-activate-if-applicable' would do it.
See `dynamic-spaces-mode' for more information on Dynamic-Spaces mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dynamic-spaces-autoloads.el ends here
