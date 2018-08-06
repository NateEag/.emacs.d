;;; focus-autosave-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "focus-autosave-mode" "focus-autosave-mode.el"
;;;;;;  (23400 19173 0 0))
;;; Generated autoloads from focus-autosave-mode.el

(defvar focus-autosave-mode nil "\
Non-nil if Focus-Autosave mode is enabled.
See the `focus-autosave-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `focus-autosave-mode'.")

(custom-autoload 'focus-autosave-mode "focus-autosave-mode" nil)

(autoload 'focus-autosave-mode "focus-autosave-mode" "\
Automatically save all the modified files when the frame loses its focus.

\(fn &optional ARG)" t nil)

(autoload 'focus-autosave-local-mode "focus-autosave-mode" "\
Automatically save this buffer when the frame loses its focus.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; focus-autosave-mode-autoloads.el ends here
