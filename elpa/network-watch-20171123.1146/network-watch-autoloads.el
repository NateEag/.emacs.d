;;; network-watch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "network-watch" "network-watch.el" (23400 19261
;;;;;;  0 0))
;;; Generated autoloads from network-watch.el

(defvar network-watch-mode t "\
Non-nil if Network-Watch mode is enabled.
See the `network-watch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `network-watch-mode'.")

(custom-autoload 'network-watch-mode "network-watch" nil)

(autoload 'network-watch-mode "network-watch" "\
Network is automatically on when there is a valid network interface active.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; network-watch-autoloads.el ends here
