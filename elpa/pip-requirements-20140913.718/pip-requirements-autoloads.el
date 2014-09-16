;;; pip-requirements-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pip-requirements" "pip-requirements.el" (21528
;;;;;;  12537 0 0))
;;; Generated autoloads from pip-requirements.el

(add-to-list 'auto-mode-alist `(,(rx ".pip" string-end) . pip-requirements-mode))

(add-to-list 'auto-mode-alist `(,(rx "requirements" (zero-or-more anything) ".txt" string-end) . pip-requirements-mode))

(autoload 'pip-requirements-mode "pip-requirements" "\
Major mode for editing pip requirements files.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pip-requirements-autoloads.el ends here
