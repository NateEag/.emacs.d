;;; editorconfig-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "editorconfig" "editorconfig.el" (22074 45632
;;;;;;  0 0))
;;; Generated autoloads from editorconfig.el

(autoload 'edconf-find-file-hook "editorconfig" "\


\(fn)" nil nil)

(add-hook 'find-file-hook 'edconf-find-file-hook)

(add-to-list 'auto-mode-alist '("/\\.editorconfig\\'" . conf-unix-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; editorconfig-autoloads.el ends here
