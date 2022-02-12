;;; inline-docs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inline-docs" "inline-docs.el" (0 0 0 0))
;;; Generated autoloads from inline-docs.el

(autoload 'inline-docs-display-docs-momentary "inline-docs" "\
Display inline docs FORMAT-STRING under point with extra ARGS.

\(fn FORMAT-STRING &rest ARGS)" nil nil)

(defalias 'inline-docs 'inline-docs-display-docs-momentary)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inline-docs" '("inline-docs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inline-docs-autoloads.el ends here
