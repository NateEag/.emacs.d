;;; separedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "separedit" "separedit.el" (0 0 0 0))
;;; Generated autoloads from separedit.el

(autoload 'separedit "separedit" "\
Edit comment or docstring or code BLOCK in them.

Normally, the major mode of the edit buffer will be selected automatically,
but users can also manually select it by pressing `C-u \\[separedit]'.

\(fn &optional BLOCK)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "separedit" '("separedit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; separedit-autoloads.el ends here
