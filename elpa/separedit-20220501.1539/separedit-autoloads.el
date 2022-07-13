;;; separedit-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "separedit" "separedit.el" (0 0 0 0))
;;; Generated autoloads from separedit.el

(autoload 'separedit-dwim-described-variable "separedit" "\
Edit value of variable at poin in help/helpful buffer." t nil)

(autoload 'separedit-dwim-vterm "separedit" "\
Edit content after vterm prompt." t nil)

(autoload 'separedit-dwim-default "separedit" "\
Edit comment or docstring or code BLOCK in them.

Normally, the major mode of the edit buffer will be selected automatically,
but users can also manually select it by pressing `C-u \\[separedit]'.

\(fn &optional BLOCK)" t nil)

(autoload 'separedit-dwim "separedit" "\


\(fn &optional BLOCK)" t nil)

(defalias 'separedit 'separedit-dwim)

(register-definition-prefixes "separedit" '("separedit-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; separedit-autoloads.el ends here
