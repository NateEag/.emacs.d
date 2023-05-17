;;; counsel-dash-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel-dash" "counsel-dash.el" (0 0 0 0))
;;; Generated autoloads from counsel-dash.el

(autoload 'counsel-dash "counsel-dash" "\
Query dash docsets.
INITIAL will be used as the initial input, if given.

\(fn &optional INITIAL)" t nil)

(autoload 'counsel-dash-at-point "counsel-dash" "\
Bring up a `counsel-dash' search interface with symbol at point." t nil)

(register-definition-prefixes "counsel-dash" '("counsel-dash-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-dash-autoloads.el ends here
