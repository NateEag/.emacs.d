;;; markdown-changelog-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "markdown-changelog" "markdown-changelog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from markdown-changelog.el

(autoload 'markdown-changelog-new "markdown-changelog" "\
Create a new changelog buffer with project URL.

\(fn URL)" t nil)

(autoload 'markdown-changelog-insert-release "markdown-changelog" "\
Add a new release to the change log." t nil)

(register-definition-prefixes "markdown-changelog" '("markdown-changelog-"))

;;;***

;;;### (autoloads nil nil ("markdown-changelog-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; markdown-changelog-autoloads.el ends here
