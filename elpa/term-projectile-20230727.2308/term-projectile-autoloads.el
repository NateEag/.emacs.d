;;; term-projectile-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "term-projectile" "term-projectile.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from term-projectile.el

(autoload 'term-projectile-switch-to "term-projectile" "\
Switch to an existing term-projectile buffer using `completing-read'." t nil)

(autoload 'term-projectile-forward "term-projectile" "\
Switch forward to the next term-projectile ansi-term buffer.
Make a new one if none exists." t nil)

(autoload 'term-projectile-backward "term-projectile" "\
Switch backward to the next term-projectile ansi-term buffer.
Make a new one if none exists." t nil)

(autoload 'term-projectile-create-new "term-projectile" "\
Make a new `ansi-term' buffer for DIRECTORY.
If directory is nil, use the current projectile project

\(fn &optional (DIRECTORY (projectile-project-root)))" t nil)

(autoload 'term-projectile-default-directory-forward "term-projectile" "\
Switch forward to the next term-projectile ansi-term buffer for `default-directory'." t nil)

(autoload 'term-projectile-default-directory-backward "term-projectile" "\
Switch backward to the next term-projectile ansi-term buffer for `default-directory'." t nil)

(autoload 'term-projectile-default-directory-create-new "term-projectile" "\
Make a new `ansi-term' buffer in `default-directory'." t nil)

(autoload 'term-projectile-global-forward "term-projectile" "\
Switch forward to the next term-projectile ansi-term buffer.
Make a new one if none exists." t nil)

(autoload 'term-projectile-global-backward "term-projectile" "\
Switch backward to the next term-projectile ansi-term buffer.
Make a new one if none exists." t nil)

(autoload 'term-projectile-global-create-new "term-projectile" "\
Make a new `ansi-term' buffer in `term-projectile-global-directory'." t nil)

(register-definition-prefixes "term-projectile" '("maybe-intern" "term-projectile"))

;;;***

;;;### (autoloads nil nil ("term-projectile-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; term-projectile-autoloads.el ends here
