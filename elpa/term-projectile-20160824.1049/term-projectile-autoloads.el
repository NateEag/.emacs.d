;;; term-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "term-projectile" "term-projectile.el" (22465
;;;;;;  938 0 0))
;;; Generated autoloads from term-projectile.el

(autoload 'term-projectile-switch-to "term-projectile" "\
Switch to an existing term-projectile buffer using `completing-read'.

\(fn)" t nil)

(autoload 'term-projectile-forward "term-projectile" "\
Switch forward to the next term-projectile ansi-term buffer.
Make a new one if none exists.

\(fn)" t nil)

(autoload 'term-projectile-backward "term-projectile" "\
Switch backward to the next term-projectile ansi-term buffer.
Make a new one if none exists.

\(fn)" t nil)

(autoload 'term-projectile-default-directory-forward "term-projectile" "\
Switch forward to the next term-projectile ansi-term buffer for `defualt-directory'.

\(fn)" t nil)

(autoload 'term-projectile-default-directory-backward "term-projectile" "\
Switch backward to the next term-projectile ansi-term buffer for `defualt-directory'.

\(fn)" t nil)

(autoload 'term-projectile-create-new "term-projectile" "\
Make a new `ansi-term' buffer for DIRECTORY.
If directory is nil, use the current projectile project

\(fn &optional DIRECTORY)" t nil)

(autoload 'term-projectile-create-new-default-directory "term-projectile" "\
Make a new `ansi-term' buffer in `default-directory'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; term-projectile-autoloads.el ends here
