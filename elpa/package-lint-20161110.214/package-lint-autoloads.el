;;; package-lint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "package-lint" "package-lint.el" (22564 50452
;;;;;;  0 0))
;;; Generated autoloads from package-lint.el

(autoload 'package-lint-buffer "package-lint" "\
Get linter errors and warnings for BUFFER.

With FORCE non-nil, lint the buffer even if neither Package-Requires nor
Package-Version headers are present.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified.

\(fn &optional BUFFER FORCE)" nil nil)

(autoload 'package-lint-current-buffer "package-lint" "\
Display lint errors and warnings for the current buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; package-lint-autoloads.el ends here
