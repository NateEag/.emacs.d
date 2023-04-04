;;; magit-stats-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-stats" "magit-stats.el" (0 0 0 0))
;;; Generated autoloads from magit-stats.el

(autoload 'magit-stats-in-buffer "magit-stats" "\
HTML report in a new buffer." t nil)

(autoload 'magit-stats-with-viewer "magit-stats" "\
Open HTML report with OS default viewer." t nil)

(autoload 'magit-stats-json-buffer "magit-stats" "\
JSON report data in a new buffer." t nil)

(autoload 'magit-stats "magit-stats" "\
Generate GIT Repository Statistics via BACKEND.

\(fn BACKEND)" t nil)

(register-definition-prefixes "magit-stats" '("magit-stats-backends"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-stats-autoloads.el ends here
