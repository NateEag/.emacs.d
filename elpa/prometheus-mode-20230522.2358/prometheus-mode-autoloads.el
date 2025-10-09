;;; prometheus-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prometheus-data-mode" "prometheus-data-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from prometheus-data-mode.el

(autoload 'prometheus-data-mode "prometheus-data-mode" "\
Major mode for viewing files containing Prometheus metrics data.

\(fn)" t nil)

(register-definition-prefixes "prometheus-data-mode" '("prometheus-data-mode--build-imenu"))

;;;***

;;;### (autoloads nil "prometheus-mode" "prometheus-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from prometheus-mode.el

(defvar prometheus-mode-line-numbers 't "\
Enable line numbers by default.")

(add-to-list 'magic-mode-alist `(,(rx bol "#" (optional space) (or "HELP" "TYPE") space) . prometheus-data-mode))

;;;***

;;;### (autoloads nil nil ("prometheus-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prometheus-mode-autoloads.el ends here
