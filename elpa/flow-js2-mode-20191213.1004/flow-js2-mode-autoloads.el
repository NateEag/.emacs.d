;;; flow-js2-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flow-js2-mode" "flow-js2-mode.el" (0 0 0 0))
;;; Generated autoloads from flow-js2-mode.el

(autoload 'flow-js2-mode "flow-js2-mode" "\
Minor mode for editing JS files with flow type annotations.

If called interactively, enable Flow-Js2 mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flow-js2-mode" '("activate-flow-js2-mode" "flow-js2-" "js2-parse-flow-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flow-js2-mode-autoloads.el ends here
