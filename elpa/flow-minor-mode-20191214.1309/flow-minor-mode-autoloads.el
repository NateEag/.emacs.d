;;; flow-minor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flow-minor-mode" "flow-minor-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from flow-minor-mode.el

(autoload 'flow-minor-mode "flow-minor-mode" "\
Flow mode

If called interactively, enable Flow minor mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'flow-minor-enable-automatically "flow-minor-mode" "\
Search for a flow marker and enable flow-minor-mode." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flow-minor-mode" '("flow-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flow-minor-mode-autoloads.el ends here
