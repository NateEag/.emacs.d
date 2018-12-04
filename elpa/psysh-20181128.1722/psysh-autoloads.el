;;; psysh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "psysh" "psysh.el" (23558 35267 0 0))
;;; Generated autoloads from psysh.el

(autoload 'psysh-doc-buffer "psysh" "\
Execute PsySH Doc `TARGET' and Return PsySH buffer `BUF'.

\(fn TARGET &optional BUF)" nil nil)

(autoload 'psysh-doc-mode "psysh" "\
Major mode for viewing PsySH Doc.

\(fn)" t nil)

(autoload 'psysh-doc-string "psysh" "\
Return string of PsySH Doc `TARGET'.

\(fn TARGET)" nil nil)

(autoload 'psysh-doc "psysh" "\
Display PsySH doc `TARGET'.

\(fn TARGET)" t nil)

(autoload 'psysh "psysh" "\
Run PsySH interactive shell.

\(fn)" t nil)

(autoload 'psysh-run "psysh" "\
Run PsySH interactive-shell in `BUFFER-NAME' and `PROCESS'.

\(fn BUFFER-NAME PROCESS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; psysh-autoloads.el ends here
