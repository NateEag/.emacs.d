;;; add-node-modules-path-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "add-node-modules-path" "add-node-modules-path.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from add-node-modules-path.el

(defvar add-node-modules-path-command "npm bin" "\
Command to find the bin path.")

(custom-autoload 'add-node-modules-path-command "add-node-modules-path" t)

(defvar add-node-modules-path-debug nil "\
Enable verbose output when non nil.")

(custom-autoload 'add-node-modules-path-debug "add-node-modules-path" t)

(autoload 'add-node-modules-path "add-node-modules-path" "\
Run `npm bin` command and add the path to the `exec-path`.
If `npm` command fails, it does nothing." t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; add-node-modules-path-autoloads.el ends here
