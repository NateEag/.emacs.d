;;; skerrick-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "skerrick" "skerrick.el" (0 0 0 0))
;;; Generated autoloads from skerrick.el

(autoload 'skerrick-eval-region "skerrick" "\
Evaluate the selected JS code." t nil)

(autoload 'skerrick-install-or-upgrade-server-binary "skerrick" "\
Install or upgrade skerrick from NPM." t nil)

(autoload 'skerrick-start-server "skerrick" "\
Start skerrick server." t nil)

(autoload 'skerrick-stop-server "skerrick" "\
Stop skerrick server." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skerrick" '("skerrick-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skerrick-autoloads.el ends here
