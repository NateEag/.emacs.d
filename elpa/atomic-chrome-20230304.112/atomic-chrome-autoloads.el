;;; atomic-chrome-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "atomic-chrome" "atomic-chrome.el" (0 0 0 0))
;;; Generated autoloads from atomic-chrome.el

(autoload 'atomic-chrome-start-server "atomic-chrome" "\
Start websocket server for atomic-chrome.
Fails silently if a server is already running." t nil)

(autoload 'atomic-chrome-stop-server "atomic-chrome" "\
Stop websocket server for atomic-chrome." t nil)

(register-definition-prefixes "atomic-chrome" '("atomic-chrome-" "global-atomic-chrome-edit-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; atomic-chrome-autoloads.el ends here
