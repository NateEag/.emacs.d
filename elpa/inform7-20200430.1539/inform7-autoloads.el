;;; inform7-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inform7" "inform7.el" (0 0 0 0))
;;; Generated autoloads from inform7.el

(autoload 'inform7-mode "inform7" "\
Major mode for editing Inform 7 files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(ni\\|i7\\)\\'" . inform7-mode))

(add-to-list 'auto-mode-alist '("\\.i7x\\'" . inform7-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inform7" '("inform7-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inform7-autoloads.el ends here
