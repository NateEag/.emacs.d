;;; vcard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vcard" "vcard.el" (0 0 0 0))
;;; Generated autoloads from vcard.el

(autoload 'vcard-mode "vcard" "\
Major mode for viewing vCard files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Vv][Cc][Ff]\\'" . vcard-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vcard" '("vcard-font-lock-keywords")))

;;;***

;;;### (autoloads nil "vcard-parse" "vcard-parse.el" (0 0 0 0))
;;; Generated autoloads from vcard-parse.el

(autoload 'vcard-parse-file "vcard-parse" "\
Parse FILE containing vCard data into an alist.

\(fn FILE)" t nil)

(autoload 'vcard-parse-buffer "vcard-parse" "\
Parse current buffer, containing vCard data.
Returns a list of contact objects." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vcard-parse" '("vcard-")))

;;;***

;;;### (autoloads nil nil ("vcard-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vcard-autoloads.el ends here
