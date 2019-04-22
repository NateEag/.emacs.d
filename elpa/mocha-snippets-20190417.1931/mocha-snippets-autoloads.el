;;; mocha-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mocha-snippets" "mocha-snippets.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mocha-snippets.el

(autoload 'mocha-snippets-initialize "mocha-snippets" "\
Add mocha-snippets directories to YAS.

\(fn)" nil nil)

(eval-after-load "yasnippet" '(mocha-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mocha-snippets" '("mocha-snippets-")))

;;;***

;;;### (autoloads nil nil ("mocha-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mocha-snippets-autoloads.el ends here
