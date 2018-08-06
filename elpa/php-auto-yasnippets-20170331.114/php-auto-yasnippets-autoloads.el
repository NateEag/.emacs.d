;;; php-auto-yasnippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "php-auto-yasnippets" "php-auto-yasnippets.el"
;;;;;;  (23400 19291 0 0))
;;; Generated autoloads from php-auto-yasnippets.el

(autoload 'yas/initialize "php-auto-yasnippets" "\
Setup yasnippet hook for php-auto-yasnippet.

\(fn)" nil nil)

(eval-after-load 'yasnippet '(yas/initialize))

(autoload 'yas/create-php-snippet "php-auto-yasnippets" "\
Creates and expands a snippet for the PHP function at point.

If called with the universal prefix then it prompts the user for
the name of a PHP class and treats the name at point as the name
of a method for that class.

\(fn PREFIX)" t nil)

;;;***

;;;### (autoloads nil nil ("php-auto-yasnippets-pkg.el") (23400 19291
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; php-auto-yasnippets-autoloads.el ends here
