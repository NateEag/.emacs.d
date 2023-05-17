;;; php-runtime-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "php-runtime" "php-runtime.el" (0 0 0 0))
;;; Generated autoloads from php-runtime.el

(eieio-defclass-autoload 'php-runtime-execute 'nil "php-runtime" nil)

(autoload 'php-runtime-expr "php-runtime" "\
Evaluate and echo PHP expression PHP-EXPR.

Pass INPUT-BUFFER to PHP executable as STDIN.

\(fn PHP-EXPR &optional INPUT-BUFFER)" nil nil)

(autoload 'php-runtime-eval "php-runtime" "\
Evaluate PHP code CODE without open tag, and return buffer.

Pass INPUT-BUFFER to PHP executable as STDIN.
Pass OUTPUT-BUFFER to PHP executable as STDOUT.

\(fn CODE &optional INPUT-BUFFER OUTPUT-BUFFER)" nil nil)

(autoload 'php-runtime-extension-loaded-p "php-runtime" "\
Return T if EXTENSION is loaded.

\(fn EXTENSION)" nil nil)

(register-definition-prefixes "php-runtime" '("php-runtime-"))

;;;***

;;;### (autoloads nil nil ("php-runtime-pkg.el" "php-runtime-shortdoc.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-runtime-autoloads.el ends here
