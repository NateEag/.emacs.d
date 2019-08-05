;;; php-runtime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "php-runtime" "php-runtime.el" (0 0 0 0))
;;; Generated autoloads from php-runtime.el

(eieio-defclass-autoload 'php-runtime-execute 'nil "php-runtime" nil)

(autoload 'php-runtime-expr "php-runtime" "\
Evalute and echo PHP expression `PHP-EXPR'.

Pass `INPUT-BUFFER' to PHP executable as STDIN.

\(fn PHP-EXPR &optional INPUT-BUFFER)" nil nil)

(autoload 'php-runtime-eval "php-runtime" "\
Evalute PHP code `CODE' without open tag, and return buffer.

Pass `INPUT-BUFFER' to PHP executable as STDIN.

\(fn CODE &optional INPUT-BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-runtime" '("php-runtime-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-runtime-autoloads.el ends here
