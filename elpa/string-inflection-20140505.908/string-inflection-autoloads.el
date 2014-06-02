;;; string-inflection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "string-inflection" "string-inflection.el"
;;;;;;  (21388 62553 0 0))
;;; Generated autoloads from string-inflection.el

(autoload 'string-inflection-camelize "string-inflection" "\
FooBar format

\(fn)" t nil)

(autoload 'string-inflection-lower-camelize "string-inflection" "\
fooBar format

\(fn)" t nil)

(autoload 'string-inflection-underscore "string-inflection" "\
foo_bar format

\(fn)" t nil)

(autoload 'string-inflection-upcase "string-inflection" "\
FOO_BAR format

\(fn)" t nil)

(autoload 'string-inflection-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar

\(fn)" t nil)

(autoload 'string-inflection-toggle "string-inflection" "\
toggle foo_bar and FooBar

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; string-inflection-autoloads.el ends here
