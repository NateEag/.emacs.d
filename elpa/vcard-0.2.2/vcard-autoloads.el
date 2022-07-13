;;; vcard-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vcard" "vcard.el" (0 0 0 0))
;;; Generated autoloads from vcard.el

(defvar vcard-pretty-print-function #'vcard-format-sample-box "\
*Formatting function used by `vcard-pretty-print'.")

(custom-autoload 'vcard-pretty-print-function "vcard" t)

(defvar vcard-standard-filters (list #'vcard-filter-html #'vcard-filter-adr-newlines #'vcard-filter-tel-normalize #'vcard-filter-textprop-cr) "\
*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'.")

(custom-autoload 'vcard-standard-filters "vcard" t)

(autoload 'vcard-pretty-print "vcard" "\
Format VCARD into a string suitable for display to user.
VCARD can be an unparsed string containing raw VCF vcard data
or a parsed vcard alist as returned by `vcard-parse-string'.

The result is a string with formatted vcard information suitable for
insertion into a mime presentation buffer.

The function specified by the variable `vcard-pretty-print-function'
actually performs the formatting.  That function will always receive a
parsed vcard alist.

\(fn VCARD)" nil nil)

(autoload 'vcard-parse-string "vcard" "\
Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.)
If supplied to this function an alist of the form

    (((\"prop1a\") \"value1a\")
     ((\"prop2a\" \"prop2b\" (\"prop2c\" . \"param2c\")) \"value2a\")
     ((\"prop3a\" \"prop3b\") \"value3a\" \"value3b\" \"value3c\"))

would be returned.

\(fn RAW &optional FILTER)" nil nil)

(autoload 'vcard-parse-region "vcard" "\
Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!

\(fn BEG END &optional FILTER)" nil nil)

(register-definition-prefixes "vcard" '("vcard-"))

;;;***

;;;### (autoloads nil "vcard-mode" "vcard-mode.el" (0 0 0 0))
;;; Generated autoloads from vcard-mode.el

(autoload 'vcard-mode "vcard-mode" "\
Major mode for viewing vCard files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[Vv][Cc][Ff]\\'" . vcard-mode))

(register-definition-prefixes "vcard-mode" '("vcard-font-lock-keywords"))

;;;***

;;;### (autoloads nil "vcard-parse" "vcard-parse.el" (0 0 0 0))
;;; Generated autoloads from vcard-parse.el

(autoload 'vcard-parse-file "vcard-parse" "\
Parse FILE containing vCard data into an alist.

\(fn FILE)" t nil)

(autoload 'vcard-parse-buffer "vcard-parse" "\
Parse current buffer, containing vCard data.
Returns a list of contact objects." t nil)

(register-definition-prefixes "vcard-parse" '("vcard-"))

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
