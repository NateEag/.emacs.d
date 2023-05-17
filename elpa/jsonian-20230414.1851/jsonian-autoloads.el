;;; jsonian-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jsonian" "jsonian.el" (0 0 0 0))
;;; Generated autoloads from jsonian.el

(autoload 'jsonian-enclosing-item "jsonian" "\
Move point to the item enclosing the current point.
If ARG is not nil, move to the ARGth enclosing item.

\(fn &optional ARG)" t nil)

(autoload 'jsonian-find "jsonian" "\
Navigate to a item in a JSON document.
If PATH is supplied, navigate to it.

\(fn &optional PATH)" t nil)

(autoload 'jsonian-mode "jsonian" "\
Major mode for editing JSON files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.json\\'" . jsonian-mode))

(autoload 'jsonian-indent-line "jsonian" "\
Indent a single line.
The indent is determined by examining the previous line.  The
number of spaces is determined by `jsonian-indentation' if it is
set, otherwise it is inferred from the document." t nil)

(autoload 'jsonian-c-mode "jsonian" "\
A major mode for editing JSON documents with comments.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonian-c-mode))

(autoload 'jsonian-enable-flycheck "jsonian" "\
Enable `jsonian-mode' for all checkers where `json-mode' is enabled." t nil)

(autoload 'jsonian-no-so-long-mode "jsonian" "\
Prevent `so-long-mode' from supplanting `jsonian-mode'." t nil)

(register-definition-prefixes "jsonian" '("jsonian-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jsonian-autoloads.el ends here
