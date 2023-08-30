;;; language-id-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "language-id" "language-id.el" (0 0 0 0))
;;; Generated autoloads from language-id.el

(autoload 'language-id-buffer "language-id" "\
Get GitHub Linguist language name for current buffer.

Return the name of the programming language or markup language
used in the current buffer.  The name is a string from the GitHub
Linguist language list.  The language is determined by looking at
the active `major-mode'.  Some major modes support more than one
language.  In that case minor modes and possibly other variables
are consulted to disambiguate the language.

In addition to the modes bundled with GNU Emacs, many third-party
modes are recognized.  No statistical text matching or other
heuristics are used in detecting the language.

The language definitions live inside the language-id library and
are updated in new releases of the library.

If the language is not unambiguously recognized, the function
returns nil." t nil)

(register-definition-prefixes "language-id" '("language-id--"))

;;;***

;;;### (autoloads nil nil ("language-id-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; language-id-autoloads.el ends here
