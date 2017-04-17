;;; add-hooks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "add-hooks" "add-hooks.el" (22772 47624 0 0))
;;; Generated autoloads from add-hooks.el

(autoload 'add-hooks-pair "add-hooks" "\
Call `add-hook' for each combined pair of items in HOOKS and FUNCTIONS.

Either value can be a single symbol or a list of symbols, in
which case a function can be added to multiple hooks and/or
multiple functions can be added to a hook.  This behaves like
`add-hook' when both values are atoms.

Example:

  (add-hooks-pair '(css-mode-hook sgml-mode-hook) 'emmet-mode)

Result:

  ELISP> css-mode-hook
  (emmet-mode)

  ELISP> sgml-mode-hook
  (emmet-mode)

\(fn HOOKS FUNCTIONS)" nil nil)

(autoload 'add-hooks "add-hooks" "\
Call `add-hooks-pair' on each cons pair in PAIRS.

Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Either side of the pair can be
a single symbol or a list of symbols, as passed to
`add-hooks-pair'.

Usage:

  (add-hooks '((hook-or-hooks . function-or-functions)...))

Example:

  (add-hooks '(((css-mode-hook sgml-mode-hook) . emmet-mode)))

Result:

  ELISP> css-mode-hook
  (emmet-mode)

  ELISP> sgml-mode-hook
  (emmet-mode)

\(fn PAIRS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; add-hooks-autoloads.el ends here
