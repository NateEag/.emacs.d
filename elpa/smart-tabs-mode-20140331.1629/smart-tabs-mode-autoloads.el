;;; smart-tabs-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "smart-tabs-mode" "smart-tabs-mode.el" (21361
;;;;;;  38533 0 0))
;;; Generated autoloads from smart-tabs-mode.el

(autoload 'smart-tabs-when "smart-tabs-mode" "\


\(fn CONDITION ADVICE-LIST)" nil t)

(put 'smart-tabs-when 'lisp-indent-function '1)

(autoload 'smart-tabs-create-advice-list "smart-tabs-mode" "\


\(fn ADVICE-LIST)" nil t)

(autoload 'smart-tabs-create-language-advice "smart-tabs-mode" "\
Create a cons cell containing the actions to take to enable
`smart-tabs-mode' for the language LANG. This usually involved enabling
`smart-tabs-mode' through `smart-tabs-mode-enable' and adding a lambda
function to the MODE-HOOK for the specified language. This macro
simplifies the creation of such a cons cell.

\(fn LANG MODE-HOOK ADVICE-LIST &rest BODY)" nil t)

(put 'smart-tabs-create-language-advice 'lisp-indent-function '2)

(autoload 'smart-tabs-mode "smart-tabs-mode" "\
Intelligently indent with tabs, align with spaces!

\(fn &optional ARG)" t nil)

(autoload 'smart-tabs-mode-enable "smart-tabs-mode" "\
Enable smart-tabs-mode.

\(fn)" nil nil)

(autoload 'smart-tabs-advice "smart-tabs-mode" "\


\(fn FUNCTION OFFSET)" nil t)

(autoload 'smart-tabs-insinuate "smart-tabs-mode" "\
Enable smart-tabs-mode for LANGUAGES.
LANGUAGES is a list of SYMBOL or NAME as defined in
'smart-tabs-insinuate-alist' alist or a language using standard named
indent function and indent level.

\(fn &rest LANGUAGES)" nil nil)

(autoload 'smart-tabs-add-language-support "smart-tabs-mode" "\
Add support for a language not already in the `smart-tabs-insinuate-alist'.

\(fn LANG MODE-HOOK ADVICE-LIST &rest BODY)" nil t)

(put 'smart-tabs-add-language-support 'lisp-indent-function '2)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smart-tabs-mode-autoloads.el ends here
