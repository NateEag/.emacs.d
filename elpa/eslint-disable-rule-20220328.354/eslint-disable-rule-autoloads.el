;;; eslint-disable-rule-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eslint-disable-rule" "eslint-disable-rule.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eslint-disable-rule.el

(autoload 'eslint-disable-rule-disable-next-line "eslint-disable-rule" "\
Add eslint-disable-next-line comment above current line to disable RULE-NAME.

If DESCRIPTION is non-nil, insert a description explaining why RULE-NAME
was disabled.

Interactively, ask for RULE-NAME by executing hooks in
`eslint-disable-rule-find-rules-hook'.  Also ask for DESCRIPTION depending
on `eslint-disable-rule-require-description'.

\(fn RULE-NAME &optional DESCRIPTION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eslint-disable-rule" '("eslint-disable-rule-")))

;;;***

;;;### (autoloads nil "eslint-disable-rule-all" "eslint-disable-rule-all.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eslint-disable-rule-all.el

(autoload 'eslint-disable-rule-all "eslint-disable-rule-all" "\
Return a list of all eslint rules." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eslint-disable-rule-all" '("eslint-disable-rule-all-executable")))

;;;***

;;;### (autoloads nil "eslint-disable-rule-flycheck" "eslint-disable-rule-flycheck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eslint-disable-rule-flycheck.el

(autoload 'eslint-disable-rule-flycheck "eslint-disable-rule-flycheck" "\
Return a list of eslint rule names from flycheck-eslint errors.

Return nil if `flycheck' or `flycheck-eslint' are not active so it is safe,
but useless, to use even when not using flycheck." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eslint-disable-rule-flycheck" '("eslint-disable-rule-flycheck--eslint-active-p")))

;;;***

;;;### (autoloads nil "eslint-disable-rule-flymake" "eslint-disable-rule-flymake.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from eslint-disable-rule-flymake.el

(autoload 'eslint-disable-rule-flymake "eslint-disable-rule-flymake" "\
Return a list of eslint rule names from flymake-eslint errors.

Return nil if `flymake' or `flymake-eslint' are not active so it is safe,
but useless, to use even when not using flymake." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eslint-disable-rule-flymake" '("eslint-disable-rule-flymake--")))

;;;***

;;;### (autoloads nil nil ("eslint-disable-rule-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eslint-disable-rule-autoloads.el ends here
