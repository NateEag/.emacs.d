;;; bnf-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bnf-mode" "bnf-mode.el" (0 0 0 0))
;;; Generated autoloads from bnf-mode.el

(let ((loads (get 'bnf 'custom-loads))) (if (member '"bnf-mode" loads) nil (put 'bnf 'custom-loads (cons '"bnf-mode" loads))))

(autoload 'bnf-mode "bnf-mode" "\
A major mode for editing BNF grammars.

\\{bnf-mode-map}

Turning on BNF Mode calls the value of `prog-mode-hook' and then of
`bnf-mode-hook', if they are non-nil.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bnf-mode" '("bnf-")))

;;;***

;;;### (autoloads nil nil ("bnf-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bnf-mode-autoloads.el ends here
