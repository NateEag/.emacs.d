;;; bnf-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "bnf-mode" "bnf-mode.el" (23727 12864 120589
;;;;;;  25000))
;;; Generated autoloads from bnf-mode.el

(let ((loads (get 'bnf 'custom-loads))) (if (member '"bnf-mode" loads) nil (put 'bnf 'custom-loads (cons '"bnf-mode" loads))))

(autoload 'bnf-mode "bnf-mode" "\
A major mode for editing BNF grammars.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bnf-mode-autoloads.el ends here
