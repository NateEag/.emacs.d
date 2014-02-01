;;; gitattributes-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gitattributes-mode) "gitattributes-mode" "gitattributes-mode.el"
;;;;;;  (21228 32219 0 0))
;;; Generated autoloads from gitattributes-mode.el

(autoload 'gitattributes-mode "gitattributes-mode" "\
A major mode for editing .gitattributes files.
\\{gitattributes-mode-map}

\(fn)" t nil)

(dolist (pattern '("/\\.gitattributes\\'" "/\\.git/info/attributes\\'" "/git/attributes\\'")) (add-to-list 'auto-mode-alist (cons pattern #'gitattributes-mode)))

;;;***

;;;### (autoloads nil nil ("gitattributes-mode-pkg.el") (21228 32219
;;;;;;  172476 0))

;;;***

(provide 'gitattributes-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitattributes-mode-autoloads.el ends here
