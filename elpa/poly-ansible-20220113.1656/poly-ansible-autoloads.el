;;; poly-ansible-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "poly-ansible" "poly-ansible.el" (0 0 0 0))
;;; Generated autoloads from poly-ansible.el
 (autoload 'poly-ansible-mode "poly-ansible")

(add-to-list 'auto-mode-alist '("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode))

(add-to-list 'auto-mode-alist '("/\\(?:group\\|host\\)_vars/" . poly-ansible-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-ansible" '("jinja2-ansible-functions-keywords" "pm-inner/jinja2" "poly-ansible-mode")))

;;;***

;;;### (autoloads nil "poly-ansible-jinja2-filters" "poly-ansible-jinja2-filters.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from poly-ansible-jinja2-filters.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-ansible-jinja2-filters" '("poly-ansible-jinja2-filters")))

;;;***

;;;### (autoloads nil nil ("poly-ansible-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; poly-ansible-autoloads.el ends here
