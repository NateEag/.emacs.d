;;; git-walktree-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-walktree" "git-walktree.el" (0 0 0 0))
;;; Generated autoloads from git-walktree.el

(autoload 'git-walktree-open "git-walktree" "\
Open git tree buffer of COMMITISH.
When PATH was given and non-nil open that, otherwise try to open current path.
If target path is not found in COMMITISH tree, go up path and try again until found.

\(fn COMMITISH &optional PATH)" t nil)

(defalias 'git-walktree 'git-walktree-open)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-walktree" '("git-walktree-")))

;;;***

;;;### (autoloads nil "git-walktree-mode" "git-walktree-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from git-walktree-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-walktree-mode" '("git-walktree-")))

;;;***

;;;### (autoloads nil "git-walktree-read" "git-walktree-read.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from git-walktree-read.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-walktree-read" '("git-walktree-read--")))

;;;***

;;;### (autoloads nil "git-walktree-utils" "git-walktree-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from git-walktree-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-walktree-utils" '("git-walktree-")))

;;;***

;;;### (autoloads nil nil ("git-walktree-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-walktree-autoloads.el ends here
