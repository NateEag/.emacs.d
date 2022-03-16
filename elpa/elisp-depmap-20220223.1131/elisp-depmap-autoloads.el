;;; elisp-depmap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-depmap" "elisp-depmap.el" (0 0 0 0))
;;; Generated autoloads from elisp-depmap.el

(autoload 'elisp-depmap-makesummarytable "elisp-depmap" "\
Make a summary org table of variables and references to them." t nil)

(autoload 'elisp-depmap-graphviz-digraph "elisp-depmap" "\
Make a dot file representation of all definitions and references.
Optionally set INDENT-WIDTH which is 2 by default.
If SHUFFLE gives a random seed (default 0) to shuffle subgraph cluster layouts.

\(fn &optional SHUFFLE)" t nil)

(autoload 'elisp-depmap-graphviz "elisp-depmap" "\
Make a very basic dot file representation of all the top level definitions in a project, and their references." t nil)

;;;***

;;;### (autoloads nil "elisp-depmap-exec" "elisp-depmap-exec.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from elisp-depmap-exec.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-depmap-exec" '("elisp-depmap-exec-")))

;;;***

;;;### (autoloads nil "elisp-depmap-graph" "elisp-depmap-graph.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from elisp-depmap-graph.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-depmap-graph" '("elisp-depmap-graph-")))

;;;***

;;;### (autoloads nil "elisp-depmap-parse" "elisp-depmap-parse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from elisp-depmap-parse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-depmap-parse" '("elisp-depmap-parse-")))

;;;***

;;;### (autoloads nil nil ("elisp-depmap-pkg.el" "elisp-depmap-secondhelp.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-depmap-autoloads.el ends here
