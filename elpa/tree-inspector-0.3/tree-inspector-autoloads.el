;;; tree-inspector-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tree-inspector" "tree-inspector.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from tree-inspector.el

(autoload 'tree-inspector-inspect-last-sexp "tree-inspector" "\
Evaluate sexp before point and inspect the result." t nil)

(autoload 'tree-inspector-inspect-defun "tree-inspector" "\
Inspect the top-level defun." t nil)

(autoload 'tree-inspector-inspect-region "tree-inspector" "\
Inspect the region.

\(fn START END)" t nil)

(autoload 'tree-inspector-inspect-expression "tree-inspector" "\
Evaluate EXP and inspect its result with a tree-inspector.

\(fn EXP)" t nil)

(register-definition-prefixes "tree-inspector" '("tree-inspector-"))

;;;***

;;;### (autoloads nil "tree-inspector-tests" "tree-inspector-tests.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from tree-inspector-tests.el

(register-definition-prefixes "tree-inspector-tests" '("inspector-tests--person" "tree-inspector-tests-"))

;;;***

;;;### (autoloads nil nil ("tree-inspector-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tree-inspector-autoloads.el ends here
