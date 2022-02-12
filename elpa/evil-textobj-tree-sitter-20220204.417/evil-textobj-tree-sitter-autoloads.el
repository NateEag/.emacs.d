;;; evil-textobj-tree-sitter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-textobj-tree-sitter" "evil-textobj-tree-sitter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-textobj-tree-sitter.el

(autoload 'evil-textobj-tree-sitter-get-textobj "evil-textobj-tree-sitter" "\
Macro to create a textobj function from `GROUP'.
You can pass in multiple groups as a list and in that case as long as
any one of them is available, it will be picked.

You can optionally pass in a alist mapping `major-mode' to their
respective tree-sitter query in `QUERY' with named captures to use
that instead of the default query list.  Check the README file in the
repo to see how to use it.

Check this url for builtin objects
https://github.com/nvim-treesitter/nvim-treesitter-textobjects#built-in-textobjects

\(fn GROUP &optional QUERY)" nil t)

(function-put 'evil-textobj-tree-sitter-get-textobj 'lisp-indent-function 'defun)

(autoload 'evil-textobj-tree-sitter-goto-textobj "evil-textobj-tree-sitter" "\
Got to the start/end of the textobj of type `GROUP'.
By default it goes to the start of the textobj, but pass in `END' if
you want to go to the end of the textobj instead.  You can pass in
`PREVIOUS' if you want to search backwards.  Optionally pass in
`QUERY' if you want to define a custom query.

\(fn GROUP &optional PREVIOUS END QUERY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-textobj-tree-sitter" '("evil-textobj-tree-sitter-")))

;;;***

;;;### (autoloads nil nil ("evil-textobj-tree-sitter-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-textobj-tree-sitter-autoloads.el ends here
