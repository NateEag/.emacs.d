;;; evil-textobj-tree-sitter-core.el --- Provides evil textobjects using tree-sitter -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/evil-textobj-tree-sitter
;; Keywords: evil, tree-sitter, text-object, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This package is a port of nvim-treesitter/nvim-treesitter-textobjects.
;; This package will let you create evil textobjects using the power
;; of tree-sitter grammars.  You can easily create
;; function,class,comment etc textobjects in multiple languages.
;;
;; You can do a sample map like below to create a function textobj.
;; (define-key evil-outer-text-objects-map "f"
;;             (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; `evil-textobj-tree-sitter-get-textobj' will return you a function
;; that you can use in a define-key map.  You can pass in any of the
;; supported queries as an arg of that function.  You can also pass in
;; multiple queries as a list and we will match on all of them, ranked
;; on which ones comes up first in the file.
;; You can find more info in the  README.md file at
;; https://github.com/meain/evil-textobj-tree-sitter

;;; Code:

(require 'cl-lib)
(require 'map)

(defvar evil-textobj-tree-sitter--can-use-elisp-treesitter
  (require 'tree-sitter nil t)
  "If non-nil, we are can make use of elisp-tree-sitter instead of builtin.")
(defvar evil-textobj-tree-sitter--can-use-builtin-treesit
  (require 'treesit nil t)
  "If non-nil, we are can make use of builtin treesit instead of elisp-tree-sitter.")

(defvar evil-textobj-tree-sitter--evil-available (require 'evil nil t)
  "Specifies if we have `evil-mode' available to use in package.")

(defgroup evil-textobj-tree-sitter nil "Text objects based on tree-sitter for Evil."
  :group 'tools)

;; Ignore warnings from optionally requiring tree-sitter and treesit
(defvar tree-sitter-language)
(defvar tree-sitter-tree)
(declare-function tsc-node-byte-range "tsc" t t)
(declare-function tsc--buffer-substring-no-properties "tsc")
(declare-function tsc-query-matches "tsc")
(declare-function tsc-root-node "tsc" t t)
(declare-function tsc-make-query "tsc")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-buffer-root-node "treesit")

(defconst evil-textobj-tree-sitter--dir (file-name-directory (locate-library "evil-textobj-tree-sitter.el"))
  "The directory where the library `tree-sitter-langs' is located.")


(defun evil-textobj-tree-sitter--use-builtin-treesitter ()
  "Return non-nil if we should use builtin treesitter."
  (and evil-textobj-tree-sitter--can-use-builtin-treesit
       (string-suffix-p "-ts-mode" (symbol-name major-mode))))

(defun evil-textobj-tree-sitter--get-queries-dir ()
  "Get the queries directory.
Currently treesit queries are different from queries for elisp-tree-sitter."
  (if (evil-textobj-tree-sitter--use-builtin-treesitter)
      (file-name-as-directory (concat evil-textobj-tree-sitter--dir "treesit-queries"))
    (file-name-as-directory (concat evil-textobj-tree-sitter--dir "queries"))))

(defcustom evil-textobj-tree-sitter--get-queries-dir-func  #'evil-textobj-tree-sitter--get-queries-dir
  "Function to get location for the queries."
  :group 'evil-textobj-tree-sitter
  :type 'function)

(defcustom evil-textobj-tree-sitter-major-mode-language-alist nil
  "Alist that maps major modes to tree-sitter language names."
  :group 'evil-textobj-tree-sitter
  :type '(alist :key-type symbol
                :value-type string))
(pcase-dolist (`(,major-mode . ,lang-symbol)
               (reverse '((c++-mode . "cpp")
                          (c++-ts-mode . "cpp")
                          (c-mode . "c")
                          (c-ts-mode . "c")
                          (csharp-mode . "csharp")
                          (csharp-ts-mode . "csharp")
                          (elixir-mode . "elixir")
                          (elixir-ts-mode . "elixir")
                          (elm-mode . "elm")
                          (elm-ts-mode . "elm")
                          (ess-r-mode . "r")
                          (go-mode . "go")
                          (go-ts-mode . "go")
                          (haskell-mode . "haskell")
                          (haskell-ts-mode . "haskell")
                          (html-mode . "html")
                          (html-ts-mode . "html")
                          (java-mode . "java")
                          (java-ts-mode . "java")
                          (javascript-mode . "javascript")
                          (javascript-ts-mode . "javascript")
                          (js-mode . "javascript")
                          (js-ts-mode . "javascript")
                          (js2-mode . "javascript")
                          (js3-mode . "javascript")
                          (julia-mode . "julia")
                          (julia-ts-mode . "julia")
                          (matlab-mode . "matlab")
                          (php-mode . "php")
                          (php-ts-mode . "php")
                          (prisma-mode . "prisma")
                          (prisma-ts-mode . "prisma")
                          (python-mode . "python")
                          (python-ts-mode . "python")
                          (rjsx-mode . "javascript")
                          (ruby-mode . "ruby")
                          (ruby-ts-mode . "ruby")
                          (rust-mode . "rust")
                          (rust-ts-mode . "rust")
                          (rustic-mode . "rust")
                          (sh-mode . "bash")
                          (bash-ts-mode . "sh")
                          (shell-script-mode . "bash")
                          (typescript-mode . "typescript")
                          (typescript-ts-mode . "typescript")
                          (verilog-mode . "verilog")
                          (zig-mode . "zig"))))
  (setf (map-elt evil-textobj-tree-sitter-major-mode-language-alist
                 major-mode) lang-symbol))

(defcustom evil-textobj-tree-sitter-use-next-if-not-within t
  "When non-nil, use the next match if we are not within one."
  :group 'evil-textobj-tree-sitter
  :type 'boolean)

(defun evil-textobj-tree-sitter--nodes-filter-before (nodes)
  "NODES which contain the current after them."
  (sort (cl-remove-if-not (lambda (x)
                            (< (car (last x)) (point)))
                          nodes)
        (lambda (x y) (< (car (last x)) (car (last y))))))

(defun evil-textobj-tree-sitter--nodes-filter-within (nodes)
  "NODES which contain the current point inside them ordered inside out."
  (let ((byte-pos (point)))
    (sort (cl-remove-if-not (lambda (x)
                              (and (<= (nth 1 x) byte-pos)
                                   (< byte-pos (car (last x)))))
                            nodes)
          (lambda (x y)
            (< (+ (abs (- byte-pos
                          (nth 1 x)))
                  (abs (- byte-pos
                          (car (last x))))) (+ (abs (- byte-pos
                          (nth 1 y)))
                  (abs (- byte-pos
                          (car (last x))))))))))

(defun evil-textobj-tree-sitter--nodes-filter-after (nodes)
  "NODES which contain the current point before them ordered top to bottom."
  (sort (cl-remove-if-not (lambda (x)
                            (> (nth 1 x) (point)))
                          nodes)
        (lambda (x y) (< (nth 1 x) (nth 1 y)))))

(defun evil-textobj-tree-sitter--get-inherits-line (filename)
  "Get the inherits line from `FILENAME'.
It might not be on the fist line and so we cannot just get the first line."
  (with-temp-buffer
    (if (file-exists-p filename)
        (progn
          (insert-file-contents filename)
          (goto-char (point-min))
          (search-forward "; inherits: " nil t)
          (let ((line (thing-at-point 'line t)))
            (if (string-match "^; inherits: \\([a-z_,()]+\\)$" line)
                (match-string 1 line)))))))

(defun evil-textobj-tree-sitter--get-query-from-dir (language queries-dir top-level)
  "Get tree sitter query for `LANGUAGE' from `QUERIES-DIR'.
`TOP-LEVEL' is used to mention if we should load optional inherits.
https://github.com/nvim-treesitter/nvim-treesitter/pull/564"
  (let ((filename (concat queries-dir language "/textobjects.scm")))
    (with-temp-buffer
      (if (file-exists-p filename)
          (progn
            (insert-file-contents filename)
            (goto-char (point-min))
            (let ((inherits-line (evil-textobj-tree-sitter--get-inherits-line filename)))
              (if inherits-line
                  (insert (string-join (mapcar (lambda (x)
                                                 (if (string-prefix-p "(" x)
                                                     (if top-level
                                                         (evil-textobj-tree-sitter--get-query-from-dir (substring x 1 -1)
                                                                                                       queries-dir nil))
                                                   (evil-textobj-tree-sitter--get-query-from-dir x queries-dir nil)))
                                               (split-string inherits-line ","))
                                       "\n"))))
            (buffer-string))))))

(defun evil-textobj-tree-sitter--get-query (language)
  "Get tree sitter query for `LANGUAGE'."
  (evil-textobj-tree-sitter--get-query-from-dir language (funcall evil-textobj-tree-sitter--get-queries-dir-func) t))

(defun evil-textobj-tree-sitter--treesit-get-nodes (query)
  "Get nodes for `QUERY' using builtin `treesit'."
  (let* ((lang-name (alist-get major-mode evil-textobj-tree-sitter-major-mode-language-alist))
         (user-query (alist-get major-mode query))
         (f-query (if (eq user-query nil)
                      (evil-textobj-tree-sitter--get-query lang-name)
                    user-query))
         (root-node (treesit-buffer-root-node))
         (captures (treesit-query-capture root-node f-query)))
    (seq-map (lambda (x)
               (list  (car x)
                      (treesit-node-start (cdr x))
                      (treesit-node-end (cdr x))))
             captures)))

(defun evil-textobj-tree-sitter--elisp-tree-sitter-get-nodes (query)
  "Get nodes for `QUERY' using `elisp-tree-sitter'."
  (let* ((lang-name (alist-get major-mode evil-textobj-tree-sitter-major-mode-language-alist))
         (user-query (alist-get major-mode query))
         (f-query (if (eq user-query nil)
                      (evil-textobj-tree-sitter--get-query lang-name)
                    user-query))
         (root-node (tsc-root-node tree-sitter-tree))
         (query (tsc-make-query tree-sitter-language f-query))
         (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties))
         (all-captures '()))
    (progn
      (seq-map (lambda (x)
                 (let* ((match-captures (cdr x))
                        (capture-start nil)
                        (capture-end nil))
                   (seq-map (lambda (y)
                              (let ((csplits (split-string (symbol-name (car y)) "\\.")))
                                (pcase (car (last csplits))
                                  ("_end" (setq capture-end y))
                                  ("_start" (setq capture-start y))
                                  (_ (push (list (car y)
                                                 (car (tsc-node-byte-range (cdr y)))
                                                 (cdr (tsc-node-byte-range (cdr y))))
                                           all-captures)))))
                            match-captures)
                   (if (and capture-start capture-end)
                       (push (list (intern (string-join (butlast (split-string (symbol-name (car capture-start)) "\\.") 1) "."))
                                   (car (tsc-node-byte-range (cdr capture-start)))
                                   (cdr (tsc-node-byte-range (cdr capture-end))))
                             all-captures))))
               matches)
      (mapcar (lambda (c)
                (list (car c)
                      (byte-to-position (cadr c))
                      (byte-to-position (caddr c))))
              all-captures))))

(defun evil-textobj-tree-sitter--get-nodes (group query)
  "Get a list of viable nodes based on `GROUP' value.
They will be order with captures with point inside them first then the
ones that follow.  If a `QUERY' alist is provided and
it contains a current `major-mode',
we make use of that instead of the builtin query set."
  (cl-remove-duplicates (cl-remove-if-not (lambda (x)
                                            (member (car x) group))
                                          (if (evil-textobj-tree-sitter--use-builtin-treesitter)
                                              (evil-textobj-tree-sitter--treesit-get-nodes query)
                                            (evil-textobj-tree-sitter--elisp-tree-sitter-get-nodes query)))
                        :test (lambda (x y)
                                (and (eq (car x) (car y)) ;; names are equal
                                     (= (nth 1 x) (nth 1 y))
                                     (= (car (last x)) (car (last y)))))))

(defun evil-textobj-tree-sitter--get-within-and-after (group count query)
  "Given a `GROUP' `QUERY' find `COUNT' number of nodes within in and after point."
  (let* ((nodes (evil-textobj-tree-sitter--get-nodes group query))
         (nodes-within (evil-textobj-tree-sitter--nodes-filter-within nodes))
         (nodes-after (evil-textobj-tree-sitter--nodes-filter-after nodes))
         (filtered-nodes (if (and (equal 1 count)
                                  (not evil-textobj-tree-sitter-use-next-if-not-within))
                             nodes-within
                           (append nodes-within nodes-after))))
    (if (> (length filtered-nodes) 0)
        (cl-subseq filtered-nodes 0 count))))

(defun evil-textobj-tree-sitter--get-within (group count query)
  "Given a `GROUP' `QUERY' find `COUNT' number of nodes within point."
  (let* ((nodes (evil-textobj-tree-sitter--get-nodes group query))
         (nodes-within (evil-textobj-tree-sitter--nodes-filter-within nodes)))
    (if (> (length nodes-within) 0)
        (cl-subseq nodes-within 0 count))))

(defun evil-textobj-tree-sitter--range (count ts-group &optional query)
  "Get the range of the closeset item of type `TS-GROUP'.
`COUNT' is supported even thought it does not actually make sense in
most cases as if we do 3-in-func the selections will not be continues,
but we can only provide the start and end as of now which is what we
are doing.  If a `QUERY' alist is provided, we make use of that
instead of the builtin query set."
  (let ((nodes (evil-textobj-tree-sitter--get-within-and-after ts-group count query)))
    (if (not (eq nodes nil))
        (let ((range-min (apply #'min
                                (seq-map (lambda (x)
                                           (nth 1 x))
                                         nodes)))
              (range-max (apply #'max
                                (seq-map (lambda (x)
                                           (car (last x)))
                                         nodes))))
          ;; Have to compute min and max like this as we might have nested functions
          (cons range-min range-max)))))

(defun evil-textobj-tree-sitter--message-not-found (groups)
  "Log a message that `GROUPS' are not found."
  (let ((not-found (mapconcat (lambda (g)
                                (concat "'" g "'"))
                              groups
                              " or ")))
    (error (concat "No " not-found " text object found"))))

;;;###autoload
(defmacro evil-textobj-tree-sitter-get-textobj (group &optional query)
  "Macro to create a textobj function from `GROUP'.
You can pass in multiple groups as a list and in that case as long as
any one of them is available, it will be picked.

You can optionally pass in a alist mapping `major-mode' to their
respective tree-sitter query in `QUERY' with named captures to use
that instead of the default query list.  If `QUERY' does not contain
current `major-mode', then the default queries are used.
Check the README file in the repo to see how to use it.

Check this url for builtin objects
https://github.com/nvim-treesitter/nvim-treesitter-textobjects#built-in-textobjects"
  (declare (debug t) (indent defun))
  (if evil-textobj-tree-sitter--evil-available
      (let* ((groups (if (eq (type-of group) 'string)
                         (list group)
                       group))
             (funsymbol (intern (concat "evil-textobj-tree-sitter-function--"
                                        (mapconcat 'identity groups "-"))))
             (interned-groups (mapcar #'intern groups)))
        `(evil-define-text-object ,funsymbol
           ;; rest argument is named because of compiler warning `argument _ not left unused`
           (count &rest unused)
           (let ((range (evil-textobj-tree-sitter--range count ',interned-groups ,query)))
             (if (not (eq range nil))
                 (evil-range (car range)
                             (cdr range))
               (evil-textobj-tree-sitter--message-not-found ',groups)))))
    (error "Cannot use `evil-textobj-tree-sitter-goto-textobj' without `evil-mode'")))

(defun evil-textobj-tree-sitter--get-goto-location (groups previous end query)
  "Get the start/end of the textobj of type `GROUPS'.
By default it goes to the start of the textobj, but pass in `END' if
you want to go to the end of the textobj instead.  You can pass in
`PREVIOUS' if you want to search backwards.  `QUERY' for custom queries."
  (let* ((nodes (evil-textobj-tree-sitter--get-nodes groups query))
         (usable-nodes (if previous
                           (if end
                               (cl-remove-if-not (lambda (x)
                                                   (< (car (last x)) (point)))
                                                 nodes)
                             (cl-remove-if-not (lambda (x)
                                                 (< (nth 1 x) (point)))
                                               nodes))
                         (if end
                             (cl-remove-if-not (lambda (x)
                                                 ;; -1 is needed as evil cannot select till end
                                                 (> (- (car (last x)) 1) (point)))
                                               nodes)
                           (cl-remove-if-not (lambda (x)
                                               (> (nth 1 x) (point)))
                                             nodes))))
         (node (car (sort usable-nodes
                          (lambda (x y)
                            (if previous
                                (if end
                                    (> (car(last x)) (car (last y)))
                                  (> (nth 1 x) (nth 1 y)))
                              (if end
                                  (< (car(last x)) (car (last y)))
                                (< (nth 1 x) (nth 1 y)))))))))
    (if node
        (let ((actual-position (if end
                                   (car (last node))
                                 (nth 1 node))))
          (if end
              ;; tree sitter count + 1 kinda (probably have to look in other places as well)
              ;; This is a mess that evil creates (not really an issue in Emacs mode)
              (- actual-position 1)
            actual-position)))))

;;;###autoload
(defun evil-textobj-tree-sitter-goto-textobj (group &optional previous end query)
  "Got to the start/end of the textobj of type `GROUP'.
By default it goes to the start of the textobj, but pass in `END' if
you want to go to the end of the textobj instead.  You can pass in
`PREVIOUS' if you want to search backwards.  Optionally pass in
`QUERY' if you want to define a custom query."
  (let* ((groups (if (eq (type-of group) 'string)
                     (list group)
                   group))
         (interned-groups (mapcar #'intern groups))
         (goto-position (evil-textobj-tree-sitter--get-goto-location
                         interned-groups previous end query)))
    (if goto-position
        (goto-char goto-position)
      (evil-textobj-tree-sitter--message-not-found groups))))

(provide 'evil-textobj-tree-sitter-core)
;;; evil-textobj-tree-sitter-core.el ends here
