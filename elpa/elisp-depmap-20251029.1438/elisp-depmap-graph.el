;;; elisp-depmap-graph.el --- Generate a graphviz map of functions and definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/elisp-depmap.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; See elisp-depmap.el

;;; Code:
(require 'elisp-depmap-parse)
(require 'subr-x)
(require 'seq)

(defcustom elisp-depmap-graph-stripprojectname t
  "Strip the project name from the graph."
  :type 'boolean
  :group 'elisp-depmap)

(defcustom elisp-depmap-graph-linemod 10
  "Line scaling modifier.  Higher reduces the border width."
  :type 'integer
  :group 'elisp-depmap)

(defcustom elisp-depmap-graph-indentwidth 4
  "Indent width in spaces."
  :type 'integer
  :group 'elisp-depmap)

(defcustom elisp-depmap-graph-decorate
  '(:edge
    ((style . tapered))
    :graph
    ((style . rounded) (bgcolor . white) (labelfloat . true) (penwidth . 3) (pencolor . black) (splines . ortho) (rankdir . TB))
    :subgraph
    ((bgcolor . grey70) (fontcolor . black) (fontsize . 25.0) (fontname . "\"times bold\""))
    :subsubgraph
    ((bgcolor . white) (fontcolor . black) (fontsize . 12.0) (margin . 10)))
  "Attributes to give to :edge, :node, :graph, the :subgraph, and :subsubgraph.
Subgraph represent (file clusters), and the nested clusters :subsubgraph (groups
defined by `elisp-depmap-parse-subclustergroups')."
  :type 'plist
  :group 'elisp-depmap)

(defcustom elisp-depmap-graph-filecolorsymbols
  '((red . "ᚻ") (blue .  "ᛉ") (darkgreen . "ᛊ") (orange . "ᛋ") (purple . "ᛗ")
    (gray . "ᛝ") (green . "ᛢ") (yellow . "ᛪ") (pink . "ᛯ") (brown . "ᛸ")
    (navy . "ᛒ") (maroon . "ᚷ") (violet . "ᚫ") (brown . "ᚣ") (cornflowerblue . "ŧ")
    (darkslategray4 . "Ω")(firebrick . "Æ") (goldenrod4 . "þ"))
  "Alist of colors and symbols used to style and prefix files.
More colors at the https://www.graphviz.org/doc/info/colors.html website."
  :type 'alist
  :group 'elisp-depmap)

(defcustom elisp-depmap-graph-subclustergroups
  '(:variables (setq defvar defcustom) :functions (defun defsubst defmacro))
  "Define subcluster groups and the which symbols should be assigned to them.
By default we only have variables and functions, though any number of groups
can be defined.  It is not necessary to use all symbols from the
 `elisp-depmap-parse-function-shapes' variable."
  :type 'plist
  :group 'elisp-depmap)


(defun elisp-depmap-graph--decorate (keyword &optional indent)
  "Generate format string for KEYWORD from `elisp-depmap-graph-decorate'.
If INDENT is nil, all properties are inlined into square brackets, otherwise
each property is seperated by a newline followed by the INDENT amount in spaces."
  (let ((func-lay (lambda (x) (format "%s=%s" (car x) (cdr x))))
        (keyw-lst (plist-get elisp-depmap-graph-decorate keyword))
        (inds-spc (if indent (make-string indent ? ) "")))
    (if keyw-lst
        (if indent
            (concat (mapconcat func-lay keyw-lst
                               (concat ";\n" inds-spc)) ";")
          (format "[%s]" (mapconcat func-lay keyw-lst ";"))))))

(defun elisp-depmap-graph--filesuniq (hashtable)
  "Get the unique files in HASHTABLE."
  (seq-uniq (--map (plist-get it :file)
                   (hash-table-values hashtable))))

(defun elisp-depmap-graph--makefilemapcolors (hashtable)
  "From HASHTABLE make a plist of file, cluster no, and color for each file."
  (let ((colors (mapcar #'car elisp-depmap-graph-filecolorsymbols))
        (symbls (mapcar #'cdr elisp-depmap-graph-filecolorsymbols))
        (files-uniq (elisp-depmap-graph--filesuniq hashtable)))
    (--map (let ((colr (nth it colors))
                 (file (nth it files-uniq))
                 (symb (nth it symbls))
                 (clst (format "cluster_%d" it)))
             `(:file ,file :color ,colr :clust ,clst, :symbol ,symb))
           (number-sequence 0 (1- (length files-uniq))))))

(defun elisp-depmap-graph--newname (functionname filename &optional symbol)
  "Strip either the projectname or the FILENAME prefix from FUNCTIONNAME.
If SYMBOL, use that as replacement."
  (let* ((prool (car (split-string filename "\\.el")))
         (pregx (format "^%s" prool)))
    (if elisp-depmap-graph-stripprojectname
        (replace-regexp-in-string pregx (or symbol "§") functionname)
      functionname)))

(defun elisp-depmap-graph--makesubsubgraph (hashtable funcmap entry subg ind)
  "Make a sub subgraph for file ENTRY info using the SUBG keyword.
The SUBG keyword from `elisp-depmap-graph-subclustergroups' from HASHTABLE.
Use FUNCMAP for shapes, and use IND to set the indent number."
  (let ((vfile (plist-get entry :file))
        ;;(color (plist-get entry :color))
        (clust (plist-get entry :clust))
        (symbl (plist-get entry :symbol))
        (nex-ind (+ ind elisp-depmap-graph-indentwidth))
        (vr-subclust elisp-depmap-graph-subclustergroups)
        (vr-linemods elisp-depmap-graph-linemod)
        (fn-graphdec #'elisp-depmap-graph--decorate)
        (fn-newnames #'elisp-depmap-graph--newname))
    (let ((accepted-stypes (--map (format "%s" it) (plist-get vr-subclust subg)))
          (clust-keyword (concat
                          clust (string-remove-prefix ":" (format "%s" subg))))
          (ind-now (make-string ind ? ))
          (ind-nex (make-string nex-ind ? )))
      (insert "\n"
              ind-now (format "subgraph %s {\n" clust-keyword)
              ind-nex (format "label = \"%s\";\n" subg)
              ind-nex (funcall fn-graphdec :subsubgraph nex-ind) "\n")
      (maphash
       (lambda (funcname info)
         ;; Only process functions from VFILE
         (if (string= (plist-get info :file) vfile)
             (let ((oname (funcall fn-newnames funcname vfile symbl))
                   (vbegs (plist-get info :line-beg))
                   (vends (plist-get info :line-end))
                   (vtype (plist-get info :type)))
               (if (member vtype accepted-stypes)
                   (let ((numlines (if vends (- vends vbegs) 1)))
                     (insert ind-nex
                             (format
                              "node [shape=%s,penwidth=%s] \"%s\";\n"
                              (alist-get (intern vtype) funcmap)
                              (1+ (/ numlines vr-linemods))
                              oname)))))))
       hashtable)
      (insert ind-now "}\n"))))

(defun elisp-depmap-graph--makedigraphgroups (hashtable filemap funcmap ind)
  "Make digraph subgraphs for each file cluster, using files from HASHTABLE.
Decorate them using colors from FILEMAP and shapes from FUNCMAP.
Set indent by IND amount."
  (let* ((next-ind (+ ind elisp-depmap-graph-indentwidth))
         (ind-prev (make-string ind ? ))
         (ind-next (make-string next-ind ? )))
    (dolist (vfile (--map (plist-get it :file) filemap))
      (let ((entry (--first (string= (plist-get it :file) vfile) filemap))
            (fn-newnames #'elisp-depmap-graph--newname)
            (fn-subgraph #'elisp-depmap-graph--makesubsubgraph)
            (fn-decorate #'elisp-depmap-graph--decorate)
            (vr-striproj elisp-depmap-graph-stripprojectname)
            (vr-subclust elisp-depmap-graph-subclustergroups))
        (let ((subg-keys  ;; Not how plists are meant to be used...
               (--filter (string-prefix-p ":" (format "%s" it)) vr-subclust))
              (color (plist-get entry :color))
              (symbl (plist-get entry :symbol))
              (clust (plist-get entry :clust)))
          (insert "\n"
                  ind-prev (format "subgraph %s {\n" clust)
                  ind-next (funcall fn-decorate :subgraph next-ind) "\n"
                  ind-next (format "label = \"%s\";\n"
                                   (if vr-striproj
                                       (format "[%s] %s" symbl vfile) vfile))
                  ind-next (format "edge [color=%s];\n" color)
                  ind-next (format "node [color=%s];\n" color))
          ;; First pass define nodes
          ;; - Create subsubgraphs based on keys in `vr-subclust'.
          (dolist (subg subg-keys)
            (funcall fn-subgraph hashtable funcmap entry subg next-ind))
          ;;
          ;; Second pass define intrafile links
          (maphash
           (lambda (funcname info)
             ;; Only process functions from VFILE
             (let ((oname (funcall fn-newnames funcname vfile symbl))
                   (vment (plist-get info :mentions)))
               (if (eq (plist-get info :file) vfile)
                   (dolist (mento vment)
                     (unless (eq funcname mento)
                       (let* ((mento-info (gethash mento hashtable))
                              (mento-file (plist-get mento-info :file)))
                         ;; Only use functions are from the same file
                         (if (string= vfile mento-file)
                             (insert
                              ind-next (format
                                        "\"%s\" -> \"%s\";\n"
                                        oname
                                        (funcall fn-newnames
                                                 mento vfile symbl))))))))))
           hashtable)
          (insert ind-prev "}\n"))))))


(defun elisp-depmap-graph--makedigraphcrossinglinks (hashtable filemap ind)
  "Make digraph across clusters from HASHTABLE and FILEMAP, with IND indent."
  (let ((indent (make-string ind ? )))
    (maphash
     (lambda (funcname info)
       (let ((vfile (plist-get info :file))
             (vment (plist-get info :mentions)))
         (let* ((ventry (--first (string= (plist-get it :file) vfile) filemap))
                (vsymbl (plist-get ventry :symbol))
                (oname (elisp-depmap-graph--newname funcname vfile vsymbl)))
           (dolist (mento vment)
             (unless (eq funcname mento)
               (let* ((mento-info (gethash mento hashtable))
                      (mento-file (plist-get mento-info :file))
                      (mento-entr (--first (string= (plist-get it :file) mento-file)
                                           filemap))
                      (mento-symb (plist-get mento-entr :symbol)))
                 ;; If functions are NOT from the same file,
                 ;; list them here.
                 (unless (string= vfile mento-file)
                   (insert indent
                           (format
                            "\"%s\" -> \"%s\";\n"
                            oname
                            (elisp-depmap-graph--newname mento
                                                         mento-file
                                                         mento-symb))))))))))
     hashtable)))


(provide 'elisp-depmap-graph)
;;; elisp-depmap-graph.el ends here
