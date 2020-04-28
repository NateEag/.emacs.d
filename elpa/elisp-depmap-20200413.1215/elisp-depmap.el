;;; elisp-depmap.el --- Generate an elisp dependency map in graphviz -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/elisp-depmap.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash "2.17.0"))
;; Version: 0.1

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

;; This package generates a graphviz DOT map of all the functions and variables
;; in a given elisp project.  The aim is to help developers see how tangled their
;; code is and to help them refactor it.

;;; Code:
(require 'elisp-depmap-graph)
(require 'elisp-depmap-exec)

(require 'org-table)
(require 'subr-x)

;;;###autoload
(defun elisp-depmap-makesummarytable ()
  "Make a summary org table of variables and references to them."
  (interactive)
  (let ((hashtable (elisp-depmap-parse--generatemap)))
    (with-current-buffer
        (find-file (format "%s.%s"
                           (car (split-string elisp-depmap-exec-file "\\."))
                           "org"))
      (erase-buffer)
      (insert "| Type | #Lines | Name | File | #Mentions | Mentions |\n|--\n")
      (maphash
       (lambda (funcname info)
         (let ((vfile (plist-get info :file))
               (vbegs (plist-get info :line-beg))
               (vends (plist-get info :line-end))
               (vtype (plist-get info :type))
               (vment (--filter (not (string= funcname it))
                                (plist-get info :mentions))))
           (insert (format "| %s | %d | %s | %s | %d | %s |\n"
                           vtype
                           (if vends (- vends vbegs) 1)
                           funcname
                           vfile
                           (length vment)
                           vment))))
       hashtable)
      (org-table-align)
      (save-buffer))))


;;;###autoload
(defun elisp-depmap-graphviz-digraph (&optional shuffle)
  "Make a dot file representation of all definitions and references.
Optionally set INDENT-WIDTH which is 2 by default.
If SHUFFLE gives a random seed (default 0) to shuffle subgraph cluster layouts."
  (interactive)
  (let ((hashtable (elisp-depmap-parse--generatemap shuffle))
        (fn-decorate #'elisp-depmap-graph--decorate)
        (fn-digraph #'elisp-depmap-graph--makedigraphgroups)
        (fn-dicross #'elisp-depmap-graph--makedigraphcrossinglinks)
        (fn-execshow #'elisp-depmap-exec--executeandshow)
        (vr-funcmap elisp-depmap-parse-function-shapes)
        (vr-indwidth elisp-depmap-graph-indentwidth))
    (let ((filemap (elisp-depmap-graph--makefilemapcolors hashtable))
          (ind-now (make-string vr-indwidth ? ))
          (decor-graph (funcall fn-decorate :graph))
          (decor-node (funcall fn-decorate :node))
          (decor-edge (funcall fn-decorate :edge)))
      (with-current-buffer (find-file-noselect elisp-depmap-exec-file)
        (erase-buffer)
        (insert "digraph G {\n")
        (insert (format "%sgraph %s;\n" ind-now decor-graph))
        (if decor-node (insert (format "%snode %s;\n" ind-now decor-node)))
        (if decor-edge (insert (format "%sedge %s;\n" ind-now decor-edge)))
        ;; --{external inserts}--
        (funcall fn-digraph hashtable filemap vr-funcmap vr-indwidth)
        (funcall fn-dicross hashtable filemap vr-indwidth)
        ;; --
        (insert "}\n")
        (save-buffer)
        (funcall fn-execshow)))))


;;;###autoload
(defun elisp-depmap-graphviz ()
  "Make a very basic dot file representation of all the top level definitions in a project, and their references."
  (interactive)
  (let ((hashtable (elisp-depmap-parse--generatemap)))
    (let ((filemap (elisp-depmap-graph--makefilemapcolors hashtable))
          (funcmap elisp-depmap-parse-function-shapes))
      (with-current-buffer (find-file-noselect elisp-depmap-exec-file)
        (erase-buffer)
        (insert "strict graph {\n")
        (maphash
         (lambda (funcname info)
           (let ((vfile (plist-get info :file))
                 (vbegs (plist-get info :line-beg))
                 (vends (plist-get info :line-end))
                 (vtype (plist-get info :type))
                 (vment (plist-get info :mentions)))
             (let* ((numlines (if (and vends (not (eq vtype 'defun))) (- vends vbegs) 1))
                    (fileentry (--first (string= (plist-get it :file) vfile) filemap))
                    (filecolor (plist-get fileentry :color))
                    (filesymbl (plist-get fileentry :symbol))
                    (funcshape (alist-get (intern vtype) funcmap))
                    (linemods (1+ (/ numlines elisp-depmap-graph-linemod)))
                    (oname (elisp-depmap-graph--newname funcname vfile filesymbl)))
               (insert (format "  \"%s\" [shape=%s,color=%s,penwidth=%s]\n"
                               oname
                               funcshape
                               filecolor
                               linemods))
               (dolist (mento vment)
                 (unless (eq funcname mento)
                   (let* ((mento-entry (gethash mento hashtable))
                          (mento-file (plist-get mento-entry :file))
                          (mento-fileinfo (--first (string= (plist-get it :file)
                                                            mento-file)
                                                   filemap))
                          (mento-symb (plist-get mento-fileinfo :symbol)))
                     (insert (format "  \"%s\" -- \"%s\";\n"
                                     oname
                                     (elisp-depmap-graph--newname mento
                                                                  mento-file
                                                                  mento-symb)))))))))
         hashtable)
        (insert "}\n")
        (save-buffer)
        (elisp-depmap-exec--executeandshow)))))


;; TODO:
;;  * Implement arrows between clusters to show how
;;    the 'requires and 'provide work

(provide 'elisp-depmap)
;;; elisp-depmap.el ends here

