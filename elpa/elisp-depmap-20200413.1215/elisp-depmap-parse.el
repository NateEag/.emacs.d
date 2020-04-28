;;; elisp-depmap-parse.el --- Construct a hashtable of top level definitions -*- lexical-binding: t; -*-

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

;; Go through all and retrieve the top level definitions and their
;; positions then determine which functions are called by which others.


;;; Code:
(require 'elisp-depmap-secondhelp)
(require 'paren)

(defcustom elisp-depmap-parse-function-shapes
  '((setq . underline) (defvar . underline) (defcustom . plain) (defun . tab) (defsubst . component) (defmacro . trapezium) (defgeneric . trapezium) (defmethod . trapezium))
  "Define variables to look, and the graphviz shapes they should take.
More info at the https://graphviz.org/doc/info/attrs.html website."
  :type 'list
  :group 'elisp-depmap)

(defcustom elisp-depmap-parse-hashtablesize 50
  "Size of hash table.  50 by default."
  :type 'integer
  :group 'elisp-depmap)

(defun elisp-depmap-parse--getsourcefiles (&optional directory)
  "Find all source files from DIRECTORY, otherwise defer to `default-directory'."
  (let ((dir (or directory default-directory)))
    (--map (replace-regexp-in-string (format "^%s" dir) "" it)  ;; replace main directory
           (--filter (and (string-suffix-p ".el" it)            ;; don't want elc
                          (not (string-match-p "\\#" it)))      ;; don't want temp
                     (directory-files-recursively dir ".*\\.el")))))

;; ;; -- Not sure if this needs to be used. It could be useful for checking
;; ;;    import loops.
;; (defun elisp-depmap-parse--alltopdefs-file-requireprovide (file hashdefs)
;;   "Get all imports and package definitions from FILE and put into a HASHDEFS."
;;   (save-excursion
;;     (with-current-buffer (find-file-noselect file)
;;       (goto-char 0)
;;       (let ((provname nil)
;;             (mentions nil)
;;             (regit "^(\\(require\\|provide\\) '"))
;;         (while (search-forward-regexp regit nil t)
;;           ;; Get type
;;           (let* ((type-end (progn (forward-whitespace -1) (point)))
;;                  (type-beg (1+ (move-beginning-of-line 1)))
;;                  (type-nam (buffer-substring-no-properties type-beg type-end)))
;;             (goto-char type-end)
;;             (forward-whitespace 1)
;;             ;; Get variable name
;;             (let* ((req-beg (search-forward "'" (point-at-eol)))
;;                    (req-end (progn (forward-whitespace 1)
;;                                    (forward-whitespace -1)
;;                                    (search-backward ")" req-beg)))
;;                    (req-nam (buffer-substring-no-properties req-beg req-end)))
;;               ;; Make a wish make a succotash wish
;;               (cond ((string= type-nam "require") (push req-nam mentions))
;;                     ((string= type-nam "provide") (setq provname req-nam))
;;                     (t (error "Unknown: %s" type-nam))))))
;;         (if provname
;;             (puthash provname
;;                      `(:type "imports" :file ,file :mentions ,mentions)
;;                      hashdefs)
;;           (error "Unable to find provides for file %s" file))))))

(defun elisp-depmap-parse--alltopdefs-file (file hashdefs)
  "Get all top definitions in FILE and put into HASHDEFS.
Don't use `grep' or `projectile-ripgrep', because those sonuvabitch finish hooks are not reliable."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char 0)
      (let ((reg-type (elisp-depmap-secondhelp--generateregexfromalist elisp-depmap-parse-function-shapes)))
        ;;(reg-vnam "\\(-*\\w+\\)+"))
        (while (search-forward-regexp reg-type nil t)
          ;; Get type
          (let* ((type-end (point))
                 (type-beg (1+ (move-beginning-of-line 1)))
                 (type-nam (buffer-substring-no-properties type-beg type-end)))
            (goto-char type-end)
            (forward-whitespace 1)
            ;; Get variable name
            (let* ((vnam-beg (point))
                   (vnam-end (progn (forward-whitespace 1) (forward-whitespace -1) (point)))
                   (vnam-nam (buffer-substring-no-properties vnam-beg vnam-end)))
              ;; Get bounds or line number
              (let ((lnum-beg (line-number-at-pos))
                    (lnum-end nil))
                (when (string= type-nam "defun")
                  (move-beginning-of-line 1)
                  (let* ((bounk (funcall show-paren-data-function))
                         (keybl (nth 3 bounk)))
                    (goto-char keybl)
                    (setq lnum-end (line-number-at-pos))))
                (puthash vnam-nam
                         `(:type ,type-nam
                                 :line-beg ,lnum-beg
                                 :line-end ,lnum-end
                                 :file ,file
                                 ;; when mentions is nil, somehow all entries in
                                 ;; the hash table point to the same mentions.
                                 :mentions (,vnam-nam))
                         hashdefs)))))
        hashdefs))))


(defun elisp-depmap-parse--alltopdefs-filelist (filelist)
  "Get all top definitions from FILELIST and return a hashtable, with variable names as keys as well as type and bounds as values."
  (let ((hashtable (make-hash-table
                    :size elisp-depmap-parse-hashtablesize
                    :test #'equal)))
    (dolist (pfile filelist hashtable)
      ;; (elisp-depmap-parse--alltopdefs-file-requireprovide pfile hashtable)
      (elisp-depmap-parse--alltopdefs-file pfile hashtable))))



(defun elisp-depmap-parse--allsecondarydefs-file (file hashtable)
  "Get all secondary definitions in FILE for each of the top level definitions in HASHTABLE."
  (let ((funcs-by-line-asc (elisp-depmap-secondhelp--makesortedlinelist
                            hashtable)))
    ;; -- Check each top def in the buffer
    (with-current-buffer (find-file-noselect file)
      (maphash   ;; iterate hashtable
       (lambda (vname annotations)
         (elisp-depmap-secondhelp--updatementionslist vname
                                                      file
                                                      annotations
                                                      funcs-by-line-asc))
       hashtable))))


(defun elisp-depmap-parse--allsecondarydefs-filelist (filelist hashtable)
  "Get all secondary definitions for all files in FILELIST for the top level definitions in HASHTABLE."
  (dolist (pfile filelist hashtable)
    (elisp-depmap-parse--allsecondarydefs-file pfile hashtable)))


(defun elisp-depmap-parse--shuffle (lst seed)
  "Shuffle LST using SEED.  Not a true random shuffle, at all.
Deterministic rotate and cut."
  (let ((sta-ind (mod seed (length lst))) ;; first element in list
        (hlf-ind (/ (length lst) 2)))
    (let* ((rotated (append (cl-subseq lst sta-ind)
                            (cl-subseq lst 0 sta-ind)))
           (revpack (reverse rotated))
           (cutpack (append (cl-subseq revpack hlf-ind)
                            (cl-subseq revpack 0 hlf-ind))))
      cutpack)))


(defun elisp-depmap-parse--generatemap (&optional seed)
  "Generate a map of toplevel function and variable definitions in a project.
Randomise `proj-files' using SEED (default 0)."
  (let* ((proj-files (elisp-depmap-parse--shuffle
                      (elisp-depmap-parse--getsourcefiles)
                      (or seed 0)))
         (hash-table (elisp-depmap-parse--alltopdefs-filelist proj-files)))
    (elisp-depmap-parse--allsecondarydefs-filelist proj-files hash-table)
    hash-table))

(provide 'elisp-depmap-parse)
;;; elisp-depmap-parse.el ends here
