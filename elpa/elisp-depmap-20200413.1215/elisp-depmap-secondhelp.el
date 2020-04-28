;;; elisp-depmap-secondhelp.el --- Helper functions for parse library -*- lexical-binding: t; -*-

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

;; See elisp-depmap.el

;;; Code:
(require 'cl-lib)
(require 'dash)

(defgroup elisp-depmap nil
  "Main group for elisp-depmap package."
  :group 'coding)

(defcustom elisp-depmap-secondhelp-regexreferences
  "\\( \\|(\\|\\b\\|'\\)%s\\( \\|)\\|\\b\\)"
  "Regex to find references to a definition."
  :type 'string
  :group 'elisp-depmap)

(defsubst elisp-depmap-secondhelp--generateregexfromalist (alist)
  "From ALIST, get the car variables and put them in a regex.
This will be used to scan all files for top level definitions."
  (concat "^(\\(cl-\\)?\\(" (mapconcat (lambda (x) (format "%s" (car x)))
                                       alist "\\|") "\\)"))

(defun elisp-depmap-secondhelp--callingfuncatline (lnum file list-asc)
  "Retrieve the function name in LIST-ASC that LNUM bisects in FILE."
  (let ((func nil))
    (dolist (elm list-asc)
      (let ((func-lbeg (1+ (nth 0 elm)))
            (func-lend (nth 1 elm))
            (func-name (nth 2 elm))
            (func-file (nth 3 elm)))
        ;; If current line number in file matches the
        ;; flanking beg and end in the list (and the file)
        (if (and (string= file func-file)
                 (<= func-lbeg lnum func-lend))
            (cl-pushnew func-name func))))
    (if func
        (if (> 1 (length func))
            (error "Multiple functions at line... %d: %s" lnum func)
          (car func)))))

(defun elisp-depmap-secondhelp--makesortedlinelist (hashtable)
  "Make an ascending list of the start and end positions of all functions from HASHTABLE."
  (let ((funcsbylinenum nil))
    (maphash
     (lambda (nam vals)
       (let ((lbeg (plist-get vals :line-beg))
             (lend (plist-get vals :line-end))
             (file (plist-get vals :file)))
         ;; We only want functions (those with a lend)
         (if lend
             (cl-pushnew `(,lbeg ,lend ,nam ,file) funcsbylinenum))))
     hashtable)
    (--sort (< (car it) (car other)) funcsbylinenum)))

(defun elisp-depmap-secondhelp--updatementionslist (vname file annotations funcs-by-line-asc)
  "Update mentions list from ANNOTATIONS for variable VNAME by checking in ASCLIST of line numbers for function bounds in FILE."
  (let ((vnam-regex (format elisp-depmap-secondhelp-regexreferences vname))
        (mentionlst (plist-get annotations :mentions))
        (vnam-line (plist-get annotations :line-beg)))
    (save-excursion
      (goto-char 0)
      (while (search-forward-regexp vnam-regex nil t)
        (let ((lnum (line-number-at-pos)))
          (unless (eq lnum vnam-line)
            ;; skip the top level definition
            (let ((called-func (elisp-depmap-secondhelp--callingfuncatline
                                lnum
                                file
                                funcs-by-line-asc)))
              (if called-func
                  (push called-func mentionlst))))))
      (plist-put annotations :mentions mentionlst))))


(provide 'elisp-depmap-secondhelp)
;;; elisp-depmap-secondhelp.el ends here
