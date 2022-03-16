;;; parse-it-util.el --- Util define  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Shen, Jen-Chieh <jcs090218@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Util define.
;;

;;; Code:

(require 'cl-lib)
(require 'rect)
(require 'subr-x)

(require 's)

(defun parse-it-util--get-string-from-file (path)
  "Return PATH file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun parse-it-util--get-string-from-buffer (buf-name)
  "Return BUF-NAME file content."
  (with-current-buffer buf-name
    (buffer-string)))

(defun parse-it-util--add-to-list (lst elm)
  "Append ELM to LST."
  (append lst (list elm)))

(defun parse-it-util--is-contain-list-string (in-list in-str)
  "Check if a string IN-STR contain in any string in the string list IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun parse-it-util--remove-nth-element (nth lst)
  "Remove NTH element from the LST and return the list."
  (if (zerop nth)
      (cdr lst)
    (let ((last (nthcdr (1- nth) lst)))
      (setcdr last (cddr last))
      lst)))

(defun parse-it-util--print-token-list (token-list)
  "Print out the TOKEN-LIST."
  (dolist (token token-list)
    (message "%s" token)))

(defun parse-it-util--print-ast-tree (ast-tree &optional ind st-ind)
  "Print out the AST-TREE with indentation (IND) and started-indentation (ST-IND)."
  (unless st-ind (setq st-ind 0))
  (unless ind (setq ind 2))
  (dolist (node ast-tree)
    (let ((node-type (car node)) (node-val (cdr node)))
      (if (listp node-val)
          (progn
            (if (= 0 (length node-val))
                (message "%s(%s)" (spaces-string st-ind) node-type)
              (message "%s(%s" (spaces-string st-ind) node-type)
              (setq st-ind (+ st-ind ind))
              (parse-it-util--print-ast-tree node-val ind st-ind)
              (setq st-ind (- st-ind ind))
              (message "%s)" (spaces-string st-ind))))
        (message "%s%s" (spaces-string st-ind) node)))))

(provide 'parse-it-util)
;;; parse-it-util.el ends here
