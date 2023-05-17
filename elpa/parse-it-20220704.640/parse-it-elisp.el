;;; parse-it-elisp.el --- Core parser for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Core parser for Emacs Lisp.
;;

;;; Code:

(require 'parse-it-lisp)

(defconst parse-it-elisp--token-type
  '()
  "Lisp token type.")

(defun parse-it-elisp--make-token-type ()
  "Make up the token type."
  (append parse-it-elisp--token-type
          parse-it-lisp--comment-token-type
          parse-it-lex--token-type))

(defun parse-it-elisp (path)
  "Parse the PATH Emacs Lisp."
  (let* ((parse-it-lex--token-type (parse-it-elisp--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-lisp--into-level-symbols
                        parse-it-lisp--back-level-symbols)))

(provide 'parse-it-elisp)
;;; parse-it-elisp.el ends here
