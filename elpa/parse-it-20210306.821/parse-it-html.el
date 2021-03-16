;;; parse-it-html.el --- Core parser for HTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Core parser for html.
;;

;;; Code:

(require 'parse-it)

(defconst parse-it-html--token-type
  '(("COMMENT_BEG" . "[<][!][-][-]")
    ("COMMENT_END" . "[-][-][>]")
    ("TAG_BEG" . "\\([<]\\)[^!][^-][^-]")
    ("TAG_BEG" . "\\([<]\\)[!][^-]")
    ("TAG_END" . "[^-][^-]\\([>]\\)"))
  "HTML token type.")

(defconst parse-it-html--into-level-symbols
  '("TAG_BEG")
  "All symbols that goes into one nested level.")

(defconst parse-it-html--back-level-symbols
  '("TAG_END")
  "All symbols that goes back up one nested level.")

(defun parse-it-html--make-token-type ()
  "Make up the token type."
  (append parse-it-html--token-type
          parse-it-lex--token-type))

(defun parse-it-html (path)
  "Parse the PATH HTML."
  (let* ((parse-it-lex--token-type (parse-it-html--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-html--into-level-symbols
                        parse-it-html--back-level-symbols)))

(provide 'parse-it-html)
;;; parse-it-html.el ends here
