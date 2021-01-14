;;; parse-it-markdown.el --- Core parser for Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Core parser for Markdown.
;;

;;; Code:

(require 'parse-it)

(defconst parse-it-markdown--token-type
  '(("COMMENT_BEG" . "[<][!][-][-]")
    ("COMMENT_END" . "[-][-][>]")
    ("TAG_BEG" . "\\([<]\\)[^!][^-][^-]")
    ("TAG_BEG" . "\\([<]\\)[!][^-]")
    ("TAG_END" . "[^-][^-]\\([>]\\)")
    ("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("TRI_BACK_QT" . "[`][`][`]")
    ("BACK_QT" . "[^`]\\([`]\\)[^`]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]"))
  "Markdown token type.")

(defun parse-it-markdown--make-token-type ()
  "Make up the token type."
  (append parse-it-markdown--token-type
          parse-it-lex--token-type))

(defun parse-it-markdown (path)
  "Parse the PATH Markdown."
  (let* ((parse-it-lex--token-type (parse-it-markdown--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-markdown)
;;; parse-it-markdown.el ends here
