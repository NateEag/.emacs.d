;;; parse-it-python.el --- Core parser for Python  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Core parser for Python.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-python--token-type
  '(("COMMENT" . "[#]")
    ("COMMENT_BEG" . "[\"][\"][\"]")
    ("COMMENT_END" . "[\"][\"][\"]")
    ("COMMENT_BEG" . "[']['][']")
    ("COMMENT_END" . "[']['][']")
    ("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[^']\\([']\\)[^']")
    ("QT_S" . "[^'][']\\([']\\)[^']")
    ("QT_S" . "[^']\\([']\\)['][^']")
    ("QT_D" . "[^\"]\\([\"]\\)[^\"]")
    ("QT_D" . "[^\"][\"]\\([\"]\\)[^\"]")
    ("QT_D" . "[^\"]\\([\"]\\)[\"][^\"]")
    ("ARROW" . "[=][>]")
    ("KEYWORD" . "\\<\\(break\\|case\\|catch\\|continue\\|debugger\\|default\\|delete\\|do\\|else\\|finally\\|for\\|function\\|if\\|in\\|instanceof\\|new\\|return\\|switch\\|this\\|throw\\|try\\|typeof\\|var\\|void\\|while\\|with\\|null\\|true\\|false\\|NaN\\|Infinity\\|undefined\\)"))
  "Python token type.")

(defconst parse-it-python--bracket-token-type
  '(("BRKT_SQ_OPN" . "[\\[]")
    ("BRKT_SQ_CLS" . "[]]")
    ("PAREN_OPN" . "[(]")
    ("PAREN_CLS" . "[)]"))
  "Python bracket token type.")

(defun parse-it-python--make-token-type ()
  "Make up the token type."
  (append parse-it-python--token-type
          parse-it-python--bracket-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-python (path)
  "Parse the PATH Python."
  (let* ((parse-it-lex--token-type (parse-it-python--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-indent-build path
                               token-list
                               parse-it-c--into-level-symbols
                               parse-it-c--back-level-symbols)))

(provide 'parse-it-python)
;;; parse-it-python.el ends here
