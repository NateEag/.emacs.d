;;; parse-it-go.el --- Core parser for Go  -*- lexical-binding: t; -*-

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
;; Core parser for Go.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-go--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\<\\(break\\|default\\|func\\|interface\\|select\\|case\\|defer\\|goto\\|map\\|struct\\|chan\\|else\\|go\\|package\\|switch\\|const\\|fallthrough\\|if\\|range\\|type\\|continue\\|for\\|import\\|return\\|var\\)"))
  "Go token type.")

(defun parse-it-go--make-token-type ()
  "Make up the token type."
  (append parse-it-go--token-type
          parse-it-c--c-type-comment-token-type
          parse-it-c--bracket-token-type
          parse-it-c--macro-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-c--c-type-logical-operators-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-go (path)
  "Parse the PATH Go."
  (let* ((parse-it-lex--token-type (parse-it-go--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-go)
;;; parse-it-go.el ends here
