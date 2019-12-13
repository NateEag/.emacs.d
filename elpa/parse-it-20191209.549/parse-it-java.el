;;; parse-it-java.el --- Core parser for Java  -*- lexical-binding: t; -*-

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
;; Core parser for Java.
;;

;;; Code:

(require 'parse-it-c)


(defconst parse-it-java--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\<\\(abstract\\|assert\\|boolean\\|break\\|byte\\|case\\|catch\\|char\\|class\\|const\\|continue\\|default\\|do\\|double\\|else\\|enum\\|extends\\|final\\|finally\\|float\\|for\\|goto\\|if\\|implements\\|import\\|instanceof\\|int\\|interface\\|long\\|native\\|new\\|package\\|private\\|protected\\|public\\|return\\|short\\|static\\|strictfp\\|super\\|switch\\|synchronized\\|this\\|throw\\|throws\\|transient\\|try\\|void\\|volatile\\|while\\|true\\|false\\|null\\)"))
  "Java token type.")


(defun parse-it-java--make-token-type ()
  "Make up the token type."
  (append parse-it-java--token-type
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

(defun parse-it-java (path)
  "Parse the PATH Java."
  (let* ((parse-it-lex--token-type (parse-it-java--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))


(provide 'parse-it-java)
;;; parse-it-java.el ends here
