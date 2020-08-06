;;; parse-it-lua.el --- Core parser for Lua  -*- lexical-binding: t; -*-

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
;; Core parser for Lua.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-lua--token-type
  '(("COMMENT" . "[-][-]")
    ("COMMENT_BEG" . "[-][-][[][[]")
    ("COMMENT_END" . "[]][]][-][-]")
    ("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\<\\(break\\|case\\|catch\\|continue\\|debugger\\|default\\|delete\\|do\\|else\\|finally\\|for\\|function\\|if\\|instanceof\\|in\\|new\\|return\\|switch\\|this\\|throw\\|try\\|typeof\\|var\\|void\\|while\\|with\\|null\\|true\\|false\\|NaN\\|Infinity\\|undefined\\)"))
  "Lua token type.")

(defun parse-it-lua--make-token-type ()
  "Make up the token type."
  (append parse-it-lua--token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-lua (path)
  "Parse the PATH Lua."
  (let* ((parse-it-lex--token-type (parse-it-lua--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-lua)
;;; parse-it-lua.el ends here
