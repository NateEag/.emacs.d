;;; parse-it-typescript.el --- Core parser for TypeScript  -*- lexical-binding: t; -*-

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
;; Core parser for TypeScript.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-typescript--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[=][>]")
    ("KEYWORD" . "\\<\\(abstract\\|any\\|async\\|as\\|await\\|boolean\\|bigint\\|break\\|case\\|catch\\|class\\|constructor\\|const\\|continue\\|declare\\|default\\|delete\\|do\\|else\\|enum\\|export\\|extends\\|extern\\|false\\|finaly\\|for\\|function\\|from\\|get\\|goto\\|if\\|implements\\|import\\|in\\|instanceof\\|interface\\|keyof\\|let\\|module\\|namespace\\|never\\|new\\|null\\|number\\|object\\|of\\|private\\|protected\\|public\\|readonly\\|return\\|set\\|static\\|string\\|super\\|switch\\|this\\|throw\\|true\\|try\\|typeof\\|type\\|var\\|void\\|while\\)"))
  "TypeScript token type.")

(defun parse-it-typescript--make-token-type ()
  "Make up the token type."
  (append parse-it-typescript--token-type
          parse-it-c--c-type-comment-token-type
          parse-it-c--bracket-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-c--c-type-logical-operators-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-typescript (path)
  "Parse the PATH TypeScript."
  (let* ((parse-it-lex--token-type (parse-it-typescript--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-typescript)
;;; parse-it-typescript.el ends here
