;;; parse-it-kotlin.el --- Core parser for Kotlin  -*- lexical-binding: t; -*-

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
;; Core parser for Kotlin.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-kotlin--token-type
  '(("COLON" . "[^:]\\([:]\\)[^:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[^.]\\([.]\\)[^.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\B\\(!in\\|!is\\)\\b")
    ("KEYWORD" . "\\<\\(as\\?\\|as\\|break\\|class\\|continue\\|do\\|else\\|false\\|for\\|fun\\|if\\|interface\\|\\in\\|is\\|null\\|object\\|package\\|return\\|super\\|this\\|throw\\|true\\|try\\|typealias\\|typeof\\|val\\|var\\|when\\|while\\|by\\|catch\\|constructor\\|delegate\\|dynamic\\|field\\|file\\|finally\\|get\\|import\\|init\\|param\\|property\\|receiver\\|set\\|setparam\\|where\\|actual\\|abstract\\|annotation\\|companion\\|const\\|crossinline\\|data\\|enum\\|expect\\|external\\|final\\|infix\\|inline\\|inner\\|internal\\|lateinit\\|noinline\\|open\\|operator\\|out\\|override\\|private\\|public\\|reified\\|sealed\\|suspend\\|tailrec\\|vararg\\|it\\)"))
  "Kotlin token type.")

(defconst parse-it-kotlin--ref-equ-operators-token-type
  '(("RE_OP" . "[=][=][=]")
    ("RE_OP" . "[!][=][=]"))
  "Kotlin referential equality operators token type.")

(defconst parse-it-kotlin--logical-operators-token-type
  '(("LG_OP" . "[&][&]")
    ("LG_OP" . "[|][|]")
    ("LG_OP" . "\\([!]\\)[^=!]"))
  "Kotlin logical operators token type.")

(defconst parse-it-kotlin--special-symbols-token-type
  '(("SS_OP" . "[:][:]")
    ("SS_OP" . "[!][!]")
    ("SS_OP" . "[?][.]")
    ("SS_OP" . "[?][:]")
    ("SS_OP" . "[?]")
    ("SS_OP" . "[.][.]")
    ("SS_OP" . "[-][>]")
    ("SS_OP" . "[@]")
    ("SS_OP" . "[$]")
    ("SS_OP" . "[_]"))
  "Kotlin special symbols token type.")

(defun parse-it-kotlin--make-token-type ()
  "Make up the token type."
  (append parse-it-kotlin--token-type
          parse-it-c--c-type-comment-token-type
          parse-it-c--bracket-token-type
          parse-it-c--macro-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-kotlin--ref-equ-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-kotlin--logical-operators-token-type
          parse-it-kotlin--special-symbols-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-kotlin (path)
  "Parse the PATH Kotlin."
  (let* ((parse-it-lex--token-type (parse-it-kotlin--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-kotlin)
;;; parse-it-kotlin.el ends here
