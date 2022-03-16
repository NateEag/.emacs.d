;;; parse-it-c.el --- Core parser for C  -*- lexical-binding: t; -*-

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
;; Core parser for C.
;;

;;; Code:

(require 'parse-it)

(defconst parse-it-c--token-type
  '(("ASK" . "[^/*]\\([*]\\)[^/*]")
    ("AMP" . "[^&]\\([&]\\)[^&]")
    ("PTR" . "[-][>]")
    ("KEYWORD" . "\\<\\(auto\\|break\\|case\\|char\\|const\\|continue\\|default\\|double\\|do\\|else\\|enum\\|extern\\|float\\|for\\|goto\\|if\\|int\\|long\\|register\\|return\\|short\\|signed\\|sizeof\\|static\\|struct\\|switch\\|typedef\\|union\\|unsigned\\|void\\|volatile\\|while\\)"))
  "C token type.")

(defconst parse-it-c--c-type-comment-token-type
  '(("COMMENT" . "[/][/]")     ; //
    ("COMMENT_BEG" . "/[*]+")  ; /*
    ("COMMENT_END" . "[*]/"))  ; */
  "C type comment token type.")

(defconst parse-it-c--bracket-token-type
  '(("BRKT_CR_OPN" . "[{]")    ; {
    ("BRKT_CR_CLS" . "[}]")    ; }
    ("BRKT_SQ_OPN" . "[\\[]")  ; [
    ("BRKT_SQ_CLS" . "[]]")    ; ]
    ("PAREN_OPN" . "[(]")      ; (
    ("PAREN_CLS" . "[)]"))     ; )
  "C type bracket token type.")

(defconst parse-it-c--macro-token-type
  '(("PRE_DEF" . "[#]"))
  "C type macro token type.")

(defconst parse-it-c--c-type-arithmetic-operators-token-type
  '(("AR_OP" . "[+-*%]")                ; + - * %
    ("AR_OP" . "[^/*]\\([/]\\)[^/*]"))  ; For `/' divide symbol.
  "C type arithmetic operators token type.")

(defconst parse-it-c--c-type-inc-dec-operators-token-type
  '(("ID_OP" . "[-][-]")   ; --
    ("ID_OP" . "[+][+]"))  ; ++
  "C type increment/decrement operators token type.")

(defconst parse-it-c--c-type-assignment-operators-token-type
  '(("AS_OP" . "[+][=]")              ; +=
    ("AS_OP" . "[-][=]")              ; -=
    ("AS_OP" . "[*][=]")              ; *=
    ("AS_OP" . "[/][=]")              ; /=
    ("AS_OP" . "[%][=]")              ; %=
    ("AS_OP" . "[^+-*/%]\\([=]\\)"))  ; =
  "C type assignment operators token type.")

(defconst parse-it-c--c-type-relational-operators-token-type
  '(("RE_OP" . "[!][=]")              ; !=
    ("RE_OP" . "[=][=]")              ; ==
    ("RE_OP" . "[>][=]")              ; >=
    ("RE_OP" . "[<][=]")              ; <=
    ("RE_OP" . "[^<]\\([<]\\)[^<]")   ; <
    ("RE_OP" . "[^>]\\([>]\\)[^>]"))  ; >
  "C type relational operators token type.")

(defconst parse-it-c--c-type-logical-operators-token-type
  '(("LG_OP" . "[&][&]")          ; &&
    ("LG_OP" . "[|][|]")          ; ||
    ("LG_OP" . "\\([!]\\)[^=]"))  ; !
  "C type logical operators token type.")

(defconst parse-it-c--c-type-bitwise-operators-token-type
  '(("BT_OP" . "[^&]\\([&]\\)[^&]")  ; &
    ("BT_OP" . "[^|]\\([|]\\)[^|]")  ; |
    ("BT_OP" . "[\\^]")              ; ^
    ("BT_OP" . "[~]")                ; ~
    ("BT_OP" . "[<][<]")             ; <<
    ("BT_OP" . "[>][>]"))            ; >>
  "C type bitwise operators token type.")

(defconst parse-it-c--into-level-symbols
  '("BRKT_CR_OPN" "BRKT_SQ_OPN" "PAREN_OPN")
  "All symbols that goes into one nested level.")

(defconst parse-it-c--back-level-symbols
  '("BRKT_CR_CLS" "BRKT_SQ_CLS" "PAREN_CLS")
  "All symbols that goes back up one nested level.")

(defun parse-it-c--make-token-type ()
  "Make up the token type."
  (append parse-it-c--token-type
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

(defun parse-it-c (path)
  "Parse the PATH in C."
  (let* ((parse-it-lex--token-type (parse-it-c--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-c)
;;; parse-it-c.el ends here
