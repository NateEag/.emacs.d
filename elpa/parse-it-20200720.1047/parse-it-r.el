;;; parse-it-r.el --- Core parser for R  -*- lexical-binding: t; -*-

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
;; Core parser for R.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-r--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[<][-]")
    ("KEYWORD" . "\\<\\(if\\|else\\|repeat\\|while\\|function\\|for\\|in\\|next\\|break\\|TRUE\\|FALSSE\\|NULL\\|Inf\\|NaN\\|NA\\|NA_integer_\\|NA_real_\\|NA_complex_\\|NA_character_\\)"))
  "R token type.")

(defconst parse-it-r--arithmetic-operators-token-type
  '(("AR_OP" . "[+-*^]")                ; + - * ^
    ("AR_OP" . "[^/*]\\([/]\\)[^/*]")   ; For `/' divide symbol.
    ("AR_OP" . "[%][%]"))               ; %%
  "R arithmetic operators token type.")

(defconst parse-it-r--assignment-operators-token-type
  '(("AS_OP" . "[<][-]")              ; <-
    ("AS_OP" . "[<][<][-]")           ; <<-
    ("AS_OP" . "[-][>]")              ; ->
    ("AS_OP" . "[-][>][>]")           ; ->>
    ("AS_OP" . "[^+-*/%]\\([=]\\)"))  ; =
  "R assignment operators token type.")

(defconst parse-it-r--logical-operators-token-type
  '(("LG_OP" . "[^&]\\([&]\\)[^&]")  ; &
    ("LG_OP" . "[&][&]")             ; &&
    ("LG_OP" . "[|]\\([|]\\)[|]")    ; |
    ("LG_OP" . "[|][|]")             ; ||
    ("LG_OP" . "\\([!]\\)[^=]"))     ; !
  "R logical operators token type.")

(defun parse-it-r--make-token-type ()
  "Make up the token type."
  (append parse-it-r--token-type
          parse-it-c--c-type-comment-token-type
          parse-it-c--bracket-token-type
          parse-it-r--arithmetic-operators-token-type
          parse-it-r--assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-r--logical-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-r (path)
  "Parse the PATH R."
  (let* ((parse-it-lex--token-type (parse-it-r--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-r)
;;; parse-it-r.el ends here
