;;; parse-it-csharp.el --- Core parser for CSharp  -*- lexical-binding: t; -*-

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
;; Core parser for CSharp.
;;

;;; Code:

(require 'parse-it-c)


(defconst parse-it-csharp--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[=][>]")
    ("KEYWORD" . "\\<\\(abstract\\|as\\|base\\|bool\\|break\\|byte\\|case\\|catch\\|char\\|checked\\|class\\|const\\|continue\\|decimal\\|default\\|delegate\\|do\\|double\\|else\\|enum\\|event\\|explicit\\|extern\\|false\\|finally\\|fixed\\|float\\|foreach\\|for\\|goto\\|if\\|implicit\\|interface\\|internal\\|int\\|in\\|is\\|lock\\|long\\|namespace\\|new\\|null\\|object\\|operator\\|out\\|override\\|params\\|private\\|protected\\|public\\|readonly\\|ref\\|return\\|sbyte\\|sealed\\|short\\|sizeof\\|stackalloc\\|static\\|string\\|struct\\|switch\\|this\\|throw\\|true\\|try\\|typeof\\|uint\\|ulong\\|unchecked\\|unsafe\\|ushort\\|using\\|virtual\\|void\\|volatile\\|while\\)"))
  "CSharp token type.")


(defun parse-it-csharp--make-token-type ()
  "Make up the token type."
  (append parse-it-csharp--token-type
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

(defun parse-it-csharp (path)
  "Parse the PATH CSharp."
  (let* ((parse-it-lex--token-type (parse-it-csharp--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))


(provide 'parse-it-csharp)
;;; parse-it-csharp.el ends here
