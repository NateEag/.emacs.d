;;; parse-it-c++.el --- Core parser for C++  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Shen, Jen-Chieh <jcs090218@gmail.com>

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
;; Core parser for C++.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-c++--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\<\\(asm\\|bool\\|catch\\|class\\|const_cast\\|delete\\|dynamic_cast\\|explicit\\|export\\|false\\|friend\\|inline\\|mutable\\|namespace\\|new\\|operator\\|private\\|protected\\|public\\|reinterpret_cast\\|static_cast\\|template\\|this\\|throw\\|true\\|try\\|typeid\\|typename\\|using\\|virtual\\|wchar_t\\|nullptr\\)"))
  "C++ token type.")

(defun parse-it-c++--make-token-type ()
  "Make up the token type."
  (append parse-it-c++--token-type
          parse-it-c--token-type
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

(defun parse-it-c++ (path)
  "Parse the PATH C++."
  (let* ((parse-it-lex--token-type (parse-it-c++--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-c++)
;;; parse-it-c++.el ends here
