;;; parse-it-typescript.el --- Core parser for TypeScript  -*- lexical-binding: t; -*-

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
;; Core parser for TypeScript.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-sql--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("KEYWORD" . "\\<\\(ADD[ \t\r\n]+CONSTRAINT\\|ADD\\|ALTER[ \t\r\n]+COLUMN\\|ALTER[ \t\r\n]+TABLE\\|ALTER\\|ALL\\|ANY\\|ASC\\|AS\\|BACKUP[ \t\r\n]+DATABASE\\|BETWEEN\\)")
    ("KEYWORD" . "\\<\\(CASE\\|CHECK\\|COLUMN\\|CONSTRAINT\\|CREATE[ \t\r\n]+DATABASE\\|CREATE[ \t\r\n]+INDEX\\|CREATE[ \t\r\n]+OR[ \t\r\n]+REPLACE[ \t\r\n]+VIEW\\|CREATE[ \t\r\n]+TABLE\\|CREATE[ \t\r\n]+PROCEDURE\\|CREATE[ \t\r\n]+UNIQUE[ \t\r\n]+INDEX\\|CREATE[ \t\r\n]+VIEW\\|CREATE\\|DATABASE\\|DEFAULT\\|DELETE\\|DESC\\|DISTINCT\\|DROP[ \t\r\n]+COLUMN\\|DROP[ \t\r\n]+CONSSTRAINT\\|DROP[ \t\r\n]+DATABASE\\|DROP[ \t\r\n]+DEFAULT\\|DROP[ \t\r\n]+INDEX\\|DROP[ \t\r\n]+TABLE\\|DROP[ \t\r\n]+VIEW\\|DROP\\)")
    ("KEYWORD" . "\\<\\(EXEC\\|EXISTS\\|FOREIGN KEY\\|FROM\\|FULL[ \t]+JOIN\\|GROUP[ \t]+BY\\|HAVING\\|INDEX\\|INDEX\\|INNER[ \t]+JOIN\\|INSERT[ \t]+INTO[ \t]+SELECT\\|INSERT[ \t]+INTO\\|IS[ \t]+NULL\\|IS[ \t]+NOT[ \t]+NULL\\)")
    ("KEYWORD" . "\\<\\(JOIN\\|LEFT[ \t\r\n]+JOIN\\|LIKE\\|LIMIT\\|NOT\\|NOT NULL\\|OR\\|ORDER[ \t\r\n]+BY\\|OUTER[ \t\r\n]+JOIN\\|PRIMARY[ \t\r\n]+KEY\\|PROCEDURE\\|RIGHT[ \t\r\n]+JOIN\\|ROWNUM\\|SELECT[ \t\r\n]+DISTINCT\\|SELECT\\|SELECT[ \t\r\n]+INTO\\|SELECT[ \t\r\n]+INTO\\|SELECT[ \t\r\n]+TOP\\|SET\\)")
    ("KEYWORD" . "\\<\\(TABLE\\|TOP\\|TRUNCATE[ \t\r\n]+TABLE\\|UNION[ \t\r\n]+ALL\\|UNION\\|UNIQUE\\|UPDATE\\|VALUES\\|VIEW\\|WHERE\\)"))
  "SQL token type.")

(defconst parse-it-sql--comment-token-type
  '(("COMMENT" . "[-][-]")     ; --
    ("COMMENT_BEG" . "/[*]+")  ; /*
    ("COMMENT_END" . "[*]/"))  ; */
  "SQL comment token type.")

(defun parse-it-sql--make-token-type ()
  "Make up the token type."
  (append parse-it-sql--token-type
          parse-it-sql--comment-token-type
          parse-it-c--bracket-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-c--c-type-logical-operators-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))
(defun parse-it-sql (path)
  "Parse the PATH SQL."
  (let* ((parse-it-lex--token-type (parse-it-sql--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-sql)
;;; parse-it-sql.el ends here
