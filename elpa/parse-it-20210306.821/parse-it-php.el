;;; parse-it-php.el --- Core parser for PHP  -*- lexical-binding: t; -*-

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
;; Core parser for PHP.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-php--token-type
  '(("COMMENT" . "[#]")
    ("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[=][>]")
    ("VARIABLE" . "[$][^ \t\n]*")
    ("CONSTANT" . "\\<\\(__CLASS__\\|__DIR__\\|__FILE__\\|__FUNCTION__\\|__LINE__\\|__METHOD__\\|__NAMESPACE__\\|__TRAIT__\\)")
    ("KEYWORD" . "\\<\\(abstract\\|and\\|as\\|break\\|callable\\|case\\|catch\\|class\\|clone\\|const\\|continue\\|declare\\|default\\|do\\|echo\\|else\\|elseif\\|enddeclare\\|endfor\\|endforeach\\|endif\\|endswitch\\|endwhile\\|extends\\|final\\|finally\\|foreach\\|for\\|function\\|global\\|goto\\|if\\|implements\\|include\\|include_once\\|instanceof\\|insteadof\\|interface\\|namespace\\|new\\|or\\|print\\|private\\|protected\\|public\\|require\\|require_once\\|return\\|static\\|switch\\|throw\\|trait\\|try\\|use\\|var\\|while\\|xor\\|yield from\\|yield\\)"))
  "PHP token type.")

(defun parse-it-php--make-token-type ()
  "Make up the token type."
  (append parse-it-php--token-type
          parse-it-c--c-type-comment-token-type
          parse-it-c--bracket-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc-dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-c--c-type-logical-operators-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-php (path)
  "Parse the PATH PHP."
  (let* ((parse-it-lex--token-type (parse-it-php--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-php)
;;; parse-it-php.el ends here
