;;; parse-it-css.el --- Core parser for CSS  -*- lexical-binding: t; -*-

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
;; Core parser for CSS.
;;

;;; Code:

(require 'parse-it-c)

(defconst parse-it-css--token-type
  '(("COMMENT_BEG" . "/[*]+")
    ("COMMENT_END" . "[*]/")
    ("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[=][>]")
    ("KEYWORD" . "\\B\\(@charset\\|@font-face\\|@font-feature-values\\|@image\\|@keyframes\\|@media\\)\\b")
    ("KEYWORD" . "\\<\\(align-content\\|align-items\\|align-self\\|all\\|animation-delay\\|animation-direction\\|animation-duration\\|animation-fill-mode\\|animation-iteration-count\\|animation-name\\|animation-play-state\\|animation-timing-function\\|animation\\)")
    ("KEYWORD" . "\\<\\(backface-visibility\\|background-attachment\\|background-blend-mode\\|background-clip\\|background-color\\|background-image\\|background-origin\\|background-position\\|background-repeat\\|background-size\\|background\\|border-bottom-color\\|border-bottom-left-radius\\|border-bottom-right-radius\\|border-bottom-style\\|border-bottom\\|border-width\\|border-collapse\\|border-color\\|border-image-outset\\|border-repeat\\|border-image-slice\\|border-source\\|border-image-width\\|border-image\\|border-left-color\\|border-left-style\\|border-left-width\\|border-left\\|border-radius\\|border-right-color\\|border-right-style\\|border-right-width\\|border-right\\|border-spacing\\|border-style\\|border-top-color\\|border-top-left-radius\\|border-top-right-radius\\|border-top-style\\|border-top-width\\|border-top-style\\|border-width\\|border-color\\|border\\|bottom\\|box-decoration-break\\|box-shadow\\|box-sizing\\|break-after\\|break-before\\|break-inside\\)")
    ("KEYWORD" . "\\<\\(caption-size\\|caret-color\\|clear\\|clip\\|color\\|column-count\\|column-fill\\|column-gap\\|column-rule-color\\|column-rule-style\\|column-rule-width\\|column-rule\\|column-span\\|column-width\\|columns\\|content\\|counter-increment\\|counter-reset\\|cursor\\)")
    ("KEYWORD" . "\\<\\(direction\\|display\\|empty-cells\\)")
    ("KEYWORD" . "\\<\\(filter\\|flex-basis\\|flex-direction\\|flex-flow\\|flex-grow\\|flex-shrink\\|flex-wrap\\|flex\\|float\\|font-family\\|font-feature-settings\\|font-kerning\\|font-language-override\\|font-size-adjust\\|font-size\\|font-stretch\\|font-style\\|font-synthesis\\|font-variant-alternates\\|font-variant-caps\\|font-variant-east-asian\\|font-variant-ligatures\\|font-variant-numeric\\|font-variant-position\\|font-variant\\|font-weight\\|font\\)")
    ("KEYWORD" . "\\<\\(grid-area\\|grid-auto-columns\\|grid-auto-flow\\grid-auto-rows\\|grid-column-end\\|grid-column-gap\\|grid-column-start\\|grid-column\\|grid-gap\\|grid-row-end\\|grid-row-gap\\|grid-row-start\\|grid-row\\|grid-template-areas\\|grid-template-columns\\|grid-template-rows\\|grid-template\\|grid\\)")
    ("KEYWORD" . "\\<\\(hanging-punctuation\\|height\\|hyphens\\image-rendering\\|isolation\\|justify-content\\|left\\|letter-spacing\\|line-break\\|line-height\\|list-style-image\\|list-style-position\\|list-style-type\\|list-style\\)")
    ("KEYWORD" . "\\<\\(margin-bottom\\|margin-left\\|margin-right\\margin-top\\|margin\\|max-height\\|max-width\\|min-height\\|min-width\\|mix-blend-mode\\|object-fit\\|object-position\\|opacity\\|order\\|orphans\\|outline-color\\|outline-offset\\|outline-style\\|outline-width\\|outline\\|overflow-wrap\\|overflow-x\\|overflow-y\\|overflow\\)")
    ("KEYWORD" . "\\<\\(padding-bottom\\|padding-left\\|padding-right\\padding-top\\|padding\\|page-break-after\\|page-break-before\\|page-break-inside\\|perspective-origin\\|perspective\\|pointer-events\\|position\\|quotes\\|resize\\|right\\|scroll-behavior\\|tab-size\\|table-layout\\|text-align-last\\|text-align\\|text-combine-upright\\|text-decoration-color\\|text-decoration-line\\|text-decoration-style\\|text-decoration\\|text-indent\\|text-justify\\|text-orientation\\|text-overflow\\|text-shadow\\|text-transform\\|text-underline-position\\|top\\|transform-origin\\|transform-style\\|transition-delay\\|transition-duration\\|transition-property\\|transition-timing-function\\|transition\\|transform\\)")
    ("KEYWORD" . "\\<\\(unicode-bidi\\|user-select\\|vertical-align\\visibility\\|white-space\\|windows\\|width\\|word-break\\|word-break\\|word-spacing\\|word-wrap\\|writing-mode\\|z-index\\)"))
  "CSS token type.")

(defun parse-it-css--make-token-type ()
  "Make up the token type."
  (append parse-it-css--token-type
          parse-it-c--bracket-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-css (path)
  "Parse the PATH CSS."
  (let* ((parse-it-lex--token-type (parse-it-css--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))

(provide 'parse-it-css)
;;; parse-it-css.el ends here
