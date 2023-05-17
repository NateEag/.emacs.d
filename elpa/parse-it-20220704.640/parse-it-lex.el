;;; parse-it-lex.el --- Basic lexical analysis  -*- lexical-binding: t; -*-

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
;; Basic lexical analysis.
;;

;;; Code:

(require 'parse-it-util)

(defvar parse-it-lex--token-type
  '(("URL" . "http[s]*://")
    ("NUMBER" . "\\`[0-9]+\\'")
    ("UNKNOWN" . ""))
  "List of token identifier.")

(defconst parse-it-lex--magic-comment-beg "COMMENT_BEG"
  "Magic string represent beginning of the comment.")

(defconst parse-it-lex--magic-comment-end "COMMENT_END"
  "Magic string represent ending of the comment.")

(defconst parse-it-lex--magic-comment "COMMENT"
  "Magic string represent single line comment.")

(defconst parse-it-lex--magic-newline "NEWLN"
  "Magic string represent newline.")

(defvar parse-it-lex--ignore-newline t
  "Ignore newline when tokenizing.")

;;; Core

(defun parse-it-lex--get-inner-regex (regex)
  "Return a inner group REGEX."
  (let ((start-bnd (string-match-p "\\\\(" regex))
        (end-bnd (string-match-p "\\\\)" regex)))
    (if (and start-bnd end-bnd)
        (substring regex (+ start-bnd 2) end-bnd)
      nil)))

(defun parse-it-lex--regex-engine (match-str regex)
  "Lexer's REGEX engine definition to MATCH-STR."
  (let ((inner-regex (parse-it-lex--get-inner-regex regex)))
    (if inner-regex
        (s-replace-regexp inner-regex
                          (lambda (inner-match-str) (concat " " inner-match-str " "))
                          match-str)
      (concat " " match-str " "))))

(defun parse-it-lex--split-to-token-list (src-code)
  "Split SRC-CODE to list of token readable list."
  (let ((ana-src src-code) (token-regex ""))
    (setq ana-src (s-replace-regexp "[\n]" " \n " ana-src))
    (setq ana-src (s-replace-regexp "[\t]" " " ana-src))
    (dolist (token-type parse-it-lex--token-type)
      (setq token-regex (cdr token-type))
      (unless (string-empty-p token-regex)
        (setq ana-src
              (s-replace-regexp
               token-regex
               (lambda (match-str)
                 (parse-it-lex--regex-engine match-str token-regex))
               ana-src))))
    (split-string ana-src " " t nil)))

(defun parse-it-lex--find-token-type (sec mul-comment)
  "Find out section of code's (SEC) token type.
MUL-COMMENT is the flag to check if is multiline commenting."
  (let ((tk-tp "") (token-type "") (token-regex "") (tk-index 0) (tk-break nil)
        (inner-regex nil))
    (while (and (< tk-index (length parse-it-lex--token-type))
                (not tk-break))
      (setq tk-tp (nth tk-index parse-it-lex--token-type))
      (setq token-regex (cdr tk-tp))
      (setq inner-regex (parse-it-lex--get-inner-regex token-regex))
      (when inner-regex (setq token-regex inner-regex))
      (when (string-match-p token-regex sec)
        (setq token-type (car tk-tp))
        ;; NOTE: Check if beginning of comment, in order to escape
        ;; beginning of comment patterns. Hence, if comment (end) pattern
        ;; is the same as comment (beg) pattern then type should be assign
        ;; to comment (end).
        ;;
        ;; This should help recognize programming language using the same
        ;; pattern to (beg) and (end) commenting.
        (when (or (not mul-comment)
                  (not (string= token-type parse-it-lex--magic-comment-beg)))
          (setq tk-break t)))
      (setq tk-index (1+ tk-index)))
    token-type))

(defun parse-it-lex--form-node (val type ln pos)
  "Form a node with TYPE, VAL, LN and POS."
  (list :value val :type type :lineno ln :pos pos))

(defun parse-it-lex-tokenize-it (path)
  "Tokenize the PATH and return list of tokens."
  (let* ((src-code (if path (parse-it-util--get-string-from-file path)
                     (parse-it-util--get-string-from-buffer (current-buffer))))
         (src-sec (parse-it-lex--split-to-token-list src-code))
         (res-lst '())
         (mul-comment nil) (in-comment nil) (newline-there nil)
         (src-ln (split-string src-code "\n"))
         (cur-src-ln (nth 0 src-ln))
         (matched-pos 0) (matched-off 0) (matched-len 0)
         (ln 0) (pos 1) (token-type ""))
    (dolist (sec src-sec)
      (when newline-there (setq pos (1+ pos)))  ; To the beginning of the line.
      (if (string-match-p "[\n]" sec)
          (progn
            (setq sec (nth 0 (split-string sec "\n")))
            ;; NOTE: Do something after seeing newline.
            (progn
              (setq pos (+ pos (length sec) matched-len))
              (setq ln (1+ ln))
              (setq cur-src-ln (nth ln src-ln))
              (setq matched-pos 0)
              (setq matched-off 0)
              (setq matched-len 0)
              (setq newline-there t)))
        (setq newline-there nil)
        (setq pos (- pos matched-pos))
        (setq matched-pos (string-match-p (regexp-quote sec) cur-src-ln matched-off))
        (setq matched-len (length sec))
        (setq matched-off (+ matched-pos matched-len))
        (when matched-pos (setq pos (+ pos matched-pos))))
      (when (or (not (string-empty-p sec))
                newline-there)
        (setq token-type (parse-it-lex--find-token-type sec mul-comment))
        (cond
         ((string= token-type parse-it-lex--magic-comment-beg) (setq mul-comment t))
         ((string= token-type parse-it-lex--magic-comment-end) (setq mul-comment nil))
         ((string= token-type parse-it-lex--magic-comment) (setq in-comment t))
         ((and in-comment newline-there (not mul-comment)) (setq in-comment nil))
         (t
          (when (and (not in-comment) (not mul-comment))
            (unless (string-empty-p sec)
              (setq res-lst
                    (parse-it-util--add-to-list
                     res-lst
                     (parse-it-lex--form-node sec token-type (1+ ln) pos))))
            (when (and newline-there (not parse-it-lex--ignore-newline))
              (setq res-lst
                    (parse-it-util--add-to-list
                     res-lst
                     (parse-it-lex--form-node "\n" parse-it-lex--magic-newline ln pos)))))))))
    res-lst))

(provide 'parse-it-lex)
;;; parse-it-lex.el ends here
