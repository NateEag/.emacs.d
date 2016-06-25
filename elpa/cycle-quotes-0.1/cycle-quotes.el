;;; cycle-quotes.el --- Cycle between quote styles  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords: convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `cycle-quotes' command to cycle between
;; different string quote styles. For instance, in JavaScript, there's
;; three string quote characters: ", ` and '.  In a JavaScript buffer,
;; with point located someplace within the string, `cycle-quotes' will
;; cycle between the following quote styles each time it's called:
;;
;;    --> "Hi, it's me!" --> `Hi, it's me!` --> 'Hi, it\'s me!' --
;;   |                                                            |
;;    ------------------------------------------------------------
;;
;; As seen in the above example, `cycle-quotes' tries to escape and
;; unescape quote characters intelligently.

;;; Code:

(defvar-local cycle-quotes--quote-chars '()
  "A list of string quote characters for the current mode.
Set the first time `cycle-quotes' is called in a buffer.")

(defvar-local cycle-quotes--quote-chars-mode nil
  "The latest mode that the quote char list was last computed for.
If this is different from the current mode, the quote chars need
to be recomputed.")

(defun cycle-quotes--set-quote-chars ()
  "Set the quote chars for the current syntax table."
  (let ((syntax-table (syntax-table)))
    (while syntax-table
      (map-char-table
       (lambda (char-code-or-range syntax)
         (when (equal (syntax-class syntax) 7)
           (if (consp char-code-or-range)
               (let ((from (car char-code-or-range))
                     (to (cdr char-code-or-range)))
                 (dolist (char-code (number-sequence from to))
                   (add-to-list 'cycle-quotes--quote-chars char-code)))
             (add-to-list
              'cycle-quotes--quote-chars char-code-or-range))))
       syntax-table)
      (setq syntax-table (char-table-parent syntax-table)))
    (setq-local cycle-quotes--quote-chars-mode major-mode)))

(defun cycle-quotes--next-quote-char (char)
  "Return quote char after CHAR."
  (let ((list-from-char (member char cycle-quotes--quote-chars)))
    (when list-from-char
      (if (= (length list-from-char) 1)
          (car cycle-quotes--quote-chars)
        (cadr list-from-char)))))

(defun cycle-quotes--fix-escapes (beg end escape-char unescape-char)
  "Fix character escapes between BEG and END.
Instances of ESCAPE-CHAR will be escaped by `\', while instances
where UNESCAPE-CHAR are escaped by `\' will have their escape
character removed."
  (let ((escape-string (string escape-char))
        (unescape-string (string unescape-char)))
    (save-excursion
      (goto-char end)
      (while (search-backward (concat "\\" unescape-string) beg t)
        (replace-match unescape-string nil t)))
    (save-excursion
      (goto-char end)
      (while (search-backward escape-string beg t)
        (replace-match (concat "\\" escape-string) nil t)
        (forward-char -1)))))

;;;###autoload
(defun cycle-quotes ()
  "Cycle between string quote styles."
  (interactive)
  (unless (eq major-mode cycle-quotes--quote-chars-mode)
    (cycle-quotes--set-quote-chars))
  (if (< (length cycle-quotes--quote-chars) 2)
      (message "The current mode has no alternative quote syntax")
    (let ((quote-char (nth 3 (syntax-ppss))))
      (if (not quote-char)
          (message "Not inside a string")
        (let ((inside-generic-string (eq quote-char t))
              ;; Can't use `save-excursion', because the marker will get
              ;; deleted if point is at the beginning of the string.
              (start-pos (point)))
          (when inside-generic-string
            (skip-syntax-backward "^|")
            (forward-char -1)
            (setq quote-char (char-after)))
          (let ((new-quote-char
                 (cycle-quotes--next-quote-char
                  (if inside-generic-string
                      (char-after)
                    quote-char))))
            (unless inside-generic-string
              (search-backward-regexp
               (concat "\\([^\\]" (string quote-char) "\\)\\|"
                       "^" (string quote-char)))
              (when (match-beginning 1)
                (forward-char)))
            (let ((repeat
                   ;; Handle multiple quotes, such as Python's triple
                   ;; quotes.
                   (save-excursion
                     (search-forward-regexp
                      (format "%c+" quote-char))
                     (- (match-end 0) (match-beginning 0)))))
              (save-excursion
                (let ((beg (point)))
                  (forward-sexp)
                  ;; `forward-sexp' fails to jump to the matching quote
                  ;; in some modes, for instance `js2-mode'.
                  (skip-syntax-backward "^\"|")
                  (cycle-quotes--fix-escapes
                   (+ beg 1) (+ (point) 1) new-quote-char quote-char))
                (delete-char (- repeat))
                (dotimes (_ repeat)
                  (insert new-quote-char)))
              (delete-char repeat)
              (dotimes (_ repeat)
                (insert new-quote-char))))
          (goto-char start-pos))))))

(provide 'cycle-quotes)
;;; cycle-quotes.el ends here
