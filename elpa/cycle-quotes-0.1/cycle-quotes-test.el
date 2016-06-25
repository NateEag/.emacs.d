;;; cycle-quotes-test.el --- Tests for cycle-quotes.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>

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

;;; Code:

(require 'cycle-quotes)
(require 'ert)
;; For testing triple quotes
(require 'python)

(ert-deftest test-cycle-quotes--set-quote-chars ()
  (with-temp-buffer
    (let ((st (make-syntax-table)))
      (set-syntax-table st)
      (modify-syntax-entry ?a "\"")
      (modify-syntax-entry ?b "\"")
      (cycle-quotes--set-quote-chars)
      (should (= (length cycle-quotes--quote-chars) 3))
      (should (memq ?a cycle-quotes--quote-chars))
      (should (memq ?b cycle-quotes--quote-chars))
      (should (memq ?\" cycle-quotes--quote-chars)))))

(ert-deftest test-cycle-quotes--next-quote-char ()
  (let ((cycle-quotes--quote-chars '(?a)))
    (should (= (cycle-quotes--next-quote-char ?a) ?a)))
  (let ((cycle-quotes--quote-chars '(?a ?b)))
    (should (= (cycle-quotes--next-quote-char ?a) ?b)))
  (let ((cycle-quotes--quote-chars '(?a ?b ?c)))
    (should (= (cycle-quotes--next-quote-char ?c) ?a))))

(ert-deftest test-cycle-quotes--fix-escapes ()
  (with-temp-buffer
    (insert "b\\baabc\\b")
    (cycle-quotes--fix-escapes (point-min) (point-max) ?a ?b)
    (should (equal (buffer-string) "bb\\a\\abcb"))))

(ert-deftest test-cycle-quotes ()
  (with-temp-buffer
    (let ((st (make-syntax-table)))
      (set-syntax-table st)
      (modify-syntax-entry ?' "\"")
      (modify-syntax-entry ?` "\"")
      (insert "\"Hi, it's me!\"")
      (goto-char 5)
      (cycle-quotes)
      (should (equal (buffer-string) "`Hi, it's me!`"))
      (cycle-quotes)
      (should (equal (buffer-string) "'Hi, it\\'s me!'"))
      (cycle-quotes)
      (should (equal (buffer-string) "\"Hi, it's me!\"")))))

(ert-deftest test-cycle-quotes-triple-quotes ()
  (with-temp-buffer
    (python-mode)
    (insert "'''Triple quotes, as found in Python.'''")
    (goto-char 5)
    (cycle-quotes)
    (should (equal (buffer-string)
                   "\"\"\"Triple quotes, as found in Python.\"\"\""))
    (cycle-quotes)
    (should (equal (buffer-string)
                   "'''Triple quotes, as found in Python.'''"))))

(provide 'cycle-quotes-test)
;;; cycle-quotes-test.el ends here
