;;; camel-spell.el --- spell check camelCase words -*- lexical-binding: t -*-

;; Copyright (C) 2016 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 2016-10-30
;; URL: http://github.com/jschaf/camel-spell
;; Version:  0.01
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Installation:
;;
;; Place camel-spell.el on your `load-path' by adding this to your
;; `user-init-file', usually ~/.emacs or ~/.emacs.d/init.el
;;
;; (add-to-list 'load-path "~/dir/to-camel-spell")
;;
;; Load the code:
;;
;;
;; (autoload 'camel-spell-mode "camel-spell" "Spell check camel case words." nil)
;;
;; M-x `camel-spell-mode' to enable spell checking of camel case words.

;;; Commentary:
;;
;; The most recent code is always at http://github.com/jschaf/camel-spell

(require 's)

(defvar-local camel-spell-sub-word-list '()
  "A buffer-local stack to hold parts of a camel cased word to check.
For example, the word \"myCamelCase\" should populate the list like so:

'((\"my\" 0 2) (\"Camel\" 2 7) (\"Case\" 7 11)))

The format of the entries of the stack match the output of
`ispell-get-word'.")

(defun camel-spell-sub-word-list-has-entries-p ()
  "Returns t if the local `camel-spell-sub-word-list' has entries."
  (not (eq camel-spell-sub-word-list nil)))

(defun camel-spell-pop-sub-word-list ()
  "Pops and returns the first element from `camel-spell-sub-word-list'.
If `camel-spell-sub-word-list' is nil, then return nil."
  (pop camel-spell-sub-word-list))

;; Helper function that parses the output of ispell-get-word.  If the word from
;; ispell-get-word is a camelCasedWord, then break up the result into [camel,
;; Cased, Word].  Adjust the start and end locations accordingly.  Return the
;; first entry and save the other two entries in the word queue.

(defun camel-spell--get-sub-word (str position)
  "Returns a subword pair in STR starting at POSITION
A sub-word is a like foo, Foo or HTML.  It's paired with the end
position of the STR exclusive."
  (if (< position (length str))
      (if (s-uppercase? (aref str position))

          (cond
           ;; There's another upper-case character after the first one.
           ((>= (1+ position) (length str))
            (cl-loop for index from position to (1- (length string))
                     collect (aref )
                     )
            )

           )
        ;; if the next char is an upper case, grab all the upper case chars
        ;; else grab as many lower case chars as possible

        )

    '("" (1+ position))
    )


  )

(defun camel-spell-break-string (str)
  "FooBar => '(\"foo\" \"Bar\")"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string
               "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1 \\2" str))

    ;; Handle NamesWithABBREVIATIONSInThem
    (setq str (replace-regexp-in-string
               "\\([A-Z]+\\)\\([A-Z][a-z0-9]\\)" "\\1 \\2" str))

    ;; Handle NamesEndingInABBREVIATIONS.
    (setq str (replace-regexp-in-string
               "\\([a-z0-9]\\)\\([A-Z]\\'\\)" "\\1 \\2" str))

    (split-string str)))

(defun camel-spell-break-camel-case-results (word-info)
  "Breaks WORD-INFO into separate WORD-INFOs on camel casing.
WORD-INFO is a list of '(\"string\" start-point end-point).  If
the word in WORD-INFO is not camel-cased, then return a list with
one word info."
  (when (car-safe word-info)
    (let* ((original-string (nth 0 word-info))
           (start-point (nth 1 word-info))
           (sub-words (camel-spell-break-string original-string))
           result)
      (dolist (word sub-words result)
        (setq result (cons (list word start-point (+ start-point (length word)))
                           result))
        (setq start-point (+ start-point (length word)))))))

(defun camel-spell-get-word-advisor (orig-ispell-get-words &rest args)
  "Advises `ispell-get-word' to correctly spell check camel cased words."
  (if (camel-spell-sub-word-list-has-entries-p)
      (camel-spell-pop-sub-word-list)

    (let* ((word-info (apply orig-ispell-get-words args))
           (camel-infos (camel-spell-break-camel-case-results word-info)))

      (setq camel-spell-sub-word-list
            (append camel-spell-sub-word-list (cdr-safe camel-infos)))
      (car camel-infos))))

(advice-add #'flyspell-get-word :around #'camel-spell-get-word-advisor)


(provide 'camel-spell)
;;; camel-spell.el ends here
