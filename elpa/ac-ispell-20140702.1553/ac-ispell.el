;;; ac-ispell.el --- ispell completion source for auto-complete

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-ac-ispell
;; Version: 20140702.1553
;; X-Original-Version: 0.06
;; Package-Requires: ((auto-complete "1.4"))

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

;; `ac-ispell.el' provides ispell/aspell completion source for auto-complete.
;; You can use English word completion with it.

;; To use this package, add following code to your init.el or .emacs
;;
;;    ;; Completion words longer than 4 characters
;;    (custom-set-variables
;;      '(ac-ispell-requires 4)
;;      '(ac-ispell-fuzzy-limit 4))
;;
;;    (eval-after-load "auto-complete"
;;      '(progn
;;          (ac-ispell-setup)))
;;
;;    (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
;;    (add-hook 'mail-mode-hook 'ac-ispell-ac-setupa)

;;; Code:

(require 'auto-complete)
(require 'ispell)

(defgroup ac-ispell nil
  "Auto completion with ispell."
  :group 'auto-complete)

(defcustom ac-ispell-requires 3
  "Minimum input for starting completion."
  :type 'integer
  :group 'ac-ispell)

(defcustom ac-ispell-fuzzy-limit 2
  "Limit number of candidates for fuzzy source."
  :type 'integer
  :group 'ac-ispell)

(defface ac-ispell-fuzzy-candidate-face
  '((t (:inherit ac-candidate-face :foreground "red")))
  "Face for fuzzy candidate."
  :group 'ac-ispell)

(defvar ac-ispell--cache nil)

(defun ac-ispell--case-function (input)
  (let ((case-fold-search nil))
    (cond ((string-match-p "\\`[A-Z]\\{2\\}" input) 'upcase)
          ((string-match-p "\\`[A-Z]\\{1\\}" input) 'capitalize)
          (t 'identity))))

(defun ac-ispell--lookup-candidates (lookup-func input)
  (let ((candidates (funcall lookup-func (concat input "*")
                             ispell-complete-word-dict)))
    (setq ac-ispell--cache (cons input candidates))
    candidates))

(defun ac-ispell--lookup-cache (input)
  (let* ((cached-input (car ac-ispell--cache))
         (regexp (concat "\\`" cached-input)))
    (when (string-match-p regexp input)
      (cdr ac-ispell--cache))))

(defun ac-ispell--candidates ()
  (let ((input (downcase ac-prefix))
        (case-func (ac-ispell--case-function ac-prefix))
        (lookup-func (if (fboundp 'ispell-lookup-words)
                         'ispell-lookup-words
                       'lookup-words)))
    (when (string-match-p "\\`[a-z]+\\'" input)
      (let ((candidates (or (ac-ispell--lookup-cache input)
                            (ac-ispell--lookup-candidates lookup-func input))))
        (mapcar case-func candidates)))))

(defun ac-ispell--correct-word (word)
  (when (and ispell-async-processp (> (length word) 0))
    (ispell-set-spellchecker-params)
    (ispell-accept-buffer-local-defs)
    (ispell-send-string "%\n")
    (ispell-send-string (concat "^" word "\n"))
    (while (progn
             (with-local-quit
               (accept-process-output ispell-process nil nil 1))
             (not (string= "" (car ispell-filter)))))
    (let ((poss (ispell-parse-output (cadr ispell-filter))))
      (when (listp poss)
        (nth 2 poss)))))

(defun ac-ispell--fuzzy-candidates ()
  (ac-ispell--correct-word ac-prefix))

;;;###autoload
(defun ac-ispell-ac-setup ()
  "Add `ac-source-ispell' to `ac-sources' and enable `auto-complete' mode"
  (interactive)
  (when (> ac-ispell-fuzzy-limit 0)
    (add-to-list 'ac-sources 'ac-source-ispell-fuzzy))
  (add-to-list 'ac-sources 'ac-source-ispell)
  (unless auto-complete-mode
    (auto-complete-mode +1)))

;;;###autoload
(defun ac-ispell-setup ()
  "Declare auto-complete source based on `ac-ispell-requires'"
  (interactive)

  (ac-define-source ispell
    `((candidates . ac-ispell--candidates)
      (requires . ,ac-ispell-requires)
      (symbol . "s")))
  (ac-define-source ispell-fuzzy
    `((candidates . ac-ispell--fuzzy-candidates)
      (match . (lambda (prefix candidates) candidates))
      (requires . ,ac-ispell-requires)
      (limit . ,ac-ispell-fuzzy-limit)
      (symbol . "s")
      (candidate-face . ac-ispell-fuzzy-candidate-face))))

(provide 'ac-ispell)

;;; ac-ispell.el ends here
