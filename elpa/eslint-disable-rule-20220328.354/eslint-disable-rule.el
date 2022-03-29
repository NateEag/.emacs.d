;;; eslint-disable-rule.el --- Commands to add JS comments disabling eslint rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Damien Cassou

;; Authors: Damien Cassou <damien@cassou.me>
;; Version: 0.2.0
;; URL: https://github.com/DamienCassou/eslint-disable-rule
;; Package-Requires: ((emacs "27.2"))
;; Created: 15 March 2022

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Provide commands to quickly add eslint-disable, eslint-disable-line and
;; eslint-disable-next-line comments to your JavaScript files.

;;; Code:

;;; Customization

(defgroup eslint-disable-rule nil
  "Commands to add JS comments disabling eslint rules."
  :group 'external)

(defcustom eslint-disable-rule-find-rules-hook '(eslint-disable-rule-flymake eslint-disable-rule-flycheck)
  "List of functions to find all rules the user might want to disable.

Every function is called with no argument and is expected to return a,
possibly empty, list of strings.  Each string is an eslint rule name.  The
first elements of the list should be the rules which have the higher chance
of being the one the user wants to disable.

Move the hooks which have the higher chance of finding the rule the user
wants to disable at the beginning of this variable as those choices will be
more directly accessible.

By default `eslint-disable-rule-all' is not in this list because it
possibly requires configuring `eslint-disable-rule-all-executable'.  Feel
free to add it if you want it."
  :type 'hook
  :options '(eslint-disable-rule-flymake eslint-disable-rule-flycheck eslint-disable-rule-all))

(defcustom eslint-disable-rule-require-description 'prefer-description
  "Whether the user should be asked for a description when disabling a rule.

Adding a description can be made mandatory by adding eslint rule
\"eslint-comments/require-description\" from the eslint-plugin-comments
plugin (see URL `https://www.npmjs.com/package/eslint-plugin-comments').

When the value is 'always, the user must enter a non-empty description to
justify why the rule is disabled.

When the value is 'never, the user is not prompted for a description when
disabling a rule.

When the value is 'prefer-description, the default, the user is prompted
for a description but doesn't have to write any."
  :type '(choice (const :tag "Always" 'always)
                 (const :tag "Never" 'never)
                 (const :tag "Prefer description" 'prefer-description)))


;;; Utility functions

(defun eslint-disable-rule--find-rule-names ()
  "Return a list of strings of eslint rule names that could be disabled.

This evaluates all functions in `eslint-disable-rule-find-rules-hook',
concatenates the results and remove duplicates."
  (let ((rules (mapcan #'funcall eslint-disable-rule-find-rules-hook)))
    (delete-dups rules)))

(defun eslint-disable-rule--find-rule-name (rule-names)
  "Return a string with the name of an eslint rule to disable among RULE-NAMES.

RULE-NAMES is a list of strings of eslint rule names that could be disabled.
This list can be generated with `eslint-disable-rule--find-rule-names'."
  (cl-case (length rule-names)
    (0 (user-error "No rule to disable here"))
    (1 (car rule-names))
    (otherwise (let ((default-rule (car rule-names)))
                 (completing-read (format "Which rule (default: %s): " default-rule)
                                  rule-names nil nil nil nil default-rule)))))

(defvar eslint-disable-rule--description-history (list)
  "Previously entered descriptions.")

(defun eslint-disable-rule--maybe-prompt-for-description ()
  "Ask the user why the rule is disabled.
Return nil if no description is desired.

What exactly happens depends on the value of
`eslint-disable-rule-require-description'."
  (when (memq eslint-disable-rule-require-description '(always prefer-description))
    (let* ((prompt (if (eq eslint-disable-rule-require-description 'always)
                       "Description (required): "
                     "Description: "))
           (description (read-string prompt nil 'eslint-disable-rule--description-history)))
      (if (and (string-empty-p description) (eq eslint-disable-rule-require-description 'always))
          (eslint-disable-rule--maybe-prompt-for-description)
        (if (string-empty-p description) nil description)))))


;;; Commands

;;;###autoload
(defun eslint-disable-rule-disable-next-line (rule-name &optional description)
  "Add eslint-disable-next-line comment above current line to disable RULE-NAME.

If DESCRIPTION is non-nil, insert a description explaining why RULE-NAME
was disabled.

Interactively, ask for RULE-NAME by executing hooks in
`eslint-disable-rule-find-rules-hook'.  Also ask for DESCRIPTION depending
on `eslint-disable-rule-require-description'."
  (interactive (list
                (eslint-disable-rule--find-rule-name (eslint-disable-rule--find-rule-names))
                (eslint-disable-rule--maybe-prompt-for-description)))
  (save-excursion
    (setf (point) (line-beginning-position))
    (open-line 1)
    (widen)
    (comment-indent)
    (insert "eslint-disable-next-line " rule-name)
    (when description
      (insert " -- " description))))


(provide 'eslint-disable-rule)

;;; eslint-disable-rule.el ends here

;; LocalWords:  eslint backend
