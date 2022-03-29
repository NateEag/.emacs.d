;;; eslint-disable-rule-flycheck.el --- Flycheck-eslint support for eslint-disable-rule  -*- lexical-binding: t; -*-

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

;; This file provides the function `eslint-disable-rule-flycheck' to add to
;; `eslint-disable-rule-find-rules-hook'.  This function uses eslint errors
;; displayed by flycheck to find out which rule you might want to ignore.

;;; Code:


(require 'map)


;; Declare external variables and functions

(declare-function flycheck-next-error-function "ext:flycheck")
(declare-function flycheck-error-id "ext:flycheck")

(defvar flycheck-enabled-checkers)
(defvar flycheck-mode)


;; Private variables



;; Public functions

;;;###autoload
(defun eslint-disable-rule-flycheck ()
  "Return a list of eslint rule names from flycheck-eslint errors.

Return nil if `flycheck' or `flycheck-eslint' are not active so it is safe,
but useless, to use even when not using flycheck."
  (when (eslint-disable-rule-flycheck--eslint-active-p)
    (when-let* ((error (ignore-errors (or
                                       (get-char-property (point) 'flycheck-error)
                                       (flycheck-next-error-function 1 nil))))
                (rule-name (flycheck-error-id error)))
      (list rule-name))))


;; Private functions

(defun eslint-disable-rule-flycheck--eslint-active-p ()
  "Return non nil if `flycheck-eslint' is enabled in the current buffer."
  (and
   (featurep 'flycheck)
   flycheck-mode
   (member 'javascript-eslint flycheck-enabled-checkers)))

(provide 'eslint-disable-rule-flycheck)
;;; eslint-disable-rule-flycheck.el ends here

;; LocalWords:  backend eslint flycheck
