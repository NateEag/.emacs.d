;;; eslint-disable-rule-flymake.el --- Flymake-eslint support for eslint-disable-rule  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Damien Cassou

;; Authors: Damien Cassou <damien@cassou.me>
;; Version: 0.3.0
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

;; This file provides the function `eslint-disable-rule-flymake' to add to
;; `eslint-disable-rule-find-rules-hook'.  This function uses eslint errors
;; displayed by flymake to find out which rule you might want to ignore.

;;; Code:


(require 'map)


;; Declare external variables and functions

(declare-function flymake-diagnostic-backend "ext:flymake-eslint")
(declare-function flymake-diagnostic-data "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-eslint--checker "ext:flymake-eslint")

(defvar flymake-eslint-show-rule-name)
(defvar flymake-mode)


;; Private variables

(defconst eslint-disable-rule-flymake--checker-fn #'flymake-eslint--checker
  "The flymake-eslint backend function name.")


;; Public functions

;;;###autoload
(defun eslint-disable-rule-flymake ()
  "Return a list of eslint rule names from flymake-eslint errors.

Return nil if `flymake' or `flymake-eslint' are not active so it is safe,
but useless, to use even when not using flymake."
  (when (eslint-disable-rule-flymake--eslint-active-p)
    (let* ((diagnostics (flymake-diagnostics (line-beginning-position) (line-end-position))))
      (cl-remove-if-not     ; remove nil values
       #'identity
       (mapcar
        (lambda (diagnostic)
          (when (eslint-disable-rule-flymake--eslint-diagnostic-p diagnostic)
            (eslint-disable-rule-flymake--eslint-rule-name diagnostic)))
        diagnostics)))))


;; Private functions

(defun eslint-disable-rule-flymake--eslint-diagnostic-p (diagnostic)
  "Return non nil if DIAGNOSTIC was created by `flymake-eslint'."
  (eq (flymake-diagnostic-backend diagnostic) eslint-disable-rule-flymake--checker-fn))

(defun eslint-disable-rule-flymake--eslint-rule-name (diagnostic)
  "Return the eslint rule name that triggered DIAGNOSTIC."
  (or (eslint-disable-rule-flymake--eslint-rule-name-from-data diagnostic)
      (eslint-disable-rule-flymake--eslint-rule-name-from-text diagnostic)))

(defun eslint-disable-rule-flymake--eslint-rule-name-from-data (diagnostic)
  "Return the value associated with the :rule-name property in DIAGNOSTIC's data."
  (map-elt (flymake-diagnostic-data diagnostic) :rule-name))

(defun eslint-disable-rule-flymake--eslint-rule-name-from-text (diagnostic)
  "Return the rule name by parsing the error message of DIAGNOSTIC."
  (when flymake-eslint-show-rule-name
    (let ((text (flymake-diagnostic-text diagnostic)))
      (save-match-data
        (string-match "\\[\\(.+\\)\\]$" text)
        (match-string 1 text)))))

(defun eslint-disable-rule-flymake--eslint-active-p ()
  "Return non nil if `flymake-eslint' is enabled in the current buffer."
  (and
   (featurep 'flymake)
   (featurep 'flymake-eslint)
   flymake-mode
   (member eslint-disable-rule-flymake--checker-fn flymake-diagnostic-functions)))

(provide 'eslint-disable-rule-flymake)
;;; eslint-disable-rule-flymake.el ends here

;; LocalWords:  backend eslint flymake
