;;; eslint-disable-rule-all.el --- Find all eslint rules that may be disabled  -*- lexical-binding: t; -*-

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

;; This file provides the function `eslint-disable-rule-all' to add to
;; `eslint-disable-rule-find-rules-hook'.  This function asks eslint to list all
;; rules of the project and return the result.  You may want to configure
;; `eslint-disable-rule-all-executable'.

;;; Code:


(require 'map)
(require 'json)

(defcustom eslint-disable-rule-all-executable "eslint"
  "The eslint executable to use."
  :group 'eslint-disable-rule
  :type 'string)


;; Declare external variables and functions


;; Private variables


;; Public functions

;;;###autoload
(defun eslint-disable-rule-all ()
  "Return a list of all eslint rules."
  (when (buffer-file-name)
    (when-let* ((executable (executable-find eslint-disable-rule-all-executable))
                (result-string (shell-command-to-string
                                (format "%s --print-config %s"
                                        executable
                                        (buffer-file-name))))
                (result (json-parse-string result-string))
                (rule-configuration (map-elt result "rules")))
      (map-keys rule-configuration))))


;; Private functions



(provide 'eslint-disable-rule-all)
;;; eslint-disable-rule-all.el ends here

;; LocalWords:  eslint
