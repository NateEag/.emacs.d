;;; parse-it.el --- Basic Parser in Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025  Shen, Jen-Chieh
;; Created date 2019-10-10 11:50:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/parse-it
;; Package-Version: 20250101.1011
;; Package-Revision: 19df0d8d67f0
;; Package-Requires: ((emacs "25.1") (s "1.12.0"))
;; Keywords: convenience parse parser lex lexer ast

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
;; Basic Parser in Emacs Lisp.
;;

;;; Code:

(require 'parse-it-util)
(require 'parse-it-ast)
(require 'parse-it-lex)

(defgroup parse-it nil
  "Basic Parser in Emacs Lisp."
  :prefix "parse-it-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/parse-it"))

;;;###autoload
(defun parse-it (lan &optional path)
  "Parse the PATH with symbol language LAN support.

If optional argument is nil; then it will use current buffer instead."
  (let ((mod-name (intern (format "parse-it-%s" (symbol-name lan)))))
    (if (and (ignore-errors (require mod-name))
             (functionp mod-name))
        (funcall mod-name path)
      (user-error "Language '%s' is not supported" lan))))

(provide 'parse-it)
;;; parse-it.el ends here
