;;; elisp-depmap-exec.el --- Construct the DOT executable -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/elisp-depmap.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1") (dash "2.17.0"))
;; Version: 0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; See elisp-depmap.el

;;; Code:
(defcustom elisp-depmap-exec-file "~/graphviz2.dot" ;
  "Location of dot file.  The output image file will use the prefix before the extension."
  :type 'string
  :group 'elisp-depmap)

(defcustom elisp-depmap-exec-outext "png"
  "Output file type."
  :type 'string
  :options '("png" "svg" "tiff" "jpeg" "eps" "json")
  :group 'elisp-depmap)

(defcustom elisp-depmap-exec-commandargs nil
  "Other command line args for dot executable."
  :type 'string
  :group 'elisp-depmap)

(defun elisp-depmap-exec--executeandshow ()
  "Execute the dotfile command and then show the graph."
  (let* ((outfile (format "%s.%s"
                          (car (split-string elisp-depmap-exec-file "\\."))
                          elisp-depmap-exec-outext))
         (command (combine-and-quote-strings
                   (list "dot"
                         (shell-quote-argument (expand-file-name elisp-depmap-exec-file))
                         "-T"
                         (shell-quote-argument elisp-depmap-exec-outext)
                         (or elisp-depmap-exec-commandargs "")
                         "-o"
                         (shell-quote-argument (expand-file-name outfile)))))
         (omesg (shell-command-to-string command)))
    (find-file-noselect outfile)
    `(,command . ,omesg)))

(provide 'elisp-depmap-exec)
;;; elisp-depmap-exec.el ends here
