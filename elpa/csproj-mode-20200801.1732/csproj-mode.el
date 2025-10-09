;;; csproj-mode.el --- Work with .NET project files (csproj, vbproj) -*- lexical-binding: t -*-

;;; Copyright (C) 2018 Omair Majid

;; Author: Omair Majid <omair.majid@gmail.com>
;; URL: https://github.com/omajid/csproj-mode
;; Keywords: languages, tools
;; Version: 0.1.20190318
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a major mode to edit csproj (csproj, vbproj) and other
;; msbuild xml files (*.*proj files).

;;; Code:

(defgroup csproj nil ""
  :group 'languages)

(defun csproj-mode--get-dotnet-new-templates ()
  "Find the template names supported by 'dotnet new'."
  (mapcar (lambda (line) (nth 1 (split-string line "   *" t)))
          (nthcdr 2 (split-string
                     (nth 2 (split-string
                             (shell-command-to-string "dotnet new --help")
                             "\n\n" t "[ \n\t]"))
                     "\n"))))

;;;###autoload
(defun csproj-mode-dotnet-new (template-name)
  "Invoke 'dotnet new' with the given TEMPLATE-NAME."
  (interactive (list (completing-read "Name: " (csproj-mode--get-dotnet-new-templates))))
  (message "%s" template-name)
  (start-process "dotnet" "*dotnet-new*" "dotnet" "new" template-name))

;;;###autoload
(define-derived-mode csproj-mode xml-mode "csproj"
  "A major mode for editing csproj and other msbuild-style project files"
  :group 'csproj)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[^.]*proj\\'" . csproj-mode))

(eval-after-load 'yasnippet
  '(let ((snippet-dir (expand-file-name
                       "snippets"
                       (file-name-directory (or load-file-name (buffer-file-name))))))
     (add-to-list 'yas-snippet-dirs snippet-dir t)
     (yas-load-directory snippet-dir)))

(provide 'csproj-mode)
;;; csproj-mode.el ends here
