;;; lsp-css.el --- CSS/LESS/SASS support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 George Pittarelli <g@gjp.cc>

;; Author: George Pittarelli <g@gjp.cc>
;; Version: 1.0
;; Package-Version: 20181219.504
;; Package-Requires: ((lsp-mode "3.0") (emacs "25.1"))
;; Keywords: languages tools
;; URL: https://github.com/emacs-lsp/lsp-css

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

;; CSS, LESS, and SASS support for lsp-mode using VSCode's
;; vscode-css-langserver server.

;;; Code:

(require 'lsp-mode)

(lsp-define-stdio-client
 lsp-css
 "css"
 nil
 '("css-languageserver" "--stdio"))

(lsp-define-stdio-client
 lsp-scss
 "scss"
 nil
 '("css-languageserver" "--stdio"))

(lsp-define-stdio-client
 lsp-less
 "less"
 nil
 '("css-languageserver" "--stdio"))

(provide 'lsp-css)
;;; lsp-css.el ends here
