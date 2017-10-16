;;; lsp-javascript-typescript.el --- Javascript/Typescript support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 George Pittarelli <g@gjp.cc>

;; Author: George Pittarelli <g@gjp.cc>
;; Version: 1.0
;; Package-Version: 20171011.904
;; Package-Requires: ((lsp-mode "2.0"))
;; Keywords: javascript typescript lsp
;; URL: https://github.com/emacs-lsp/lsp-javascript

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

;; Javascript and Typescript support for lsp-mode using Sourcegraph's
;; javascript-typescript-langserver server.

;;; Code:

(require 'lsp-mode)

(defconst lsp-javascript--get-root (lsp-make-traverser #'(lambda (dir)
							   (directory-files dir nil "package.json"))))

(defconst lsp-js-ts--major-modes
  '(js-mode js2-mode js3-mode rjsx-mode))

(dolist (mode lsp-js-ts--major-modes)
  (lsp-define-stdio-client mode "JavaScript" 'stdio
			   lsp-javascript--get-root
			   "JavaScript Language Server"
			   '("javascript-typescript-stdio")))

(provide 'lsp-javascript-typescript)
;;; lsp-javascript-typescript.el ends here
