;;; mocha-snippets.el --- Yasnippets for the Mocha JS Testing Framework

;; Copyright (C) 2014 Charles Lowell

;; Author: Charles Lowell <cowboyd@frontside.io>
;; Version: 0.1.0
;; Package-Requires: ((yasnippet "0.8.0"))
;; Maintainer: Charles Lowell <cowboyd@frontside.io>
;; Keywords: test javascript

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides yas snippets for mocha testing in the both
;; js-mode, coffee-mode as well as their descendants.
;;
;; SNIPPETS
;;
;; desc -> describe block
;; bef -> beforeEach block
;; before -> before block
;; aft -> afterEach block
;; after -> after block
;; it -> it block
;;
;; For both setup and teardown, the short forms `bef' and `aft' have
;; been given to `beforeEach' and `afterEach' respectively, since
;; those are more commonly used (and if they aren't, then perhaps they
;; shoud be)
;;
;;; Code:

(setq mocha-snippets-root (file-name-directory (or load-file-name
                                                   (buffer-file-name))))


;;;###autoload
(defun mocha-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" mocha-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(mocha-snippets-initialize))

(provide 'mocha-snippets)
;;; mocha-snippets.el ends here
