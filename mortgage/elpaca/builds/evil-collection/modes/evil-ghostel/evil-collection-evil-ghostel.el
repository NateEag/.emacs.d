;;; evil-collection-evil-ghostel.el --- Bindings for `evil-ghostel' -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, ghostel, tools

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
;; Bindings for `evil-ghostel'.
;;
;; `evil-ghostel' ships its own evil bindings in `evil-ghostel-mode-map'.
;; This module layers `evil-collection's customizable theme bindings on top.

;;; Code:
(require 'evil-collection)
(require 'evil-ghostel nil t)

(defvar evil-ghostel-mode-map)
(defvar evil-ghostel--escape-mode)

(defconst evil-collection-evil-ghostel-maps '(evil-ghostel-mode-map))

(defcustom evil-collection-evil-ghostel-escape 'evil
  "Where insert-state ESC is routed in `evil-ghostel' buffers.

The default is `evil', matching `evil-collection-vterm' and
`evil-collection-eat', where ESC leaves insert state unless the user
toggles terminal routing.

Valid values are those accepted by `evil-ghostel--escape-mode':
`auto', `terminal', and `evil'."
  :type '(choice (const :tag "Auto (evil-ghostel default)" auto)
                 (const :tag "Default to terminal" terminal)
                 (const :tag "Default to evil/emacs" evil))
  :group 'evil-collection)

(defun evil-collection-evil-ghostel-set-escape ()
  "Apply `evil-collection-evil-ghostel-escape' in this Ghostel buffer."
  (setq evil-ghostel--escape-mode evil-collection-evil-ghostel-escape))

;;;###autoload
(defun evil-collection-evil-ghostel-setup ()
  "Set up `evil' bindings for `evil-ghostel'."
  (add-hook 'evil-ghostel-mode-hook
            #'evil-collection-evil-ghostel-set-escape)
  (evil-collection-bind 'evil-ghostel-mode-map
                              'term-toggle-escape
                              'evil-ghostel-toggle-send-escape))

(provide 'evil-collection-evil-ghostel)
;;; evil-collection-evil-ghostel.el ends here
