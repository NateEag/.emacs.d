;;; evil-collection-eca.el --- Bindings for eca -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, eca, tools

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
;;; Bindings for eca.

;;; Code:
(require 'evil-collection)
(require 'eca nil t)

(defconst evil-collection-eca-maps '(eca-chat-mode-map))

(defvar eca-chat-mode-map)

;;;###autoload
(defun evil-collection-eca-setup ()
  "Set up `evil' bindings for `eca'."
  (add-hook 'eca-chat-mode-hook #'evil-normalize-keymaps)

  (evil-collection-bind 'eca-chat-mode-map
                        'repl-submit 'eca-chat--key-pressed-return
                        'repl-newline 'eca-chat--key-pressed-newline
                        'repl-force-newline 'eca-chat--key-pressed-newline
                        'refresh 'eca-chat-reset
                        'refresh-all 'eca-restart
                        'find-file 'eca-chat-select
                        'describe-mode 'eca-transient-menu
                        'next-item 'eca-chat-go-to-next-expandable-block
                        'prev-item 'eca-chat-go-to-prev-expandable-block
                        'next-section 'eca-chat-go-to-next-user-message
                        'prev-section 'eca-chat-go-to-prev-user-message
                        'section-toggle 'eca-chat-toggle-expandable-block
                        'zoom-in 'eca-chat-image-zoom-in
                        'zoom-out 'eca-chat-image-zoom-out
                        'zoom-reset 'eca-chat-image-zoom-reset)

  (when evil-collection-want-g-bindings
    (evil-collection-define-key 'normal 'eca-chat-mode-map
      "gs" 'eca-chat-cycle-agent
      "gm" 'eca-chat-select-agent
      "gv" 'eca-chat-select-model
      "go" 'eca-chat-select
      "gp" 'eca-chat-copy-at-point
      "gt" 'eca-chat-timeline
      "gF" 'eca-chat-new
      "gq" 'eca-chat--key-pressed-queue
      "ga" 'eca-chat-tool-call-accept-all
      "gA" 'eca-chat-tool-call-accept-next
      "gy" 'eca-chat-tool-call-accept-all-and-remember
      "gn" 'eca-chat-tool-call-reject-next
      "g," 'eca-settings
      "gM" 'eca-mcp-toggle-server
      "gV" 'eca-chat-select-variant
      "gT" 'eca-chat-talk
      "gO" 'eca-chat-load-older-history
      "gd" 'eca-chat-clear-prompt
      "gk" 'eca-chat-clear
      "gK" 'eca-chat-delete
      "gN" 'eca-chat-rename
      "gz" 'eca-chat-save-image-at-point)))

(provide 'evil-collection-eca)
;;; evil-collection-eca.el ends here
