;;; elm-mode.el --- Major mode for Elm  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015, 2016  Bogdan Popa

;; Author: Joseph Collard
;; Package-Requires: ((f "0.17") (s "1.7.0") (emacs "25.1") (seq "2.23") (reformatter "0.3"))
;; URL: https://github.com/jcollard/elm-mode
;; Package-Version: 20230315.1122
;; Package-Revision: 699841865e1b

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a major mode for editing Elm source code, and working with
;; common core and third-party Elm tools including the compiler, repl,
;; elm-format and more.

;;; Code:
(require 'elm-tags)
(require 'elm-defuns)
(require 'elm-format)
(require 'elm-imenu)
(require 'elm-indent)
(require 'elm-indent-simple)
(require 'elm-interactive)
(require 'elm-font-lock)
(require 'project)
(require 'rx)

(defgroup elm nil
  "Support for the elm programming language."
  :link '(url-link :tag "Github" "https://github.com/jcollard/elm-mode")
  :group 'languages)

(defun elm-mode-after-save-handler ()
  "Perform various operations upon saving a buffer."
  (when elm-sort-imports-on-save
    (elm-sort-imports))
  (when elm-tags-on-save
    (elm-mode-generate-tags))
  (when (or elm-sort-imports-on-save
            elm-tags-on-save)
    (let ((before-save-hook '())
          (after-save-hook '()))
      (basic-save-buffer))))

(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'elm-format-buffer)
    (define-key map (kbd "C-c M-t") 'elm-mode-generate-tags)
    (define-key map (kbd "C-c C-l") 'elm-repl-load)
    (define-key map (kbd "C-c C-p") 'elm-repl-push)
    (define-key map (kbd "C-c C-e") 'elm-repl-push-decl)
    (define-key map (kbd "C-c C-z") 'elm-interactive)
    (define-key map (kbd "C-c C-a") 'elm-compile-add-annotations)
    (define-key map (kbd "C-c C-r") 'elm-compile-clean-imports)
    (define-key map (kbd "C-c C-c") 'elm-compile-buffer)
    (define-key map (kbd "C-c M-c") 'elm-compile-main)
    (define-key map (kbd "C-c M-k") 'elm-package-catalog)
    (define-key map (kbd "C-c C-n") 'elm-preview-buffer)
    (define-key map (kbd "C-c C-m") 'elm-preview-main)
    (define-key map (kbd "C-c C-d") 'elm-documentation-lookup)
    (define-key map (kbd "C-c C-i") 'elm-import)
    (define-key map (kbd "C-c C-s") 'elm-sort-imports)
    (define-key map (kbd "C-c C-v") 'elm-test-project)
    (easy-menu-define elm-mode-map map
      "Elm Mode Menu"
      '("Elm"
        ["Format Buffer" elm-format t]
        "--"
        ["Load Current File in REPL" elm-repl-load t]
        ["Push Region to REPL" elm-repl-push t]
        "--"
        ["Sort Imports" elm-sort-imports t]
        ["View Doc in Browser" elm-documentation-lookup t]
        "--"
        ["Generate TAGS" elm-mode-generate-tags t]))
    map)
  "Keymap for Elm major mode.")

;;;###autoload
(defcustom elm-mode-hook '(elm-indent-mode)
  "Hook called by `elm-mode'."
  :type 'hook
  :group 'elm)

;;;###autoload
(define-derived-mode elm-mode prog-mode "Elm"
  "Major mode for editing Elm source code."
  :group 'elm
  :syntax-table elm--syntax-table

  ;; Indentation
  ;; Elm is not generally suitable for electric indentation, since
  ;; there is no unambiguously correct indent level for any given
  ;; line.
  (when (boundp 'electric-indent-inhibit) (setq electric-indent-inhibit t))

  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local imenu-create-index-function #'elm-imenu-create-index)
  (setq-local comment-start-skip "-- ")
  (setq-local indent-tabs-mode nil)

  (setq-local paragraph-start (concat " *{-\\| *-- |\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter))

  (setq-local beginning-of-defun-function #'elm-beginning-of-defun)
  (setq-local end-of-defun-function #'elm-end-of-defun)

  (when elm-format-on-save
    (elm-format-on-save-mode))
  (add-hook 'after-save-hook #'elm-mode-after-save-handler nil t)
  (elm--font-lock-enable))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(provide 'elm-mode)
;;; elm-mode.el ends here
