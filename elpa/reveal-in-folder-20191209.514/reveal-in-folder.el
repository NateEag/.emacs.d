;;; reveal-in-folder.el --- Reveal current file in folder  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-06 23:14:19

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Reveal current file in folder.
;; Keyword: folder finder reveal file explorer
;; Version: 0.0.2
;; Package-Version: 20191209.514
;; Package-Requires: ((emacs "24.3") (f "0.20.0"))
;; URL: https://github.com/jcs090218/reveal-in-folder

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
;; Reveal current file in folder.
;;

;;; Code:

(require 'f)
(require 'ffap)


(defun reveal-in-folder--safe-execute-p (in-cmd)
  "Correct way to check if IN-CMD execute with or without errors."
  (let ((inhibit-message t) (message-log-max nil))
    (= 0 (shell-command in-cmd))))

(defun reveal-in-folder--signal-shell (path)
  "Send the shell command by PATH."
  (let ((default-directory
          (if path (f-dirname (expand-file-name path)) default-directory)))
    (cond
     ;; Windows
     ((memq system-type '(cygwin windows-nt ms-dos))
      (reveal-in-folder--safe-execute-p "explorer ."))
     ;; macOS
     ((eq system-type 'darwin)
      (reveal-in-folder--safe-execute-p "open ."))
     ;; Linux
     ((eq system-type 'gnu/linux)
      (reveal-in-folder--safe-execute-p "xdg-open ."))
     ;; BSD
     ((or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      ;; TODO: Not sure what else command do I need to make it work in BSD.
      (reveal-in-folder--safe-execute-p "open ."))
     (t (error "[ERROR] Unknown Operating System type")))))

;;;###autoload
(defun reveal-in-folder-at-point ()
  "Reveal the current file in folder at point."
  (interactive)
  (reveal-in-folder--signal-shell (ffap-file-at-point)))

;;;###autoload
(defun reveal-in-folder-this-buffer ()
  "Reveal the current buffer in folder."
  (interactive)
  (reveal-in-folder--signal-shell (buffer-file-name)))

;;;###autoload
(defun reveal-in-folder ()
  "Reveal buffer/path depends on cursor condition."
  (interactive)
  (if (ffap-file-at-point) (reveal-in-folder-at-point) (reveal-in-folder-this-buffer)))


(provide 'reveal-in-folder)
;;; reveal-in-folder.el ends here
