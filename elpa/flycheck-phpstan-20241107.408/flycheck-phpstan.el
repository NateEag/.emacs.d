;;; flycheck-phpstan.el --- Flycheck integration for PHPStan  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 15 Mar 2018
;; Package-Version: 20241107.408
;; Package-Revision: b616f5fa5f8a
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "24.3") (flycheck "26") (phpstan "0.7.2"))
;; License: GPL-3.0-or-later

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

;; Flycheck integration for PHPStan.
;;
;; Put the following into your .emacs file (~/.emacs.d/init.el)
;;
;;     (defun my-php-mode-setup ()
;;       "My PHP-mode hook."
;;       (require 'flycheck-phpstan)
;;       (flycheck-mode t))
;;
;;     (add-hook 'php-mode-hook 'my-php-mode-setup)
;;

;;; Code:
(require 'flycheck)
(require 'phpstan)

;; Usually it is defined dynamically by flycheck
(defvar flycheck-phpstan-executable)
(defvar flycheck-phpstan--temp-buffer-name "*Flycheck PHPStan*")

(defcustom flycheck-phpstan-ignore-metadata-list nil
  "Set of metadata items to ignore in PHPStan messages for Flycheck."
  :type '(set (const identifier)
              (const tip))
  :group 'phpstan)

(defcustom flycheck-phpstan-metadata-separator "\n"
  "Separator of PHPStan message and metadata."
  :type 'string
  :safe #'stringp
  :group 'phpstan)

(defun flycheck-phpstan--enabled-and-set-variable ()
  "Return path to phpstan configure file, and set buffer execute in side effect."
  (let ((enabled (phpstan-enabled)))
    (prog1 enabled
      (when (and enabled
                 phpstan-flycheck-auto-set-executable
                 (null (bound-and-true-p flycheck-phpstan-executable))
                 (or (stringp phpstan-executable)
                     (eq 'docker phpstan-executable)
                     (and (eq 'root (car-safe phpstan-executable))
                          (stringp (cdr-safe phpstan-executable)))
                     (and (stringp (car-safe phpstan-executable))
                          (listp (cdr-safe phpstan-executable)))
                     (null phpstan-executable)))
        (setq-local flycheck-phpstan-executable (car (phpstan-get-executable-and-args)))))))

(defun flycheck-phpstan-parse-output (output &optional _checker _buffer)
  "Parse PHPStan errors from OUTPUT."
  (let* ((json-buffer (with-current-buffer (flycheck-phpstan--temp-buffer)
                        (erase-buffer)
                        (insert output)
                        (current-buffer)))
         (data (phpstan--parse-json json-buffer))
         (errors (phpstan--plist-to-alist (plist-get data :files))))
    (unless phpstan-disable-buffer-errors
      (phpstan-update-ignorebale-errors-from-json-buffer errors))
    (flycheck-phpstan--build-errors errors)))

(defun flycheck-phpstan--temp-buffer ()
  "Return a temporary buffer for decode JSON."
  (get-buffer-create flycheck-phpstan--temp-buffer-name))

(defun flycheck-phpstan--build-errors (errors)
  "Build Flycheck errors from PHPStan ERRORS."
  (cl-loop for (file . entry) in errors
           append (cl-loop for messages in (plist-get entry :messages)
                           for text = (let* ((msg (plist-get messages :message))
                                             (ignorable (plist-get messages :ignorable))
                                             (identifier (unless (memq 'identifier flycheck-phpstan-ignore-metadata-list)
                                                           (plist-get messages :identifier)))
                                             (tip (unless (memq 'tip flycheck-phpstan-ignore-metadata-list)
                                                    (plist-get messages :tip)))
                                             (lines (list (when (and identifier ignorable)
                                                            (concat phpstan-identifier-prefix identifier))
                                                          (when tip
                                                            (concat phpstan-tip-message-prefix tip))))
                                             (lines (cl-remove-if #'null lines)))
                                        (if (null lines)
                                            msg
                                          (concat msg flycheck-phpstan-metadata-separator
                                                  (mapconcat #'identity lines "\n"))))
                           collect (flycheck-error-new-at (plist-get messages :line)
                                                          nil 'error text
                                                          :filename file))))

(flycheck-define-checker phpstan
  "PHP static analyzer based on PHPStan."
  :command ("php" (eval (phpstan-get-command-args :format "json"))
            (eval (if (or (buffer-modified-p) (not buffer-file-name))
                      (phpstan-normalize-path
                       (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace))
                    buffer-file-name)))
  :working-directory (lambda (_) (phpstan-get-working-dir))
  :enabled (lambda () (flycheck-phpstan--enabled-and-set-variable))
  :error-parser flycheck-phpstan-parse-output
  :modes (php-mode php-ts-mode phps-mode))

(add-to-list 'flycheck-checkers 'phpstan t)
(flycheck-add-next-checker 'php 'phpstan)

(provide 'flycheck-phpstan)
;;; flycheck-phpstan.el ends here
