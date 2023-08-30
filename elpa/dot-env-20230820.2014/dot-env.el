;;; dot-env.el --- Dotenv functionality -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (s "1.13.0"))
;; Homepage: https://github.com/amodelbello/dot-env.el
;; Keywords: convenience, dotenv, environment, configuration


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; dot-env is a package that reproduces the functionality in
;; https://github.com/motdotla/dotenv/tree/master and allows the use of
;; environment variables stored in a .env file, similar to a technique used
;; in the node.js ecosystem.
;;
;; VARIABLES:
;; dot-env-filepath            : Path to .env file (defaults to .emacs.d/.env)
;; dot-env-environment         : alist that stores environment variables
;; dot-env-file-is-encrypted   : non-nil prevents values being stored in Emacs environment

;; COMMANDS:
;; dot-env-config    : Loads the .env file into dot-env-environment
;; dot-env-parse     : Parses a dotenv string into an association list and return the result
;; dot-env-populate  : Loads the values from an association list into dot-env-environment
;;
;; FUNCTIONS:
;; dot-env-get   : Takes a string parameter and returns corresponding value


;;; Code:

(require 's)

(defvar dot-env-filepath (format "%s%s" user-emacs-directory ".env")
  "Path to the .env file.")

(defvar dot-env-environment '()
  "An alist that stores .env variables.")

(defvar dot-env-file-is-encrypted nil
  "If non-nil, don't store dotenv values in `dot-env-environment'.
Rather, parse the source file for each variable read. This is useful
if you are reading from an encrypted file and don't want to store values
as plain text in the Emacs environment.")

(defun dot-env--get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dot-env--get-lines (str)
  "Get all of the valid lines from STR.
Returns a list of matches in `((full line, key, value) ...)` form.
Returns nil if no matches."
  (s-match-strings-all
   (rx (: (| line-start line-start)
          (* space)
          (? (: "export" (+ space)))
          (group (+ (in alnum "_" "-")))
          (| (: (* space) "=" (*? space))
             (: ":" (+? space)))
          (? (group
              (| (: (* space) "'" (* (| (: (syntax escape) "'") (not "'"))) "'")
                 (: (* space) "`" (* (| (: (syntax escape) "`") (not "`"))) "`")
                 (: (* space)
                    (syntax string-quote)
                    (* (| (: (syntax escape) (syntax string-quote))
                          (not (syntax string-quote))))
                    (syntax string-quote))
                 (+ (not (in "#" "\n")))
                 (* space)
                 (? (: "#" (* nonl)))
                 (| line-end line-end))))))
   str))

(defun dot-env--cleanse-value (raw-value)
  "Remove containing quotes, extra backslashes, and trim whitespace in RAW-VALUE."
  (replace-regexp-in-string
   "^\\(['\"`]\\)\\([[:ascii:]\\|[:nonascii:]]*\\)\\1"
   "\\2"
   (replace-regexp-in-string "\\\\" "" (string-trim (or raw-value "")))))


(defun dot-env-parse (dotenv-str)
  "Parse DOTENV-STR into an association list and return the result."
  (interactive)
  (let ((lines (dot-env--get-lines dotenv-str))
        (output))
    (dolist (item lines output)
      (let ((key (intern (nth 1 item)))
            (value (dot-env--cleanse-value (nth 2 item))))
        (setq output (if (assoc key output)
                         (cons (list key value)
                               (assq-delete-all key output))
                       (cons (append (list key value))
                             output)))))
    (setq output (nreverse output))))

(defun dot-env-config (&optional path)
  "Load the values from file located at PATH and return them or an error.
PATH defaults to `user-emacs-directory'/.env."
  (interactive)
  (condition-case err
      (let* ((path (or path dot-env-filepath))
             (environment (dot-env-parse (dot-env--get-file-contents path))))
        (if dot-env-file-is-encrypted
            (setq dot-env-environment "encrypted data, not stored in environment")
          (setq dot-env-environment environment))
        environment)
    (error (message "Failed to configure dotenv environment: %s" err))))

(defun dot-env-populate (alist &optional override debug)
  "Load the values from the association list ALIST into dot-env-environment.
If OVERRIDE is non-nil, override existing values.
If DEBUG is non-nil, print log messages.
ALIST should be in the form of ((symbol string))
Populates dot-env-environment and returns it."
  (interactive)
  (setq dot-env-environment
        (let ((override (or override nil))
              (debug (or debug nil))
              (environment dot-env-environment))
          (dolist (item alist environment)
            (let ((key (nth 0 item))
                  (value (dot-env--cleanse-value (nth 1 item))))
              (setq environment
                    (if (assoc key environment)
                        (if override
                            (progn
                              (if debug
                                  (message "%s is already defined and WAS overwritten" key))
                              (cons (list key value)
                                    (assq-delete-all key environment)))
                          (if debug
                              (message "%s is already defined and was NOT overwritten" key))
                          environment)
                      (cons (append (list key value))
                            environment))))
            environment))))

(defun dot-env-get (field &optional default)
  "Get the value of FIELD from dot-env-environment.
Use DEFAULT if no value is found."
  (interactive)
  (let ((environment (if dot-env-file-is-encrypted
                         (dot-env-config)
                       dot-env-environment)))
    (or
     (car (cdr (assoc field environment)))
     default)))

(provide 'dot-env)

;;; dot-env.el ends here
