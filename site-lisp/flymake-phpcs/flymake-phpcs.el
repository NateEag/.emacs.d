;;; flymake-phpcs.el --- Flymake handler for PHP to invoke PHP-CodeSniffer
;;
;; Copyright (C) 2011-2012  Free Software Foundation, Inc.
;;
;; Author: Sam Graham <libflymake-phpcs-emacs BLAHBLAH illusori.co.uk>
;; Maintainer: Sam Graham <libflymake-phpcs-emacs BLAHBLAH illusori.co.uk>
;; URL: https://github.com/illusori/emacs-flymake-phpcs
;; Version: 1.0.5
;; Package-Requires: ((flymake "0.3"))
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; flymake-phpcs.el adds support for running PHP_CodeSniffer
;; (http://pear.php.net/package/PHP_CodeSniffer/) to perform static
;; analysis of your PHP file in addition to syntax checking.
;;
;;; Usage:
;; (require 'flymake-phpcs)

(eval-when-compile (require 'flymake))

(defcustom flymake-phpcs-command (executable-find (concat
                                                   (file-name-directory
                                                    (or load-file-name buffer-file-name))
                                                   "bin/flymake_phpcs"))
  "Location of flymake_phpcs wrapper."
  :group 'flymake-phpcs
  :type 'string)

(defcustom flymake-phpcs-standard "PEAR"
  "The coding standard to pass to phpcs via --standard."
  :group 'flymake-phpcs
  :type 'string)

(defcustom flymake-phpcs-show-rule nil
  "Whether to display the name of the phpcs rule generating any errors or warnings."
  :group 'flymake-phpcs
  :type 'boolean)

(defun flymake-phpcs-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      (if (fboundp 'flymake-create-temp-copy)
                        'flymake-create-temp-copy
                        'flymake-create-temp-inplace)))
         (local-file (file-relative-name temp-file
                       (file-name-directory buffer-file-name))))
    (list flymake-phpcs-command
      (append
        (list local-file)
        (if flymake-phpcs-standard
          (list (concat "--standard="
            ;; Looking for "/" is hardly portable
            (if (string-match "/" flymake-phpcs-standard)
              (expand-file-name flymake-phpcs-standard)
              flymake-phpcs-standard))))
        (if flymake-phpcs-show-rule (list "-s"))))))

(eval-after-load "flymake"
  '(progn
    ;; Add a new error pattern to catch PHP-CodeSniffer output
    (add-to-list 'flymake-err-line-patterns
                 '("\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" 1 2 3 4))
    (let ((mode-and-masks (flymake-get-file-name-mode-and-masks "example.php")))
      (setcar mode-and-masks 'flymake-phpcs-init))
    (add-hook 'php+-mode-hook (lambda() (flymake-mode 1)) t)
    (add-hook 'php-mode-hook (lambda() (flymake-mode 1)) t)))

(provide 'flymake-phpcs)
;;; flymake-phpcs.el ends here
