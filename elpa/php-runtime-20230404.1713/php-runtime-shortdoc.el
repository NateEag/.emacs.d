;;; php-runtime-shortdoc.el --- Shortdoc for php-runtime  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>

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

;; Shortdoc implementation for php-runtime.

;;; Code:

(when (eval-when-compile (require 'shortdoc nil t))
  (define-short-documentation-group php-runtime
    "Evaluate PHP Code"
    (php-runtime-expr
     :no-eval (php-runtime-expr "1 + 2")
     :result-string "3"
     :no-eval (php-runtime-expr "PHP_VERSION")
     :result-string "8.2.4"
     :no-eval (php-runtime-expr "strtoupper('apple')")
     :result-string "APPLE"
     :no-eval (string-to-number (php-runtime-expr (format "strlen(%s)" (php-runtime-quote-string "abc"))))
     :result 3)
    (php-runtime-eval
     :no-eval (php-runtime-eval "echo strtoupper('apple');")
     :result-string "APPLE"
     :no-eval (php-runtime-eval "while ($line = fgets(STDIN)) { echo strtoupper($line); }"
                                "apple\nbanana\norange")
     :result-string "APPLE\nBANANA\nORANGE")
    "Check PHP extension is loaded"
    (php-runtime-extension-loaded-p
     :no-eval (php-runtime-extension-loaded-p "xdebug")
     :result t
     :no-eval (php-runtime-extension_loaded "notinstalledmod")
     :result nil)))

(provide 'php-runtime-shortdoc)
;;; php-runtime-shortdoc.el ends here
