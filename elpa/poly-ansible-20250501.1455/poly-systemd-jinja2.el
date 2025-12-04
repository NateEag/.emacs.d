;;; poly-systemd-jinja2.el --- Polymode for Jinja2 in Systemd units -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2025, Peter Oliver.
;;
;; This file is part of poly-ansible.
;;
;; poly-ansible is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; poly-ansible is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with poly-ansible.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'poly-jinja2)
(require 'polymode)
(require 'systemd)


(define-hostmode poly-systemd-hostmode :mode 'systemd-mode)

;;;###autoload (autoload 'poly-systemd-jinja2-mode "poly-systemd-jinja2" "Polymode for Jinja2 templating in Systemd units." t)
(define-polymode poly-systemd-jinja2-mode nil
                 "Polymode for Jinja2 templating in Systemd units."
                 :hostmode 'poly-systemd-hostmode
                 :innermodes '(poly-jinja2-innermode))


(defun systemd-file-podman-p-jinja2-advice (args)
  "Hide .jinja2 filename extension in ARGS from systemd-mode."
  (list (replace-regexp-in-string "\\.j\\(?:inja\\)?2\\'" "" (car args))))

(advice-add 'systemd-file-podman-p
            :filter-args #'systemd-file-podman-p-jinja2-advice)


;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (concat (replace-regexp-in-string "\\\\'$" ""
                                                     systemd-autoload-regexp)
                           "\\.j\\(?:inja\\)?2\\'")
                   'poly-systemd-jinja2-mode))


(provide 'poly-systemd-jinja2)

;;; poly-systemd-jinja2.el ends here
