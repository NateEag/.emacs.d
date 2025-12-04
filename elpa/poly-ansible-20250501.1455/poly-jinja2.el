;;; poly-jinja2.el --- Polymode inner mode for Jinja2 -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2015, 2018, 2020, 2024, Peter Oliver.
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

(require 'jinja2-mode)
(require 'polymode)


(define-obsolete-variable-alias
  'pm-inner/jinja2 'poly-jinja2-innermode "v0.4.2")

(define-innermode poly-jinja2-innermode
                  :mode #'jinja2-mode
                  :head-matcher "{[%{#][+-]?"
                  :tail-matcher "[+-]?[%}#]}"
                  :head-mode 'body
                  :tail-mode 'body
                  :head-adjust-face nil)


(provide 'poly-jinja2)

;;; poly-jinja2.el ends here
