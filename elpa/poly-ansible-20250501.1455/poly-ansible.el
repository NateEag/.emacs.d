;;; poly-ansible.el --- Polymode for Ansible: Jinja2 in YAML -*- coding: utf-8; lexical-binding: t; -*-

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
;; Author: Peter Oliver <poly-ansible@mavit.org.uk>
;; Package-Version: 20250501.1455
;; Package-Revision: fc31708bff00
;; Package-Requires: ((ansible "0.4.1") (ansible-doc "0.4") (emacs "24.1") (jinja2-mode "0.2") (polymode "0.2") (systemd "1.4") (yaml-mode "0.0.13"))
;; Keywords: languages
;; URL: https://gitlab.com/mavit/poly-ansible/

;;; Commentary:

;; Edit YAML files for Ansible containing embedded Jinja2 templating.
;;
;; This is a polymode, gluing jinja2-mode into either yaml-mode or
;; yaml-ts-mode.  If you usually use yaml-ts-mode to edit YAML files,
;; then that mode will be used as the host mode; otherwise, yaml-mode
;; will be used.  In either case, minor modes ansible-mode and
;; ansible-doc-mode are both also activated.
;;
;; Also included is poly-systemd-jinja2-mode, a polymode gluing
;; jinja2-mode into systemd-mode, for when you’re using templates to
;; create Systemd unit configurations.
;;
;; Aside: Although yaml-ts-mode is built in to Emacs, as of version 29
;; it is missing basic features compared to yaml-mode (such as
;; indentation).  It also requires the separate installation of the
;; tree-sitter-yaml Tree-sitter parser (either via your operating
;; system’s package manager, via treesit-auto, or manually).
;;
;;; Code:

(require 'ansible)
(require 'ansible-doc)
(require 'poly-ansible-jinja2-filters)
(require 'polymode)
(require 'treesit nil t)


(defun jinja2-ansible-functions-keywords (args)
  "Advice to provide additional keywords for Jinja2 filters defined by Ansible.
ARGS is provided by the advised function, `jinja2-functions-keywords'."
  (append args poly-ansible-jinja2-filters))

(advice-add 'jinja2-functions-keywords :filter-return
            #'jinja2-ansible-functions-keywords)

(require 'poly-jinja2)


(when (featurep 'treesit)
  (unless (boundp 'poly-yaml-ts-hostmode)
    (define-hostmode poly-yaml-ts-hostmode :mode 'yaml-ts-mode)))

;;;###autoload (autoload 'poly-ansible-mode "poly-ansible" "Polymode for Jinja2 templating in Ansible YAML." t)
(define-polymode poly-ansible-mode nil
                 "Polymode for Jinja2 templating in Ansible YAML."
                 :hostmode (if (eq 'yaml-ts-mode
                                   (ignore-errors
                                     (with-temp-buffer
                                       (set-visited-file-name
                                        (concat temporary-file-directory
                                                (make-temp-name "")
                                                ".yaml")
                                        t t)
                                       (set-auto-mode)
                                       major-mode)))
                               'poly-yaml-ts-hostmode
                             'poly-yaml-hostmode)
                 :innermodes '(poly-jinja2-innermode)

                 (ansible-mode 1)
                 (ansible-doc-mode 1))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\(?:group\\|host\\)_vars/" . poly-ansible-mode))


(provide 'poly-ansible)

;;; poly-ansible.el ends here
