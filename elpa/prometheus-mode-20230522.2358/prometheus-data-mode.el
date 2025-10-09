;;; prometheus-data-mode.el --- Prometheus Data Mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Peter Hoeg
;;
;; Author: Peter Hoeg <peter@hoeg.com>
;; Maintainer: Peter Hoeg <peter@hoeg.com>
;; Created: May 09, 2023
;; Modified: May 09, 2023
;; Keywords: languages
;; Homepage: https://gitlab.com/peterhoeg/prometheus-mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode for viewing data/metrics from prometheus exporters.
;;
;;  Currently, the following features are supported:
;;  1. Syntax highlighting
;;  2. imenu
;;
;;; Code:

(require 'imenu)

(defun prometheus-data-mode--build-imenu ()
  "Build `prometheus-data-mode' imenu."
  (save-excursion
    (goto-char (point-min))
    (setq imenu-generic-expression `((nil
                                      ,(if (re-search-forward (rx bol "#" (optional space) "HELP") nil 'noerror)
                                           (rx bol "#" (optional space) "HELP" space (group (one-or-more (any alnum blank punct))) eol)
                                         (rx bol "#" (optional space) "TYPE" space (group (one-or-more (any alnum blank punct))) eol))
                                      1))))
  (imenu--generic-function imenu-generic-expression))

;;;###autoload
(define-derived-mode prometheus-data-mode fundamental-mode "PrometheusData"
  "Major mode for viewing files containing Prometheus metrics data."

  (setq-local comment-start "#"
              comment-end ""
              font-lock-defaults `(((,(rx
                                       bol
                                       "#" (optional space) "HELP" space
                                       (one-or-more (any alphanumeric "_")) space
                                       (group (one-or-more (any alphanumeric blank punctuation)))
                                       eol) . (1 'font-lock-comment-face))
                                    (,(rx bol "#") . 'font-lock-comment-face)
                                    (,(rx
                                       bol
                                       "#" (optional space) "TYPE" space
                                       (one-or-more (any alphanumeric "_")) space
                                       (group (or "counter" "gauge" "summary" "untyped"))
                                       (optional space)
                                       eol) .(1 'font-lock-type-face))
                                    (,(rx
                                       bol
                                       (optional
                                        "#" (optional space) (or "HELP" "TYPE") space)
                                       (group (one-or-more (any alphanumeric "_")))
                                       (or space "{")) . (1 'font-lock-keyword-face))
                                    (,(rx
                                       (or "{" ",")
                                       (group (one-or-more (any alphanumeric "_")))
                                       "=") . 'font-lock-variable-name-face)
                                    (,(rx (group (one-or-more (any digit ".")))) . 'font-lock-constant-face)
                                    (,(rx (or "HELP" "TYPE")) . 'font-lock-builtin-face)))
              imenu-sort-function #'imenu--sort-by-name
              imenu-create-index-function #'prometheus-data-mode--build-imenu
              imenu-max-item-length nil)

  (add-hook 'prometheus-data-mode-hook
            (lambda ()
              (when (bound-and-true-p prometheus-mode-line-numbers)
                (display-line-numbers-mode))
              (setq-local view-read-only 't)
              (read-only-mode))))

(provide 'prometheus-data-mode)

;;; prometheus-data-mode.el ends here
