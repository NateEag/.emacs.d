;;; scdoc-mode.el --- Major mode for scdoc files -*- lexical-binding: t; -*-
;; Copyright 2021 Armaan Bhojwani ISC license.

;; Author: Armaan Bhojwani <me@armaanb.net>
;; Maintainer: Armaan Bhojwani <me@armaanb.net>
;; Created: May 30, 2021
;; Version: 0.0.0
;; URL: https://git.armaanb.net/scdoc-mode

;;; Commentary:
;; auto-fill-mode is reccomended.  Make sure to add the following to
;; your init file:
;;
;; (autoload 'scdoc-mode "scdoc-mode" "Major mode for editing scdoc files" t)
;; (add-to-list 'auto-mode-alist '("\\.scd\\'" . scdoc-mode))

;;; Code:

(defface scdoc-level-1-face
  '((t :inherit org-level-1))
	"Face for scdoc level 1 headings."
	:group 'scdoc-faces)
(defvar scdoc-level-1-face 'scdoc-level-1-face)

(defface scdoc-level-2-face
  '((t :inherit org-level-2))
	"Face for scdoc level 2 headings."
	:group 'scdoc-faces)
(defvar scdoc-level-2-face 'org-level-2-face)

(defface scdoc-line-break-face
  '((t :inherit font-lock-warning-face))
	"Face for scdoc manual line break markers."
	:group 'scdoc-faces)
(defvar scdoc-line-break-face 'scdoc-line-break-face)

(defface scdoc-underline-face
  '((t :inherit underline))
	"Face for scdoc underlined text."
	:group 'scdoc-faces)
(defvar scdoc-underline-face 'scdoc-underline)

(defface scdoc-bold-face
  '((t :weight extra-bold))
	"Face for scdoc bolded text."
	:group 'scdoc-faces)
(defvar scdoc-bold-face 'scdoc-bold-face)

(defface scdoc-man-face
  '((t :inherit font-lock-builtin-face
			 :weight extra-bold))
	"Face for scdoc references to manpages."
	:group 'scdoc-faces)
(defvar scdoc-man-face 'scdoc-man-face)

(defface scdoc-literal-face
  '((t :inherit font-lock-comment-delimiter-face))
	"Face for scdoc literal blocks."
	:group 'scdoc-faces)
(defvar scdoc-literal-face 'scdoc-literal-face)

(defface scdoc-comment-face
  '((t :inherit font-lock-comment-face))
	"Face for scdoc comments."
	:group 'scdoc-faces)
(defvar scdoc-comment-face 'scdoc-comment-face)

(defvar scdoc-faces
	'(("^# .*" . scdoc-level-1) ; Heading 1
		("^## .*" . scdoc-level-2) ; Heading 2
		("++$" . scdoc-line-break-face) ; Line break
		("\\(\\s-\\|^\\)_.*_" . scdoc-underline-face) ; Underlines
		("\\*.*\\*" . scdoc-bold-face) ; Bold
		("^```$" . scdoc-literal-face) ; Literal text
		("^; .*" . scdoc-comment-face) ; Comments
		("\\(\\s-\\|^\\)\\sw*\([0-9]\)" . scdoc-man-face) ; References to manpages
		))

(define-derived-mode scdoc-mode text-mode "scdoc"
	"Major mode for editing scdoc files."
	(setq font-lock-defaults '(scdoc-faces)
				fill-column 80))

(provide 'scdoc-mode)
;;; scdoc-mode.el ends here
