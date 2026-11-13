;;; nix-search.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-shell)
(require 'json)
(require 'subr-x)

;;;###autoload
(defun nix-search--search (search file &optional no-cache use-flakes)
  (nix--process-json-nocheck "search" "--json"
			     (unless use-flakes "--file") file
			     (when no-cache "--no-cache")
			     (if (string-empty-p search)
				 ;; "nix search" requires at least one regex w/ flakes
				 (when use-flakes "^")
			       search)))

(defface nix-search-pname
  '((t :height 1.5
       :weight bold))
  "Face used for package names."
  :group 'nix-mode)

(defface nix-search-version
  '((((class color) (background dark))
     :foreground "light blue")
    (((class color) (background light))
     :foreground "blue"))
  "Face used for package version."
  :group 'nix-mode)

(defface nix-search-description
  '((t))
  "Face used for package description."
  :group 'nix-mode)

(defvar nix-search-mode-menu (make-sparse-keymap "Nix")
  "Menu for Nix Search mode.")

(defvar nix-search-mode-map (make-sparse-keymap)
  "Local keymap used for Nix Search mode.")

(defvar-local nix-search--filter nil
  "Search filter used for current buffer")
(defvar-local nix-search--file nil
  "File/flake used for current buffer")
(defvar-local nix-search--use-flakes nil
  "Whether the current buffer's results came from a flake search")

(defun nix-search--refresh (&rest ignore)
  "Refresh Nix Search buffer"
  (interactive)
  (let ((results (nix-search--search nix-search--filter nix-search--file nil
				     nix-search--use-flakes)))
    (nix-search--display results (current-buffer) nix-search--use-flakes
			 nix-search--filter nix-search--file)))

(defun nix-search-create-keymap ()
  "Create the keymap associated with the Nix Search mode.")

(defun nix-search-create-menu ()
  "Create the Nix Search menu as shown in the menu bar."
  (let ((m '("Nix Search"
             ["Refresh" nix-search--refresh t])))
    (easy-menu-define nix-search-mode-menu nix-search-mode-map "Menu keymap for Nix mode" m)))

(nix-search-create-keymap)
(nix-search-create-menu)

(define-derived-mode nix-search-mode special-mode "Nix Search"
  "Major mode for showing Nix search results.

\\{nix-search-mode-map}"
  :interactive nil
  :group 'nix-mode
  (setq-local revert-buffer-function #'nix-search--refresh)

  (read-only-mode 1))

;;;###autoload
(defun nix-search--display (results &optional buffer use-flakes search file)
  (unless buffer (setq buffer (generate-new-buffer "*nix search*")))
  (with-current-buffer buffer
    (unless (derived-mode-p 'nix-search-mode)
      (nix-search-mode))
    (setq-local nix-search--filter search)
    (setq-local nix-search--file file)
    (setq-local nix-search--use-flakes use-flakes)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "-------------------------------------------------------------------------------\n")
      (dolist (entry results)
	(let ((pname (if use-flakes
			 (alist-get 'pname (cdr entry))
		       (alist-get 'pkgName (cdr entry))))
	      (version (alist-get 'version (cdr entry)))
	      (description (alist-get 'description (cdr entry))))
	  (put-text-property 0 (length pname) 'face 'nix-search-pname pname)
	  (put-text-property 0 (length version) 'face 'nix-search-version version)
	  (put-text-property 0 (length description) 'face 'nix-search-description description)
	  (insert (format "* %s (%s)\n%s\n" pname version description))
	  (insert "-------------------------------------------------------------------------------\n")
	  ))))
  (display-buffer buffer))

;;;###autoload
(defun nix-search (search &optional file buffer)
  "Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in."
  (interactive "snix-search> \n")
  (let* ((use-flakes (nix-has-flakes))
	 (file (or file (if use-flakes (nix-read-flake) (nix-read-file))))
	 (results (nix-search--search search file nil use-flakes)))
    (when (called-interactively-p 'any)
      (nix-search--display results buffer use-flakes search file))
    results))

(defun nix-search-read-attr (file)
  "Read from a list of attributes.
FILE the nix file to look in."
  (let ((collection
	 (sort (mapcar (lambda (x) (symbol-name (car x)))
		       (nix-search "" file))
	       'string<))
	(read (cond ((fboundp 'ivy-read) 'ivy-read)
		    (t 'completing-read))))
    (funcall read "Attribute: " collection)))

(provide 'nix-search)
;;; nix-search.el ends here
