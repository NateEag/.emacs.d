;; Set up load-path to include relevant files in my .emacs.d folder.
;; Add appropriate directories to load-path.
;;
;; This also includes my package.el setup, simply because initializing
;; package.el impacts load-path.

;; Set up the package library per my desires.
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defun add-subdirs-to-front-of-load-path (path)
  "Add directories beneath path to the beginning of load-path."
  (let ((default-directory path))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
                (normal-top-level-add-subdirs-to-load-path))
                 load-path))))

(add-subdirs-to-front-of-load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Set up manually-maintained autoloads. Mostly defines mode hooks.
(require 'nateeag-autoloads-init)
(nateeag-autoloads-init)
