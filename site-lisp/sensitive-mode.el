;;; sensitive-mode.el --- Do not create backups of sensitive files.

;;; Author: Anirudh Sasikumar

;;; Version:

;;; Commentary:
;;;
;;; I pulled this directly from here:
;;;
;;; http://anirudhsasikumar.net/blog/2005.01.21.html
;;;
;;; I'd prefer to install it from a package manager, but it didn't seem to be
;;; available.
;;;
;;; At this point it may contain some divergence from the original - I don't
;;; remember clearly, as I haven't done more with it since my first tests. I'm
;;; just committing it to keep my config from being broken.

;;

;;; Code:

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(provide 'sensitive-mode)
;;; sensitive-mode.el ends here
