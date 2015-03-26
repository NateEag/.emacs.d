;;; space-trail.el --- Remove trailing whitespace on save if sensible.

;;; Author: Nate Eagleson

;;; Version: 0.1

;;; Commentary:

;; To keep version control diffs readable, it's nice to strip trailing
;; whitespace on save.
;;
;; However, some file types, like diffs, can have their semantics damaged if
;; all trailing whitespace is stripped.
;;
;; Other file types, like Markdown, should have trailing whitespace stripped in
;; some parts but not others (blank lines in literal blocks should not have it
;; stripped, for instance).
;;
;; space-trail teaches Emacs to handle those correctly.

;;; Code:


(defvar space-trail-stop-whitespace-removal-predicates
  '(space-trail-ignored-mode-p)
  "A list of functions. If any return true, strip passed buffer.")

;; STUB
(defvar space-trail-strip-line-whitespace-predicates
  "A list of functions. If any return true, strip passed line.")

(defvar space-trail-ignored-modes
  '(diff-mode)
  "A list of modes that should not have trailing whitespace stripped.")

(defun space-trail-delete-trailing-whitespace ()
  "Delete trailing whitespace in current buffer if appropriate."

  ;; TODO declare cl-lib dependency for `some`.
  (unless (some
           (lambda (x) x)
           (mapcar 'funcall space-trail-stop-whitespace-removal-predicates))
        ;; TODO Replace this with something that checks whether a given line
        ;; should be stripped.
        (delete-trailing-whitespace)))

(defun space-trail-ignored-mode-p (&optional buffer-or-string)
  "Return true if `BUFFER-OR-STRING's `major-mode' should not be stripped.

`BUFFER-OR-STRING' defaults to (current-buffer)."

  (with-current-buffer (or buffer-or-string (current-buffer))
    (member major-mode space-trail-ignored-modes)))

(defun space-trail-activate ()
  "Remove meaningless trailing whitespace before saving."

  (interactive)
  (add-hook 'before-save-hook 'space-trail-delete-trailing-whitespace))

(defun space-trail-deactivate ()
  "Stop removing meaningless trailing whitespace before saving."

  (interactive)
  (remove-hook 'before-save-hook 'space-trail-delete-trailing-whitespace))

(provide 'space-trail)
;;; space-trail.el ends here
