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


(defvar space-trail-ignored-modes
  '(diff-mode)
  "A list of modes that should not have trailing whitespace stripped.")

(defvar space-trail-prevent-buffer-stripping-predicates
  '(space-trail-ignored-mode-p)
  "A list of functions. If any return true, do not strip current buffer.")

(defvar space-trail-strip-whitespace-on-current-line-p
  t
  ;; TODO Make this description coherent. Probably the variable name too...
  "Whether to strip trailing whitespace on the cursor's current line.")

;; TODO Add check for markdown-mode 4-space indented literal blocks.
(defvar space-trail-prevent-line-stripping-predicates
  '(space-trail-point-on-line-p)
  "A list of functions that can prevent stripping a line's whitespace.

Before stripping a line's trailing whitespace, each one is called,
passing the current line number and the cursor's current location.

If any function returns true, the line's trailing whitespace won't
be stripped.")

(defun space-trail-point-on-line-p (line-num orig-point)
  "Return true if LINE-NUM of current buffer contains ORIG-POINT."
  (= line-num (line-number-at-pos orig-point)))

(defun space-trail-delete-trailing-whitespace (&optional start end)
  "Delete trailing whitespace between START and END.
If called interactively, START and END are the start/end of the
region if the mark is active, or of the buffer's accessible
portion if the mark is inactive.

This command deletes whitespace characters after the last
non-whitespace character in each line between START and END, if none
of the functions in `space-trail-should-strip-line-whitespace-predicates'
return true for that line.  It does not consider formfeed characters to be
whitespace.

If this command acts on the entire buffer (i.e. if called
interactively with the mark inactive, or called from Lisp with
END nil), it also deletes all trailing lines at the end of the
buffer if the variable `delete-trailing-lines' is non-nil.

This is largely `delete-trailing-whitespace', modified
to give space-trail.el a hook point."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (copy-marker (or end (point-max))))
            (start (or start (point-min)))
            (orig-point (point)))
        (goto-char start)
        (while (re-search-forward "\\s-$" end-marker t)
          (skip-syntax-backward "-" (line-beginning-position))
          (if (or
               ;; Don't delete formfeeds, even if they are considered whitespace.
               (looking-at-p ".*\f")
               ;; Don't delete lines that have protection via space-trail.
               ;; TODO This whole mess should be abstracted.
               ;; TODO Document or fix dep on cl for `some`.
               (some
                (lambda (x) x)
                (mapcar (lambda (func)
                          (funcall func
                                   (line-number-at-pos)
                                   orig-point))
                        space-trail-prevent-line-stripping-predicates)))
              (progn
                (goto-char (match-end 0)))
            (delete-region (point) (match-end 0))))

        ;; Delete trailing empty lines.
        (goto-char end-marker)
        (when (and (not end)
		   delete-trailing-lines
                   ;; Really the end of buffer.
		   (= (point-max) (1+ (buffer-size)))
                   (<= (skip-chars-backward "\n") -2))
          (delete-region (1+ (point)) end-marker))
        (set-marker end-marker nil))))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

(defun space-trail-maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace in current buffer if appropriate."

  ;; TODO declare cl-lib dependency for `some`.
  (unless (some
           (lambda (x) x)
           ;; TODO change `stop` to `prevent`.
           (mapcar 'funcall space-trail-prevent-buffer-stripping-predicates))
        (space-trail-delete-trailing-whitespace)))

(defun space-trail-ignored-mode-p (&optional buffer-or-string)
  "Return true if `BUFFER-OR-STRING's `major-mode' should not be stripped.

`BUFFER-OR-STRING' defaults to (current-buffer)."

  (with-current-buffer (or buffer-or-string (current-buffer))
    (member major-mode space-trail-ignored-modes)))

(defun space-trail-activate ()
  "Remove meaningless trailing whitespace before saving."

  (interactive)
  (add-hook 'before-save-hook 'space-trail-maybe-delete-trailing-whitespace))

(defun space-trail-deactivate ()
  "Stop removing meaningless trailing whitespace before saving."

  (interactive)
  (remove-hook 'before-save-hook 'space-trail-maybe-delete-trailing-whitespace))

(provide 'space-trail)
;;; space-trail.el ends here
