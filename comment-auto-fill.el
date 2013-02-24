;; Auto-fill in comments is something I have wanted for a very long time.
;; This is copy/pasted from the Emacs wiki, and seems to do the basics. It's
;; not exactly what I'd like, but it's probably good enough that I won't have
;; to screw with it much in future.
(defun comment-auto-fill ()
  "Call to activate auto-filling comments. Not perfect, but beats nothing."
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-no-break-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face)))))

(provide 'comment-auto-fill)
