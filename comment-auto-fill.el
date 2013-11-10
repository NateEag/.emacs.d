;; Auto-fill in comments is something I have wanted for a very long time.
;; It looks like it may actually be simpler than I thought.
(defun comment-auto-fill ()
  "Call to activate auto-filling comments. Not perfect, but beats nothing."
  (setq comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(provide 'comment-auto-fill)
