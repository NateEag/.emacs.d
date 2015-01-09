;;; nateeag-term.el --- Yet another ansi-term multiplexer.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Neither multi-term nor sane-term quite do what I want.
;;
;; Thus, I've started working on this, which I may never finish.
;;
;; It's starting as an adaptation on top of sane-term. We'll see if that sticks.

;;; Code:

(defun nateeag/show-term-or-next-term ()
  "Show a term-mode buffer in popwin window.

If one is already displayed, move to the next term-mode buffer."
    (interactive)

    (let ((shell-window (nateeag/get-term-window)))
      (if (equalp shell-window nil)
          (sane-term-create)
        (sane-term-next)))
    )

(defun nateeag/get-term-window ()
  "Return a window in the current frame displaying a term-mode buffer.

Return nil if no such window can be found."

  (--find '(get-buffer-window it) (nateeag/get-term-buffers)))

(defun nateeag/get-term-buffers ()
  "Return a list of all term-mode buffers.

someone must have written this function before, somewhere..."

  ;; dash.el dependency.
  (-filter (lambda (buffer)
                  (with-current-buffer buffer (equal major-mode 'term-mode)))
           (buffer-list)))

(provide 'nateeag-term)
;;; nateeag-term.el ends here
