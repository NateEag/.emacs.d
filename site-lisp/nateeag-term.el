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
      (if (not shell-window)
          (progn
            (print shell-window)
            (if (not (nateeag/get-term-buffers))
                ;; sane-term dependency
                (sane-term-create))
            ; Need to create a term window. For now I'm trying popwin.
            (popwin:popup-buffer (first (nateeag/get-term-buffers))
                                 :height 24 :stick t :tail t))

        (progn
          ;; popwin dependency
          (popwin:select-popup-window)
          (popwin:display-buffer (nateeag/next-term-buffer (popwin:get-buffer)))
          ;; TODO sane-term-next calls bury-buffer, which makes popwin close
          ;; the buffer (reasonably enough). Instead, let's loop through our
          ;; term buffers and pick the one following our current buffer.
          ;; (sane-term-next)
          )))
    )

(defun nateeag/get-term-window ()
  "Return a window in the current frame displaying a term-mode buffer.

Return nil if no such window can be found."

  (let ((term-buffers (nateeag/get-term-buffers)))
    (if term-buffers
        (get-buffer-window
         (--find '(and (get-buffer-window it) it) (nateeag/get-term-buffers))))))

(defun nateeag/get-term-buffers ()
  "Return a list of all term-mode buffers.

someone must have written this function before, somewhere..."

  ;; dash.el dependency.
  (-filter (lambda (buffer)
                  (with-current-buffer buffer (equal major-mode 'term-mode)))
           (buffer-list)))

(provide 'nateeag-term)
;;; nateeag-term.el ends here
