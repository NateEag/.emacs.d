;;; my-functions.el --- Random functions worth having around.

;;; Author: Nate Eagleson and a whole lot of other people.

;;; Version: 0.1

;;; Commentary:

;; Just my personal pile of random functions, pulled from around the net.
;;
;; not much to say here.

;;; Code:

;; Function to refresh a file in my jstoolkit sandbox.
;;
;; Used as a file-local after-save-hook so I don't have to recompile the
;; toolkit every time I update a file.
;;
;; DEBUG I'm not really sure where this should live. It doesn't belong in
;; .dir-locals.el, since it's a not-entirely-trivial function, it doesn't
;; belong in jstoolkit since it's my own tooling, and it doesn't belong here
;; because it's project-specific.

;;;###autoload
(defun hit-servlet ()
  "Update JS for current buffer in jstoolkit sandbox."
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (start-process "refresh-js"
                   nil
                   "curl"
                   (concat "http://localhost:3003/controller/pp?file="
                           filename))
    (message (concat "Processing " filename))))

;; From http://stackoverflow.com/a/9697222

;;;###autoload
(defun comment-or-uncomment-region-or-line ()
    "Comment/uncomment region, or current line if there is no region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; From http://stackoverflow.com/a/17859480/1128957
(defun add-auto-mode (mode &rest patterns)
  "Add filename `patterns' to `auto-mode-alist' for MODE."
  (mapc (lambda (pattern)
          (add-to-list 'auto-mode-alist (cons pattern mode)))
        patterns))

;; GRIPE If I just generalized a tiny bit, I could probably make this work in a
;; lot more languages than just PHP, since comma for separator is a really
;; common idiom, and it'd be easy to accept different delimiters.
;;;###autoload
(defun wrap-args ()
  "Split function arg/array contents to multiple lines in PHP code."
  (interactive)
  (let ((close-paren-pos (search-forward ")" nil 't))
        (open-paren-pos (search-backward "(" nil 't)))

    (goto-char (- close-paren-pos 1))
    (insert "\n")
    (setq close-paren-pos (+ close-paren-pos 1))

    (goto-char (+ open-paren-pos 1))
    (insert "\n")

    (while (search-forward "," close-paren-pos 't)
      (insert "\n")
      (setq close-paren-pos (+ close-paren-pos 1)))
    (indent-region open-paren-pos (+ close-paren-pos 1))))

;; Move the current buffer to a new location on disk, then rename the buffer.
;; From http://www.stringify.com/2006/apr/24/rename/
;;;###autoload
(defun move-current-buffer ()
  "Move the current buffer to a different file and change its name."
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

;; Insert the current date.
;;;###autoload
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use dd-mm-YYYY format. With
     two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%B %d, %Y"))))
      (insert (format-time-string format))))

;; Insert the current time.
;;;###autoload
(defun insert-time (prefix)
    "Insert current time. With prefix-argument, use a full timestamp in 24-hour
     format."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%l:%M %p")
                   ((equal prefix '(4)) "%Y-%m-%d %H:%m:%s"))))
      (insert (concat " " (s-trim (format-time-string format))))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;;###autoload
(defun unfill-paragraph ()
  "Make a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(provide 'my-functions)
;;; my-functions.el ends here
