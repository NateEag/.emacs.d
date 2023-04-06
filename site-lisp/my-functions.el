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

(require 'dash)

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

;;;###autoload
(cl-defun wrap-args (&optional (start-delim "(") (end-delim ")"))
  "Split comma-separated things inside a pair of delimiters to individual lines.

By default it handles C-like function parameters, hence the name."

  (interactive)
  (let ((close-delim-pos (search-forward end-delim nil 't))
        (start-delim-pos (search-backward start-delim nil 't)))

    (goto-char (- close-delim-pos 1))
    (insert "\n")
    (setq close-delim-pos (+ close-delim-pos 1))

    (goto-char (+ start-delim-pos 1))
    (insert "\n")

    (while (search-forward "," close-delim-pos 't)
      (insert "\n")
      (setq close-delim-pos (+ close-delim-pos 1)))
    (indent-region start-delim-pos (+ close-delim-pos 1))))

(defun wrap-arr ()
  "Split C-like arrays to one item per line."

  (interactive)

  (wrap-args "[" "]"))

;; Slightly tweaked from http://stackoverflow.com/a/25212377, to support moving
;; to a new directory.
;;
;; FIXME Handle moving a buffer to being a descendant of its current path.
;; Right now, it fails on the reasonable grounds that what is currently the
;; buffer's path is not a directory.
(defun move-current-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((new-name (read-file-name "New name: " filename))
             (dir-name (file-name-directory new-name)))

        (if (not (file-exists-p dir-name))
            (make-directory dir-name t))

        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name))
          (normal-mode))))))

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

;; Insert the current date.
;;;###autoload
(defun insert-date-path-format (prefix)
    "Insert the current date. With prefix-argument, use dd-mm-YYYY format. With
     two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y/%m/%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%B %d, %Y"))))
      (insert (format-time-string format))))

(defun insert-date-iso-format (prefix)
  "Insert current date and time in ISO 8601 format.
With PREFIX argument, insert just the date."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d %H:%M:%S %Z")
                 ((equal prefix) "%Y-%m-%d"))))
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

(defun ne/get-buffer-project-relative-path ()
  "Get the project-relative path to the current buffer."

  (s-replace (projectile-project-root) "" (buffer-file-name)))

(defun ne/yank-buffer-project-relative-path ()
  "Copy the project-relative path to current buffer to clipboard.

Handy when writing issue comments in shared task trackers and referencing code."
  (interactive)
  (kill-new (ne/get-buffer-project-relative-path)))

(defun ne/get-dir-locals-file ()
  "Return the path to the .dir-locals.el file for the current buffer.

This turns out to be more work than you'd hope - hence this function."

  (locate-dominating-file buffer-file-name ".dir-locals.el"))

(defun ne/get-dir-locals-parent-dir ()
  "Return the path to the folder containing this file's .dir-locals.el.

Just saves a half-line or so in .dir-locals.el files, when you're trying to
find the path to the project directory."

  (file-name-directory (ne/get-dir-locals-file)))

(defun ne/extract-to-file (name start end)
  "Delete text from `START' to `END' and put it in the new file `NAME'.

Interactively, queries for the new filename and uses the selected region for
its range.

Primarily useful when refactoring code. Adapted from the Emacs Lisp-specific
version at https://stackoverflow.com/a/8603158/1128957."
  (interactive (list (read-string "File name to create: ")
                     (region-beginning) (region-end)))
  (let ((snip (buffer-substring start end)))
    (with-current-buffer (find-file-noselect name)
      (insert snip)
      (save-buffer))
    (delete-region start end)))

(defun ne/list-loaded-elisp-files ()
  "Return a string listing all Emacs Lisp files that have been loaded.

Yanked from https://stackoverflow.com/a/31449758/1128957."

  (interactive)

  (mapconcat 'car (reverse load-history) "\n"))

(defun ne/switch-window-then-find-project-file ()
"Open a file from the current project in the next window.

I usually have just two windows in a frame, so this just Does
What I Mean. If you use more than that it may not be so useful to
you."

  (interactive)
  ;; FIXME Make this work. It does not at present.
  ;;
  ;; FIXME Stop misordering buffer stack for target window when this is called.
  (let ((trigger-buffer) (current-buffer))
    (other-window 1)
    (switch-to-buffer trigger-buffer t)
    (counsel-projectile-find-file)))

(defun ne/yank-buffer-name ()
  "Put the current buffer's name in the kill ring.

Also gets it in the OS clipboard, which is usually what I actually want."

  (interactive)
  (kill-new (buffer-name)))

(defun shell-command-on-buffer ()
  "Update buffer contents with results of running shell command on it.

Tweaked from here: http://stackoverflow.com/a/19160992/1128957"

  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           (read-shell-command "Shell command on buffer: ")
                           t
                           t))

(defun ne/set-theme-to-match-system-theme ()
  "Set theme to solarized-light or solarized-dark based on OS light/dark setting.

Yanked from
https://www.reddit.com/r/emacs/comments/hejsqm/is_there_a_way_to_detect_lightdark_mode_on_mac/"

  (interactive)
  (if (string= (shell-command-to-string
                "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true")
      (load-theme 'solarized-dark-high-contrast t)
    (load-theme 'solarized-light-high-contrast t)))

(provide 'my-functions)
;;; my-functions.el ends here
