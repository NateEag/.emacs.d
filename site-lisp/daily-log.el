;;; daily-log.el --- elisp code for working with my ad-hoc daily log format.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I keep a daily log, often with start/stop times.
;;
;; When I want to manipulate it somehow from Emacs, that code goes here.

;;; Code:

(defvar daily-log-time-regex "\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{2\\} \\(A\\|P\\)M\\)")

(defun daily-log-get-time-deltas ()
  "Return a list of time deltas represented by the current buffer.

Each element is a time object, derived by subtracting a start time
from the following stop time.

Note that we assume the buffer is named for the date whose
activity it records, in the format 'YYYY-MM-DD.txt'."

  (let (times start-time stop-time time-delta)

    (save-excursion
      (goto-char (point-min))

      (while (re-search-forward (concat "^Started: +" daily-log-time-regex) nil t)
        (setq start-time (date-to-time (concat (file-name-base)
                                                " "
                                                (match-string 1))))
        ;; Apparently date-to-time ignores AM/PM?
        ;;
        ;; TODO Find a less-crazy way to do this. Emacs *must* have a way.
        (when (and (s-ends-with? "PM" (match-string 1))
                   (not (equal "12" (format-time-string "%H" start-time))))
          (setq start-time (time-add start-time (days-to-time 0.5))))

        (re-search-forward (concat "^Stopped: +" daily-log-time-regex))
        (setq stop-time (date-to-time (concat (file-name-base)
                                              " "
                                              (match-string 1))))
        ;; Apparently date-to-time ignores AM/PM?
        ;;
        ;; TODO Find a less-crazy way to do this. Emacs *must* have a way.
        (when (and (s-ends-with? "PM" (match-string 1))
                   (not (equal "12" (format-time-string "%H" stop-time))))
          (setq stop-time (time-add stop-time (days-to-time 0.5))))

        (setq time-delta (time-subtract stop-time start-time))

        (setq times (cons time-delta times)))

      times)))

(defun daily-log-get-file-time-deltas (path)
  "Return a list of time deltas from the file in PATH."

 (let ((target-buffer (find-file-noselect path)))
   (with-current-buffer target-buffer
    (daily-log-get-time-deltas))
   ))

(defun daily-log-show-current-week-time ()
  "Show the amount of time logged so far this week."

  (interactive)

  (let* ((now (decode-time))
         (current-day-of-week (nth 6 now))
         (cur-time (current-time))
         (total-time (seconds-to-time 0)))

    (dotimes (i (+ current-day-of-week 1))
      (let* ((target-date (format-time-string
                           "%Y-%m-%d"
                           (time-subtract cur-time
                                          (days-to-time i))))
             (log-file-path (concat "~/daily-log/" target-date ".txt"))
             (time-deltas (daily-log-get-file-time-deltas log-file-path)))

        (dolist (time-delta time-deltas)
          (setq total-time (time-add time-delta total-time)))
        ))

    (setq total-time (time-to-seconds total-time))
    (message "Total time worked this week: %s" (format-seconds
                                                "%h hours, %m minutes"
                                                total-time))))

(defun daily-log-show-total-time ()
  "Show the amount of time logged in current buffer."

  (interactive)

  (let ((all-times (daily-log-get-time-deltas))
        (total-time (seconds-to-time 0)))

    (dolist (time-delta all-times)
      (setq total-time (time-add time-delta total-time)))

    (setq total-time (time-to-seconds total-time))

    (message "Total time worked: %s" (format-seconds "%h hours, %m minutes"
                                                     total-time))))

(provide 'daily-log)
;;; daily-log.el ends here
