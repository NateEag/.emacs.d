;;; daily-log.el --- elisp code for working with my ad-hoc daily log format.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I keep a daily log, often with start/stop times.
;;
;; When I want to manipulate it somehow from Emacs, that code goes here.
;;
;; ...yes, I know I should just be using org-mode. No, I don't want to, because
;; I already have enough fancy twiddly bits to play with instead of working.
;;
;; My file format is ad-hoc and undocumented, but it doesn't do much and meets
;; my needs.

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

        (when (re-search-forward (concat "^Stopped: +" daily-log-time-regex) nil t)
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
          (setq times (cons time-delta times))))

      times)))

(defun daily-log-get-file-time-deltas (path)
  "Return a list of time deltas from the file in PATH."

  (when (file-exists-p path)
    ;; If we already have `path' open in a buffer, use it. Otherwise, open it.
    (let* ((buffer-visiting-path (find-buffer-visiting path))
           (buffer-for-path (or buffer-visiting-path
                                (find-file-noselect path)))
           time-deltas)

      (with-current-buffer buffer-for-path
        (setq time-deltas (daily-log-get-time-deltas)))

      ;; Clean up buffers we opened just for investigation.
      (unless buffer-visiting-path
        (kill-buffer buffer-for-path))

      time-deltas)))

(defun daily-log-get-current-week-time ()
  "Get the number of hours I have logged so far this week."
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

    (setq total-time (time-to-seconds total-time))))

(defun daily-log-show-current-week-time ()
  "Show the amount of time logged so far this week."

  (interactive)

  (message "Total time worked this week: %s"
           (format-seconds "%h hours, %m minutes"
                           (daily-log-get-current-week-time))))

(defun daily-log-show-current-week-time-remaining ()
  "Show the number of hours I still should work this week."

  (interactive)

  (let ((time-remaining (time-subtract (seconds-to-time (* 60 60 40))
                                       (daily-log-get-current-week-time))))
    (message "Total time remaining: %s" (format-seconds
                                         "%h hours, %m minutes"
                                         (time-to-seconds time-remaining)))))

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
