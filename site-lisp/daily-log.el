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
        (setq start-time (date-to-time  (concat (file-name-base)
                                                " "
                                                (match-string 1))))

        (re-search-forward (concat "^Stopped: +" daily-log-time-regex))
        (setq stop-time (date-to-time (concat (file-name-base)
                                              " "
                                              (match-string 1))))

        (setq time-delta (time-subtract stop-time start-time))

        (setq times (cons time-delta times)))
      times)))

(defun daily-log-show-total-time ()
  "Show the amount of time logged in current buffer."

  (interactive)

  (let ((all-times (daily-log-get-time-deltas))
        (total-time (seconds-to-time 0)))

    (message "There are %i time deltas." (list-length all-times))
    (dolist (time-delta all-times)
      (message "Seconds worked: %s" (time-to-seconds time-delta))
      (message "Time worked: %s" (format-seconds "%h hours, %m minutes" (time-to-seconds time-delta)))
      (setq total-time (time-add time-delta total-time))
      )

    (setq total-time (time-to-seconds total-time))

    (message "In seconds: %i" total-time)
    (message "Total time worked: %s" (format-seconds "%h hours, %m minutes"
                                                     total-time)))
  )

(provide 'daily-log)
;;; daily-log.el ends here
