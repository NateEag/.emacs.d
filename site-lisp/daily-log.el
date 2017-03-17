;;; daily-log.el --- elisp code for working with my ad-hoc daily log format.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I keep a daily log, often with start/stop times.
;;
;; When I want to manipulate it somehow from Emacs, that code goes here.

;;; Code:

(defvar daily-log-time-regex "\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{2\\} \\(A\\|P\\)M\\)")

(defun daily-log-get-times ()
  "Return a list of start and stop times in the current buffer.

Each element is a two-element list - the first item is start
time, the second item is stop time."

  (let (times start-time stop-time)

    (save-excursion
      (goto-char (point-min))

      (while (re-search-forward (concat "^Started: +" daily-log-time-regex) nil t)
        (setq start-time (match-string 1))

        (re-search-forward (concat "^Stopped: +" daily-log-time-regex))
        (setq stop-time (match-string 1))

        (setq times (append (list (list start-time stop-time)) times)))

      times)))

(provide 'daily-log)
;;; daily-log.el ends here
