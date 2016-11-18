;;; work-log-parser.el --- Extract useful info from my daily log files.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just hacking some stuff to process this stuff automatically, a few minutes
;; at a time.
;;
;; Given some time, eventually I should no longer need to think about how long
;; I've worked on a given day.

;;; Code:

(defvar work-log-parser-time-regex
  "\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{2\\} \\(A\\|P\\)M\\)"
  "Appallingly ugly regex for finding a naive localtime AM/PM time string.")

(defvar work-log-parser-start-time-regex
  (concat "^Started: " work-log-parser-time-regex "$")
  "Regex to find a start time in one of my work log documents.")

(defvar work-log-parser-stop-time-regex
  (concat "^Stopped: " work-log-parser-time-regex "$")
    "Regex to find a stop time in one of my work log documents.")

(provide 'work-log-parser)
;;; work-log-parser.el ends here
