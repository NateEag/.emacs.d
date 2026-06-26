;;; tzc-org.el --- Org mode integration for tzc  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Keywords: convenience, time zone, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Integration between tzc (time zone converter) and Org mode.
;; Provides `tzc-org-schedule' for scheduling org items with time zone conversion.
;; Provides `tzc-org-deadline' for scheduling org items with time zone conversion.

;;; Code:
(require 'tzc)
(require 'org-element)
(require 'transient)

(defcustom tzc-org-local-time-zone (format-time-string "%z" (current-time))
  "Default local time zone or offset to use when converting org timestamp."
  :type 'string
  :group 'tzc)

(defun tzc-org--get-planning-ts (schedule-or-deadline)
  "Get the timestamp for SCHEDULE-OR-DEADLINE.
Return org timestamp as (STRING BEGIN END)."
  (let* ((ctx (org-element-context))
         (ts (org-element-property schedule-or-deadline ctx)))
    (list
     (org-element-property :raw-value ts)
     (org-element-property :begin ts)
     (org-element-property :end ts))))

(defun tzc-org--schedule-or-deadline (schedule-or-deadline)
  "SCHEDULE-OR-DEADLINE with time zone conversion on the fly.
SCHEDULE-OR-DEADLINE can be SCHEDULED or DEADLINE."
  ;; Get date and time using org-read-date (which returns both date and time)
  (let* ((from-datetime (org-read-date nil t nil (format "Enter %s date and time: " schedule-or-deadline)))
	 ;; Parse the datetime to get all components
	 (org-timestamp (format-time-string "<%F %a %R>" from-datetime))
	 ;; Get from-zone
	 (from-zone (completing-read (format "Enter a time zone or UTC offset (default %s): " tzc-org-local-time-zone)
				     (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))
				     nil t nil nil tzc-org-local-time-zone))
	 ;; Get to-zone
	 (to-zone (completing-read (format "Convert %s from %s to time zone or UTC offset (default %s): "
					   org-timestamp from-zone tzc-org-local-time-zone)
				     (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))
				     nil t nil nil tzc-org-local-time-zone))
	 ;; Add zoneinfo to the timestamp
	 (org-timestamp-with-zoneinfo (concat (string-replace ">" (concat " " from-zone) org-timestamp) ">"))
	 ;; Convert the timestamp using tzc
	 (converted-timestamp (tzc-convert-org-timestamp org-timestamp-with-zoneinfo to-zone))
	 (ts))
    
    (setq ts (cond ((string-equal schedule-or-deadline "SCHEDULED") (tzc-org--get-planning-ts :scheduled))
		   ((string-equal schedule-or-deadline "DEADLINE") (tzc-org--get-planning-ts :deadline))))
    (if (nth 0 ts)
	(progn
	  (goto-char (nth 1 ts))
	  (delete-region (nth 1 ts) (nth 2 ts))
	  (insert converted-timestamp " "))
      (org-back-to-heading t)
      (forward-line 1)
      (insert (format "%s: " schedule-or-deadline) converted-timestamp " "))))

;;;###autoload
(defun tzc-org-schedule ()
  "Schedule an org item with time zone conversion.
Similar to `org-schedule', but prompts for time zone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive)
  (tzc-org--schedule-or-deadline "SCHEDULED"))

;;;###autoload
(defun tzc-org-deadline ()
  "Schedule an org item with time zone conversion.
Similar to `org-deadline', but prompts for time zone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive)
  (tzc-org--schedule-or-deadline "DEADLINE"))

;;;world clock for time at point
(defun tzc-world-clock-for-org-timestamp-at-point ()
"Get a `world-clock' for the timestamp at point."
  (interactive)
  ;;; remove existing world clock
  (when (get-buffer tzc-world-clock-buffer-name)
    (kill-buffer tzc-world-clock-buffer-name))
  (let* ((timestamp (nth 0 (tzc--get-timestamp-at-point))))
    (tzc-world-clock (org-time-string-to-time timestamp)
		     (plist-get (tzc--get-time-zone-from-timestamp timestamp) :tz))))

(transient-define-prefix tzc-org-timestamp-dispatch ()
  "TZC operations for Org timestamp at point."
  [:description
   (lambda () (format "TZC: %s" (nth 0 (tzc--get-timestamp-at-point))))

   ["Convert"
    ("c" "Convert (keep original)" tzc-convert-org-timestamp-at-mark)
    ("r" "Convert (replace)" tzc-convert-and-replace-org-timestamp-at-mark)]

   ["Time Zone"
    ("m" "modify (add or update) time zone" tzc-add-or-update-time-zone-in-timestamp-at-point)]

   ["Schedule"
    ("s" "Schedule" tzc-org-schedule)
    ("d" "Deadline" tzc-org-deadline)]

   ["Inspect"
    ("v" "View in world clock" tzc-world-clock-for-org-timestamp-at-point)]

   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defvar tzc-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x z") #'tzc-org-timestamp-dispatch)
    map)
  "Keymap for `tzc-org-mode'.")

;;;###autoload
(define-minor-mode tzc-org-mode
  "Minor mode for TZC org features."
  :lighter " TZC"
  :keymap tzc-org-mode-map)

(provide 'tzc-org)
;;; tzc-org.el ends here


