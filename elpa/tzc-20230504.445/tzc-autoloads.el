;;; tzc-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tzc" "tzc.el" (0 0 0 0))
;;; Generated autoloads from tzc.el

(autoload 'tzc-convert-time "tzc" "\
Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE.

\(fn TIME-STRING FROM-ZONE TO-ZONE)" t nil)

(autoload 'tzc-convert-current-time "tzc" "\
Convert current local time to TO-ZONE.

\(fn TO-ZONE)" t nil)

(autoload 'tzc-convert-time-to-favourite-time-zones "tzc" "\
Convert time in TIME-STRING from FROM-ZONE to `(tzc--favourite-time-zones)`.

\(fn TIME-STRING FROM-ZONE)" t nil)

(autoload 'tzc-convert-current-time-to-favourite-time-zones "tzc" "\
Convert current local time to `(tzc--favourite-time-zones)`." t nil)

(autoload 'tzc-convert-time-at-mark "tzc" "\
Convert time at the marked region to TO-ZONE.

\(fn TO-ZONE)" t nil)

(autoload 'tzc-world-clock-previous-or-next "tzc" "\
Get the `tzc-world-clock' buffer for PREVIOUS-OR-NEXT hour.

\(fn PREVIOUS-OR-NEXT)" nil nil)

(autoload 'tzc-world-clock-previous "tzc" "\
Get the `tzc-world-clock` for the previous hour." t nil)

(autoload 'tzc-world-clock-next "tzc" "\
Get the `tzc-world-clock` for the next hour." t nil)

(defvar tzc-world-clock-mode-map (let ((map (make-sparse-keymap))) (define-key map "n" #'tzc-world-clock-next) (define-key map "p" #'tzc-world-clock-previous) map))

(autoload 'tzc-world-clock "tzc" "\
Display a world clock buffer for time zones in `tzc-favourite-time-zones-alist`." t nil)

(autoload 'tzc-check-time-zone "tzc" "\
Check info for TIME-ZONE.

\(fn TIME-ZONE)" t nil)

(autoload 'tzc-convert-org-time-stamp-at-mark "tzc" "\
Convert `org-time-stamp` at the marked region to TO-ZONE.

\(fn TO-ZONE)" t nil)

(autoload 'tzc-convert-and-replace-org-time-stamp-at-mark "tzc" "\
Convert `org-time-stamp` at the marked region to TO-ZONE.

\(fn TO-ZONE)" t nil)

(autoload 'tzc-get-time-shift-between-zones "tzc" "\
Get time shift between FROM-ZONE and TO-ZONE.

\(fn FROM-ZONE TO-ZONE)" t nil)

(register-definition-prefixes "tzc" '("tzc-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tzc-autoloads.el ends here
