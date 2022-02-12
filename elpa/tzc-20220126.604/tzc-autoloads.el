;;; tzc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tzc" "tzc.el" (0 0 0 0))
;;; Generated autoloads from tzc.el

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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tzc" '("tzc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tzc-autoloads.el ends here
