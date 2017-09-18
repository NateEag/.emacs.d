;;; quick-peek-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "quick-peek" "quick-peek.el" (22976 1840 0
;;;;;;  0))
;;; Generated autoloads from quick-peek.el

(autoload 'quick-peek-overlay-at "quick-peek" "\
Find overlay for line at POS.

\(fn POS)" nil nil)

(autoload 'quick-peek-show "quick-peek" "\
Show STR in an inline window at POS.
MIN-H (default: 4) and MAX-H (default: 16) are bounds on the
height of the window.  Setting MAX-H to `none' allows the inline
window to expand past the bottom of the current Emacs window.

\(fn STR &optional POS MIN-H MAX-H)" nil nil)

(autoload 'quick-peek-hide "quick-peek" "\
Hide inline windows.
With non-nil POS, clear only windows on line below pos.
Return number of windows hidden.

\(fn &optional POS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; quick-peek-autoloads.el ends here
