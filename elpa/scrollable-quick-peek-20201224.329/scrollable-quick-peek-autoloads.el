;;; scrollable-quick-peek-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "scrollable-quick-peek" "scrollable-quick-peek.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from scrollable-quick-peek.el

(autoload 'scrollable-quick-peek-scroll-up "scrollable-quick-peek" "\
Scroll up in the currently displayed overlay." t nil)

(autoload 'scrollable-quick-peek-scroll-down "scrollable-quick-peek" "\
Scroll down in the currently displayed overlay." t nil)

(autoload 'scrollable-quick-peek-show "scrollable-quick-peek" "\
Show STR in an inline scrollable window at POS.
MIN-H (default: 4) and MAX-H (default: 16) are passed directly to
`quick-peek'. `quick-peek' also accepts a `'none' option for MIN-H
and MAX-H but these will not get passed given these don't make
sense for `scrollale-quick-peek'.

\(fn STR &optional POS MIN-H MAX-H)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scrollable-quick-peek" '("scrollable-quick-peek")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scrollable-quick-peek-autoloads.el ends here
