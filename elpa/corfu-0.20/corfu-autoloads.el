;;; corfu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu" "corfu.el" (0 0 0 0))
;;; Generated autoloads from corfu.el

(autoload 'corfu-mode "corfu" "\
Completion Overlay Region FUnction.

If called interactively, enable Corfu mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'corfu-global-mode 'globalized-minor-mode t)

(defvar corfu-global-mode nil "\
Non-nil if Corfu-Global mode is enabled.
See the `corfu-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-global-mode'.")

(custom-autoload 'corfu-global-mode "corfu" nil)

(autoload 'corfu-global-mode "corfu" "\
Toggle Corfu mode in all buffers.
With prefix ARG, enable Corfu-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Corfu mode is enabled in all buffers where
`corfu--on' would do it.
See `corfu-mode' for more information on Corfu mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "corfu" '("corfu-")))

;;;***

;;;### (autoloads nil nil ("corfu-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corfu-autoloads.el ends here
