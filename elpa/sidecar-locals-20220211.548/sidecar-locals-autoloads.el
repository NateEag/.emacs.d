;;; sidecar-locals-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sidecar-locals" "sidecar-locals.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from sidecar-locals.el

(autoload 'sidecar-locals-report "sidecar-locals" "\
Report paths that are used to detect locals.

This creates a buffer with links that visit that file." t nil)

(defvar sidecar-locals-mode nil "\
Non-nil if Sidecar-Locals mode is enabled.
See the `sidecar-locals-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `sidecar-locals-mode'.")

(custom-autoload 'sidecar-locals-mode "sidecar-locals" nil)

(autoload 'sidecar-locals-mode "sidecar-locals" "\
Toggle variable `sidecar-locals-mode' in the current buffer.

If called interactively, enable Sidecar-Locals mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sidecar-locals" '("sidecar-locals-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sidecar-locals-autoloads.el ends here
