;;; pyvenv-auto-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyvenv-auto" "pyvenv-auto.el" (0 0 0 0))
;;; Generated autoloads from pyvenv-auto.el

(defvar pyvenv-auto-mode nil "\
Non-nil if Pyvenv-Auto mode is enabled.
See the `pyvenv-auto-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-auto-mode'.")

(custom-autoload 'pyvenv-auto-mode "pyvenv-auto" nil)

(autoload 'pyvenv-auto-mode "pyvenv-auto" "\
Turn on pyvenv-auto-mode.

If called interactively, enable Pyvenv-Auto mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pyvenv-auto" '("pyvenv-auto-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvenv-auto-autoloads.el ends here
