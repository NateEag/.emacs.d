;;; corfu-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corfu-doc" "corfu-doc.el" (0 0 0 0))
;;; Generated autoloads from corfu-doc.el

(autoload 'corfu-doc-scroll-up "corfu-doc" "\


\(fn &optional ARG)" t nil)

(autoload 'corfu-doc-scroll-down "corfu-doc" "\


\(fn &optional ARG)" t nil)

(autoload 'corfu-doc-mode "corfu-doc" "\
Corfu doc minor mode.

If called interactively, enable Corfu-Doc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'corfu-doc-toggle "corfu-doc" "\
Toggles the doc popup display or hide.

When using this command to manually hide the doc popup, it will
not be displayed until this command is called again. Even if the
corfu doc mode is turned on and `corfu-doc-auto' is set to Non-nil." t nil)

(autoload 'toggle-corfu-doc-mode "corfu-doc" "\
Toggles corfu doc mode on or off.
With optional ARG, turn corfu doc mode on if and only if ARG is positive.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "corfu-doc" '("corfu-doc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corfu-doc-autoloads.el ends here
