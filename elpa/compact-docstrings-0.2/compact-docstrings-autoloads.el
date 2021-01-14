;;; compact-docstrings-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "compact-docstrings" "compact-docstrings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from compact-docstrings.el

(autoload 'compact-docstrings-mode "compact-docstrings" "\
Shrink empty lines in docstrings and doc comments.

If called interactively, enable Compact-Docstrings mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defalias 'shrink-docstrings #'compact-docstrings--mode-on)

(put 'global-compact-docstrings-mode 'globalized-minor-mode t)

(defvar global-compact-docstrings-mode nil "\
Non-nil if Global Compact-Docstrings mode is enabled.
See the `global-compact-docstrings-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-compact-docstrings-mode'.")

(custom-autoload 'global-compact-docstrings-mode "compact-docstrings" nil)

(autoload 'global-compact-docstrings-mode "compact-docstrings" "\
Toggle Compact-Docstrings mode in all buffers.
With prefix ARG, enable Global Compact-Docstrings mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Compact-Docstrings mode is enabled in all buffers where
`compact-docstrings--mode-on' would do it.
See `compact-docstrings-mode' for more information on Compact-Docstrings mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "compact-docstrings" '("compact-docstrings-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; compact-docstrings-autoloads.el ends here
