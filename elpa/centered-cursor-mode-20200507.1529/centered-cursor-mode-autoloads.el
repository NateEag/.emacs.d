;;; centered-cursor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "centered-cursor-mode" "centered-cursor-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centered-cursor-mode.el

(autoload 'ccm-visible-text-lines "centered-cursor-mode" "\
Visible text lines" nil nil)

(autoload 'centered-cursor-mode "centered-cursor-mode" "\
Makes the cursor stay vertically in a defined
position (usually centered).

If called interactively, enable Centered-Cursor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-centered-cursor-mode 'globalized-minor-mode t)

(defvar global-centered-cursor-mode nil "\
Non-nil if Global Centered-Cursor mode is enabled.
See the `global-centered-cursor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-centered-cursor-mode'.")

(custom-autoload 'global-centered-cursor-mode "centered-cursor-mode" nil)

(autoload 'global-centered-cursor-mode "centered-cursor-mode" "\
Toggle Centered-Cursor mode in all buffers.
With prefix ARG, enable Global Centered-Cursor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Centered-Cursor mode is enabled in all buffers where
`centered-cursor-mode' would do it.
See `centered-cursor-mode' for more information on Centered-Cursor mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centered-cursor-mode" '("animate-first-start-p" "ccm-" "recenter-sequence")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; centered-cursor-mode-autoloads.el ends here
