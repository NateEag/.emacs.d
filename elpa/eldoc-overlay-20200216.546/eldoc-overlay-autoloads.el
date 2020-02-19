;;; eldoc-overlay-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eldoc-overlay" "eldoc-overlay.el" (0 0 0 0))
;;; Generated autoloads from eldoc-overlay.el

(defvar eldoc-overlay-mode nil "\
Non-nil if Eldoc-Overlay mode is enabled.
See the `eldoc-overlay-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eldoc-overlay-mode'.")

(custom-autoload 'eldoc-overlay-mode "eldoc-overlay" nil)

(autoload 'eldoc-overlay-mode "eldoc-overlay" "\
Minor mode for displaying eldoc contextual documentation using a text overlay.

If called interactively, enable Eldoc-Overlay mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eldoc-overlay" '("eldoc-overlay-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-overlay-autoloads.el ends here
