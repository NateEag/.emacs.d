;;; moody-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "moody" "moody.el" (0 0 0 0))
;;; Generated autoloads from moody.el

(autoload 'moody-replace-mode-line-buffer-identification "moody" "\
Use moody's variant of `mode-line-buffer-identification'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

\(fn &optional RESTORE)" t nil)

(autoload 'moody-replace-sml/mode-line-buffer-identification "moody" "\
Use moody's variant of `mode-line-buffer-identification'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

Use instead of `moody-replace-mode-line-buffer-identification'
if you use the `smart-mode-line' package, after `sml/setup' has
already been called.

\(fn &optional RESTORE)" t nil)

(autoload 'moody-replace-vc-mode "moody" "\
Use moody's variant of `vc-mode' mode-line element.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

\(fn &optional RESTORE)" t nil)

(autoload 'moody-replace-eldoc-minibuffer-message-function "moody" "\
Use moody's variant of `eldoc-minibuffer-message'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

\(fn &optional RESTORE)" t nil)

(autoload 'moody-replace-mode-line-front-space "moody" "\
Use moody's variant of `mode-line-front-space'.

If optional RESTORE is true, then go back to the default.
If called interactively, then toggle between the variants.

Adjust the display width so that subsequent character in the
mode-line are aligned with those in the buffer.  Unlike other
moody variants do not use any tab or ribbon.

\(fn &optional RESTORE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "moody" '("moody-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; moody-autoloads.el ends here
