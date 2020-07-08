;;; prettier-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prettier" "prettier.el" (0 0 0 0))
;;; Generated autoloads from prettier.el

(put 'prettier-mode-sync-config-flag 'safe-local-variable 'booleanp)

(put 'prettier-editorconfig-flag 'safe-local-variable 'booleanp)

(put 'prettier-infer-parser-flag 'safe-local-variable 'booleanp)

(put 'prettier-parsers 'safe-local-variable 'listp)

(autoload 'prettier-prettify "prettier" "\
Prettify the whole current buffer, or the part it is narrowed to.

With prefix, ask for the parser to use" t nil)

(autoload 'prettier-prettify-region "prettier" "\
Prettify the current region.

With prefix, ask for the parser to use" t nil)

(autoload 'prettier-mode "prettier" "\
Runs prettier on file save when this mode is turned on

If called interactively, enable Prettier mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-prettier-mode 'globalized-minor-mode t)

(defvar global-prettier-mode nil "\
Non-nil if Global Prettier mode is enabled.
See the `global-prettier-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-prettier-mode'.")

(custom-autoload 'global-prettier-mode "prettier" nil)

(autoload 'global-prettier-mode "prettier" "\
Toggle Prettier mode in all buffers.
With prefix ARG, enable Global Prettier mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Prettier mode is enabled in all buffers where
`(lambda nil (when (and (not prettier-mode) (or (null prettier-mode-ignore-buffer-function) (not (funcall prettier-mode-ignore-buffer-function))) (prettier--parsers)) (with-temp-message (unless (eq prettier-pre-warm 'none) Prettier pre-warming...) (prettier-mode))))' would do it.
See `prettier-mode' for more information on Prettier mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prettier" '("prettier-")))

;;;***

;;;### (autoloads nil nil ("prettier-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prettier-autoloads.el ends here
