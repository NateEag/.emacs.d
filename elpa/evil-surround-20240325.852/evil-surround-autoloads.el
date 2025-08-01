;;; evil-surround-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from evil-surround.el

(autoload 'evil-surround-delete "evil-surround" "\
Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted.

(fn CHAR &optional OUTER INNER)" t)
(autoload 'evil-surround-change "evil-surround" "\
Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `evil-surround-delete'.

(fn CHAR &optional OUTER INNER)" t)
(autoload 'evil-surround-mode "evil-surround" "\
Buffer-local minor mode to emulate surround.vim.

This is a minor mode.  If called interactively, toggle the
`Evil-Surround mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-surround-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{evil-surround-mode-map}

(fn &optional ARG)" t)
(autoload 'turn-on-evil-surround-mode "evil-surround" "\
Enable evil-surround-mode in the current buffer.")
(autoload 'turn-off-evil-surround-mode "evil-surround" "\
Disable evil-surround-mode in the current buffer.")
(put 'global-evil-surround-mode 'globalized-minor-mode t)
(defvar global-evil-surround-mode nil "\
Non-nil if Global Evil-Surround mode is enabled.
See the `global-evil-surround-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-surround-mode'.")
(custom-autoload 'global-evil-surround-mode "evil-surround" nil)
(autoload 'global-evil-surround-mode "evil-surround" "\
Toggle Evil-Surround mode in all buffers.
With prefix ARG, enable Global Evil-Surround mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Surround mode is enabled in all buffers where `turn-on-evil-surround-mode'
would do it.

See `evil-surround-mode' for more information on Evil-Surround mode.

(fn &optional ARG)" t)
(register-definition-prefixes "evil-surround" '("evil-surround-"))

;;; End of scraped data

(provide 'evil-surround-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; evil-surround-autoloads.el ends here
