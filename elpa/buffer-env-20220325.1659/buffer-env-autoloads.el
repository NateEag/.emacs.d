;;; buffer-env-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "buffer-env" "buffer-env.el" (0 0 0 0))
;;; Generated autoloads from buffer-env.el

(autoload 'buffer-env-update "buffer-env" "\
Update the process environment buffer locally.
FILE is executed in the way prescribed by `buffer-env-commands'
and the buffer-local values of `process-environment' and
`exec-path' are set accordingly.

If FILE omitted, a file with base name `buffer-env-script-name'
is looked up in the current directory and its parents; nothing
happens if no such file is found.  This makes this function
suitable for use in a normal hook.

When called interactively, ask for a FILE.

\(fn &optional FILE)" t nil)

(autoload 'buffer-env-reset "buffer-env" "\
Reset the process environment of this buffer to the default values." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffer-env" '("buffer-env-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; buffer-env-autoloads.el ends here
