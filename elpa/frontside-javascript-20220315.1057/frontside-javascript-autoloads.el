;;; frontside-javascript-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frontside-javascript" "frontside-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from frontside-javascript.el

(autoload 'frontside-javascript "frontside-javascript" "\
Make Emacs have your back no matter what JavaScript project you'refaced with.
This is the main entry point which configures JS, JSX, TS, TSX, and NodeJS development" t nil)

(setq use-package--frontside-javascript--pre-config-hook #'frontside-javascript)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frontside-javascript" '("frontside-javascript--deno-project-p")))

;;;***

;;;### (autoloads nil nil ("frontside-javascript-pkg.el" "test-suppport.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frontside-javascript-autoloads.el ends here
