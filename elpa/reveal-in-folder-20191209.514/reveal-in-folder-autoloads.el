;;; reveal-in-folder-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "reveal-in-folder" "reveal-in-folder.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from reveal-in-folder.el

(autoload 'reveal-in-folder-at-point "reveal-in-folder" "\
Reveal the current file in folder at point." t nil)

(autoload 'reveal-in-folder-this-buffer "reveal-in-folder" "\
Reveal the current buffer in folder." t nil)

(autoload 'reveal-in-folder "reveal-in-folder" "\
Reveal buffer/path depends on cursor condition." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "reveal-in-folder" '("reveal-in-folder--s")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; reveal-in-folder-autoloads.el ends here
