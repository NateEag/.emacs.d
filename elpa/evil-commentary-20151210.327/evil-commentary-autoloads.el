;;; evil-commentary-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-commentary" "evil-commentary.el" (22132
;;;;;;  53896 0 0))
;;; Generated autoloads from evil-commentary.el

(defvar evil-commentary-mode nil "\
Non-nil if Evil-Commentary mode is enabled.
See the command `evil-commentary-mode' for a description of this minor mode.")

(custom-autoload 'evil-commentary-mode "evil-commentary" nil)

(autoload 'evil-commentary-mode "evil-commentary" "\
Commentary mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-commentary/org-comment-or-uncomment-region "evil-commentary" "\
Comment function for `org-mode'.

\(fn BEG END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-commentary-autoloads.el ends here
