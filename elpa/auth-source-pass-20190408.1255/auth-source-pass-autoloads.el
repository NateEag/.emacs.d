;;; auth-source-pass-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "auth-source-pass" "auth-source-pass.el" (23727
;;;;;;  12863 286747 183000))
;;; Generated autoloads from auth-source-pass.el

(autoload 'auth-source-pass-enable "auth-source-pass" "\
Enable auth-source-password-store.

\(fn)" nil nil)

(autoload 'auth-source-pass-get "auth-source-pass" "\
Return the value associated to KEY in the password-store entry ENTRY.

ENTRY is the name of a password-store entry.
The key used to retrieve the password is the symbol `secret'.

The convention used as the format for a password-store file is
the following (see http://www.passwordstore.org/#organization):

secret
key1: value1
key2: value2

\(fn KEY ENTRY)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auth-source-pass-autoloads.el ends here
