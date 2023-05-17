;;; qrencode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "qrencode" "qrencode.el" (0 0 0 0))
;;; Generated autoloads from qrencode.el

(autoload 'qrencode-region "qrencode" "\
Encode region between BEG and END into a QR code and show in a buffer.

\(fn BEG END)" t nil)

(autoload 'qrencode-url-at-point "qrencode" "\
Encode any URL found at point." t nil)

(register-definition-prefixes "qrencode" '("qrencode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; qrencode-autoloads.el ends here
