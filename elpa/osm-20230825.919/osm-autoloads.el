;;; osm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "osm" "osm.el" (0 0 0 0))
;;; Generated autoloads from osm.el
 (autoload 'osm-prefix-map "osm" nil t 'keymap)

(autoload 'osm-home "osm" "\
Go to home coordinates." t nil)

(autoload 'osm-goto "osm" "\
Go to LAT/LON/ZOOM.

\(fn LAT LON ZOOM)" t nil)

(autoload 'osm "osm" "\
Go to LINK.
When called interactively, call the function `osm-home'.

\(fn &rest LINK)" t nil)

(autoload 'osm-bookmark-jump "osm" "\
Jump to osm bookmark BM.

\(fn BM)" t nil)

(autoload 'osm-bookmark-delete "osm" "\
Delete osm bookmark BM.

\(fn BM)" t nil)

(autoload 'osm-bookmark-rename "osm" "\
Rename osm bookmark OLD-NAME.

\(fn OLD-NAME)" t nil)

(autoload 'osm-search "osm" "\
Globally search for NEEDLE and display the map.
If the prefix argument LUCKY is non-nil take the first result and jump there.

\(fn NEEDLE &optional LUCKY)" t nil)

(autoload 'osm-gpx-show "osm" "\
Show the tracks of gpx FILE in an `osm-mode' buffer.

\(fn FILE)" t nil)

(autoload 'osm-server "osm" "\
Select tile SERVER.

\(fn SERVER)" t nil)

(when (>= emacs-major-version 28) (add-to-list 'browse-url-default-handlers '("\\`geo:" . osm)))

(register-definition-prefixes "osm" '("osm-"))

;;;***

;;;### (autoloads nil nil ("osm-ol.el" "osm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; osm-autoloads.el ends here
