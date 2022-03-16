;;; osm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "osm" "osm.el" (0 0 0 0))
;;; Generated autoloads from osm.el

(autoload 'osm-home "osm" "\
Go to home coordinates." t nil)

(autoload 'osm-goto "osm" "\
Go to LAT/LON/ZOOM.

\(fn LAT LON ZOOM)" t nil)

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
Search for location and display the map." t nil)

(autoload 'osm-gpx-show "osm" "\
Show the tracks of gpx FILE in an `osm-mode' buffer.

\(fn FILE)" t nil)

(autoload 'osm-server "osm" "\
Select tile SERVER.

\(fn SERVER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "osm" '("osm-")))

;;;***

;;;### (autoloads nil "osm-ol" "osm-ol.el" (0 0 0 0))
;;; Generated autoloads from osm-ol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "osm-ol" '("osm-ol-")))

;;;***

;;;### (autoloads nil nil ("osm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; osm-autoloads.el ends here
