;;; editorconfig-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "editorconfig" "editorconfig.el" (23706 7727
;;;;;;  881882 701000))
;;; Generated autoloads from editorconfig.el

(autoload 'editorconfig-apply "editorconfig" "\
Get and apply EditorConfig properties to current buffer.
This function ignores `editorconfig-exclude-modes' and
`editorconfig-exclude-regexps', and always applies available properties.

\(fn)" t nil)

(defvar editorconfig-mode nil "\
Non-nil if Editorconfig mode is enabled.
See the `editorconfig-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `editorconfig-mode'.")

(custom-autoload 'editorconfig-mode "editorconfig" nil)

(autoload 'editorconfig-mode "editorconfig" "\
Toggle EditorConfig feature.

To disable EditorConfig in some buffers, modify
`editorconfig-exclude-modes' or `editorconfig-exclude-regexps'.

\(fn &optional ARG)" t nil)

(autoload 'editorconfig-find-current-editorconfig "editorconfig" "\
Find the closest .editorconfig file for current file.

\(fn)" t nil)

(autoload 'editorconfig-display-current-properties "editorconfig" "\
Display EditorConfig properties extracted for current buffer.

\(fn)" t nil)

(defalias 'describe-editorconfig-properties 'editorconfig-display-current-properties)

(autoload 'editorconfig-format-buffer "editorconfig" "\
Format buffer according to .editorconfig indent_style and indent_width.

\(fn)" t nil)

(autoload 'editorconfig-version "editorconfig" "\
Get EditorConfig version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

\(fn &optional SHOW-VERSION)" t nil)

;;;***

;;;### (autoloads nil "editorconfig-conf-mode" "editorconfig-conf-mode.el"
;;;;;;  (23706 7727 882878 361000))
;;; Generated autoloads from editorconfig-conf-mode.el

(autoload 'editorconfig-conf-mode "editorconfig-conf-mode" "\
Major mode for editing .editorconfig files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.editorconfig\\'" . editorconfig-conf-mode))

;;;***

;;;### (autoloads nil "editorconfig-core" "editorconfig-core.el"
;;;;;;  (23706 7727 884572 137000))
;;; Generated autoloads from editorconfig-core.el

(autoload 'editorconfig-core-get-nearest-editorconfig "editorconfig-core" "\
Return path to .editorconfig file that is closest to DIRECTORY.

\(fn DIRECTORY)" nil nil)

(autoload 'editorconfig-core-get-properties "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This functions returns alist of properties.  Each element will look like
'(KEY . VALUE) .

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

(autoload 'editorconfig-core-get-properties-hash "editorconfig-core" "\
Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead.

\(fn &optional FILE CONFNAME CONFVERSION)" nil nil)

;;;***

;;;### (autoloads nil "editorconfig-fnmatch" "editorconfig-fnmatch.el"
;;;;;;  (23706 7727 879951 82000))
;;; Generated autoloads from editorconfig-fnmatch.el

(autoload 'editorconfig-fnmatch-p "editorconfig-fnmatch" "\
Test whether STRING match PATTERN.

Matching ignores case if `case-fold-search' is non-nil.

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings can
be used:

*           Matches any string of characters, except path separators (/)
**          Matches any string of characters
?           Matches any single character
\[name]      Matches any single character in name
\[^name]     Matches any single character not in name
{s1,s2,s3}  Matches any of the strings given (separated by commas)
{min..max}  Matches any number between min and max

\(fn STRING PATTERN)" nil nil)

;;;***

;;;### (autoloads nil nil ("editorconfig-core-handle.el" "editorconfig-pkg.el")
;;;;;;  (23706 7727 886455 769000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; editorconfig-autoloads.el ends here
