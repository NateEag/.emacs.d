;;; vc-msg-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vc-msg" "vc-msg.el" (0 0 0 0))
;;; Generated autoloads from vc-msg.el

(autoload 'vc-msg-show "vc-msg" "\
Show commit message of current line.
If Git is used and some text inside the line is selected,
the correct commit which submits the selected text is displayed." t nil)

(register-definition-prefixes "vc-msg" '("vc-msg-"))

;;;***

;;;### (autoloads nil "vc-msg-git" "vc-msg-git.el" (0 0 0 0))
;;; Generated autoloads from vc-msg-git.el

aautoload 'vc-msg-git-blame-arguments "vc-msg-git" "\
Git blame at LINE-NUM.
Note git option `-C' track text copied elsewhere,
`-M' tracked moved content inside file.
See https://www.kernel.org/pub/software/scm/git/docs/git-blame.html

\(fn LINE-NUM)" nil nil)

(autoload 'vc-msg-git-execute "vc-msg-git" "\
Use FILE, LINE-NUM and VERSION to produce git command.
Parse the command execution output and return a plist:
'(:id str :author str :author-time str :summary str).

\(fn FILE LINE-NUM VERSION)" nil nil)

(autoload 'vc-msg-git-format "vc-msg-git" "\
Format the message for popup from INFO.

\(fn INFO)" nil nil)

(register-definition-prefixes "vc-msg-git" '("vc-msg-git-"))

;;;***

;;;### (autoloads nil "vc-msg-hg" "vc-msg-hg.el" (0 0 0 0))
;;; Generated autoloads from vc-msg-hg.el

(autoload 'vc-msg-hg-execute "vc-msg-hg" "\
Use FILE, LINE-NUM and VERSION to produce hg command.
Parse the command execution output and return a plist:
'(:id str :author str :date str :message str).

\(fn FILE LINE-NUM &optional VERSION)" nil nil)

(autoload 'vc-msg-hg-format "vc-msg-hg" "\
Format popup message from INFO.

\(fn INFO)" nil nil)

(register-definition-prefixes "vc-msg-hg" '("vc-msg-hg-"))

;;;***

;;;### (autoloads nil "vc-msg-p4" "vc-msg-p4.el" (0 0 0 0))
;;; Generated autoloads from vc-msg-p4.el

(autoload 'vc-msg-p4-execute "vc-msg-p4" "\
Use FILE, LINE-NUM and VERSION to produce p4 command.
Parse the command execution output and return a plist:
'(:id str :author str :date str :message str).

\(fn FILE LINE-NUM &optional VERSION)" nil nil)

(autoload 'vc-msg-p4-format "vc-msg-p4" "\
Format the INFO into a string.

\(fn INFO)" nil nil)

(register-definition-prefixes "vc-msg-p4" '("vc-msg-p4-"))

;;;***

;;;### (autoloads nil "vc-msg-sdk" "vc-msg-sdk.el" (0 0 0 0))
;;; Generated autoloads from vc-msg-sdk.el

(register-definition-prefixes "vc-msg-sdk" '("vc-msg-sdk-"))

;;;***

;;;### (autoloads nil "vc-msg-svn" "vc-msg-svn.el" (0 0 0 0))
;;; Generated autoloads from vc-msg-svn.el

(autoload 'vc-msg-svn-execute "vc-msg-svn" "\
Use FILE, LINE-NUM and VERSION to produce svn command.
Parse the command execution output and return a plist:
'(:id str :author str :date str :message str).

\(fn FILE LINE-NUM &optional VERSION)" nil nil)

(autoload 'vc-msg-svn-format "vc-msg-svn" "\
Format the message to display from INFO.

\(fn INFO)" nil nil)

(register-definition-prefixes "vc-msg-svn" '("vc-msg-svn-"))

;;;***

;;;### (autoloads nil nil ("vc-msg-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vc-msg-autoloads.el ends here
