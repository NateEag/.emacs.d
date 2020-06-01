;;; project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "project" "project.el" (0 0 0 0))
;;; Generated autoloads from project.el

(autoload 'project-current "project" "\
Return the project instance in DIR or `default-directory'.
When no project found in DIR, and MAYBE-PROMPT is non-nil, ask
the user for a different directory to look in.  If that directory
is not a part of a detectable project either, return a
`transient' project instance rooted in it.

\(fn &optional MAYBE-PROMPT DIR)" nil nil)

(autoload 'project-find-regexp "project" "\
Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

\(fn REGEXP)" t nil)

(autoload 'project-or-external-find-regexp "project" "\
Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for.

\(fn REGEXP)" t nil)

(autoload 'project-find-file "project" "\
Visit a file (with completion) in the current project.
The completion default is the filename at point, if one is
recognized." t nil)

(autoload 'project-or-external-find-file "project" "\
Visit a file (with completion) in the current project or external roots.
The completion default is the filename at point, if one is
recognized." t nil)

(autoload 'project-search "project" "\
Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

\(fn REGEXP)" t nil)

(autoload 'project-query-replace-regexp "project" "\
Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
If you exit the query-replace, you can later continue the query-replace
loop using the command \\[fileloop-continue].

\(fn FROM TO)" t nil)

(autoload 'project-compile "project" "\
Run `compile' in the project root." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project" '("project-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; project-autoloads.el ends here
