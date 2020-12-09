;;; monkeytype-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "monkeytype" "monkeytype.el" (0 0 0 0))
;;; Generated autoloads from monkeytype.el

(autoload 'monkeytype-region "monkeytype" "\
Type marked region from START to END.

\\[monkeytype-region]

\(fn START END)" t nil)

(autoload 'monkeytype-repeat "monkeytype" "\
Repeat run.

\\[monkeytype-repeat]" t nil)

(autoload 'monkeytype-dummy-text "monkeytype" "\
Dummy text.

\\[monkeytype-dummy-text]" t nil)

(autoload 'monkeytype-fortune "monkeytype" "\
Type fortune.

\\[monkeytype-fortune]" t nil)

(autoload 'monkeytype-buffer "monkeytype" "\
Type entire current buffet.

\\[monkeytype-buffer]" t nil)

(autoload 'monkeytype-pause "monkeytype" "\
Pause run.

\\[monkeytype-pause]" t nil)

(autoload 'monkeytype-stop "monkeytype" "\
Finish run.

\\[monkeytype-stop]" t nil)

(autoload 'monkeytype-resume "monkeytype" "\
Resume run.

\\[monkeytype-resume]" t nil)

(autoload 'monkeytype-mistyped-words "monkeytype" "\
Practice mistyped words for current test.

\\[monkeytype-mistyped-words]" t nil)

(autoload 'monkeytype-hard-transitions "monkeytype" "\
Practice hard key combinations/transitions for current test.

\\[monkeytype-hard-transitions]" t nil)

(autoload 'monkeytype-save-mistyped-words "monkeytype" "\
Save mistyped words for current test.

See also: `monkeytype-load-words-from-file'
See also: `monkeytype-most-mistyped-words'

\\[monkeytype-save-mistyped-words]" t nil)

(autoload 'monkeytype-save-hard-transitions "monkeytype" "\
Save hard transitions for current test.

See also: `monkeytype-load-words-from-file'

\\[monkeytype-save-hard-transition]" t nil)

(autoload 'monkeytype-load-text-from-file "monkeytype" "\
Prompt user to enter text-file to use for typing.
Buffer will be filled with the vale of `fill-column' if
`monkeytype-auto-fill' is set to true.

\\[monkeytype-load-text-from-file]" t nil)

(autoload 'monkeytype-load-words-from-file "monkeytype" "\
Prompt user to enter words-file to use for typing.

Words will be randomized if `monkeytype-randomize' is set to true.
Words will be downcased if `monkeytype-downcase' is set to true.
Words special characters will get removed based on
`monkeytype-excluded-chars-regexp'.
Buffer will be filled with the vale of `fill-column' if
`monkeytype-words-auto-fill' is set to true.

\\[monkeytype-load-words-from-file]" t nil)

(autoload 'monkeytype-region-as-words "monkeytype" "\
Put the marked region from START to END in typing buffer.

Words will be randomized if `monkeytype-randomize' is set to true.
Words will be downcased if `monkeytype-downcase' is set to true.
Words special characters will get removed based on
`monkeytype-excluded-chars-regexp'.
Buffer will be filled with the vale of `fill-column' if
`monkeytype-auto-fill' is set to true.

\\[monkeytype-region-as-words]

\(fn START END)" t nil)

(autoload 'monkeytype-most-mistyped-words "monkeytype" "\
Type most mistyped words from all word-files in `monkeytype-directory'.

See: `monkeytype-save-mistyped-words' for how word-files are saved.

\\[monkeytype-most-mistyped-words]" t nil)

(autoload 'monkeytype-mode "monkeytype" "\
Monkeytype mode is a minor mode for speed/touch typing.

If called interactively, enable Monkeytype mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{monkeytype-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "monkeytype" '("monke")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; monkeytype-autoloads.el ends here
