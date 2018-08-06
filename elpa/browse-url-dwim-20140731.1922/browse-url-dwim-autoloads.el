;;; browse-url-dwim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "browse-url-dwim" "browse-url-dwim.el" (23400
;;;;;;  19104 0 0))
;;; Generated autoloads from browse-url-dwim.el

(let ((loads (get 'browse-url-dwim 'custom-loads))) (if (member '"browse-url-dwim" loads) nil (put 'browse-url-dwim 'custom-loads (cons '"browse-url-dwim" loads))))

(let ((loads (get 'browse-url-dwim-keys 'custom-loads))) (if (member '"browse-url-dwim" loads) nil (put 'browse-url-dwim-keys 'custom-loads (cons '"browse-url-dwim" loads))))

(defvar browse-url-dwim-mode nil "\
Non-nil if Browse-Url-Dwim mode is enabled.
See the `browse-url-dwim-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `browse-url-dwim-mode'.")

(custom-autoload 'browse-url-dwim-mode "browse-url-dwim" nil)

(autoload 'browse-url-dwim-mode "browse-url-dwim" "\
Turn on `browse-url-dwim-mode'.

Turning on `browse-url-dwim' will activate keybindings as defined
in `customize'.  It may also install a command alias for `browse'
and `google' as controlled by `browse-url-dwim-install-aliases'.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle.

\(fn &optional ARG)" t nil)

(autoload 'browse-url-dwim "browse-url-dwim" "\
Opens a URL in an external browser.

When called interactively, `browse-url-dwim-get-url' will be
used to find an appropriate URL.

The browser used is as configured for `browse-url'.

\(fn URL)" t nil)

(autoload 'browse-url-dwim-search "browse-url-dwim" "\
Perform an Internet search for TEXT, or region, or interactive input.

If TEXT is a URL, browse to page directly.  Otherwise
invoke an Internet search using TEXT.  When called interactively,
TEXT may be taken from the region or entered at a prompt.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is used.

If GUESS is non-nil, an attempt will be made to extract a URL
from the context around the point.  If successful, this command
is equivalent to `browse-url-dwim'.

\(fn &optional TEXT SEARCH-URL GUESS)" t nil)

(autoload 'browse-url-dwim-guess "browse-url-dwim" "\
Perform Internet search or browse to URL under point, according to context.

Identical to calling `browse-url-dwim-search' with GUESS set
to non-nil.

Optional TEXT is a string to be submitted to the search
engine.

Optional SEARCH-URL overrides the default search engine
URL.

\(fn &optional TEXT SEARCH-URL)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; browse-url-dwim-autoloads.el ends here
