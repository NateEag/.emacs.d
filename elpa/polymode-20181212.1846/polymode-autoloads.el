;;; polymode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "polymode" "polymode.el" (23577 6634 0 0))
;;; Generated autoloads from polymode.el

(autoload 'define-polymode "polymode" "\
Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command can be used as both major and minor mode. Using
polymodes as minor modes makes sense when :hostmode (see below)
is not specified, in which case polymode installs only inner
modes and doesn't touch current major mode.

Standard hook MODE-hook is run at the end of the initialization
of each polymode buffer (both indirect and base buffers).

This macro also defines the MODE-map keymap from the :keymap
argument and PARENT-map (see below) and pm-poly/[MODE-NAME]
custom variable which holds a `pm-polymode' configuration object
for this polymode.

PARENT is either the polymode configuration object or a polymode
mode (there is 1-to-1 correspondence between config
objects (`pm-polymode') and mode functions). The new polymode
MODE inherits alll the behavior from PARENT except for the
overwrites specified by the keywords (see below). The new MODE
runs all the hooks from the PARENT-mode and inherits its MODE-map
from PARENT-map.

DOC is an optional documentation string. If present PARENT must
be provided, but can be nil.

BODY is executed after the complete initialization of the
polymode but before MODE-hook. It is executed once for each
polymode buffer - host buffer on initialization and every inner
buffer subsequently created.

Before the BODY code keyword arguments (i.e. alternating keywords
and values) are allowed. The following special keywords
controlling the behavior of the new MODE are supported:

:lighter Optional LIGHTER is displayed in the mode line when the
   mode is on. If omitted, it defaults to the :lighter slot of
   CONFIG object.

:keymap If nil, a new MODE-map keymap is created what directly
  inherits from the PARENT's keymap. The last keymap in the
  inheritance chain is always `polymode-minor-mode-map'. If a
  keymap it is used directly as it is. If a list of binding of
  the form (KEY . BINDING) it is merged the bindings are added to
  the newly create keymap.

:after-hook A single form which is evaluated after the mode hooks
  have been run. It should not be quoted.

Other keywords are added to the `pm-polymode' configuration
object and should be valid slots in PARENT config object or the
root config `pm-polymode' object if PARENT is nil. By far the
most frequently used slots are:

:hostmode Symbol pointing to a `pm-host-chunkmode' object
  specifying the behavior of the hostmode. If missing or nil,
  MODE will behave as a minor-mode in the sense that it will
  reuse the currently installed major mode and will install only
  the inner modes.

:innermodes List of symbols pointing to `pm-inner-chunkmode'
  objects which specify the behavior of inner modes (or submodes).

\(fn MODE &optional PARENT DOC &rest BODY)" nil t)

(function-put 'define-polymode 'doc-string-elt '3)

;;;***

;;;### (autoloads nil "polymode-core" "polymode-core.el" (23577 6634
;;;;;;  0 0))
;;; Generated autoloads from polymode-core.el

(defvar-local polymode-default-inner-mode nil "\
Inner mode for chunks with unspecified modes.
Intended to be used as local variable in polymode buffers.")

(put 'polymode-default-inner-mode 'safe-local-variable 'symbolp)

;;;***

;;;### (autoloads nil "polymode-debug" "polymode-debug.el" (23577
;;;;;;  6634 0 0))
;;; Generated autoloads from polymode-debug.el

(autoload 'pm-debug-minor-mode "polymode-debug" "\
Turns on/off useful facilities for debugging polymode.

Key bindings:
\\{pm-debug-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'pm-debug-minor-mode-on "polymode-debug" "\


\(fn)" nil nil)

(defvar pm-debug-mode nil "\
Non-nil if Pm-Debug mode is enabled.
See the `pm-debug-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pm-debug-mode'.")

(custom-autoload 'pm-debug-mode "polymode-debug" nil)

(autoload 'pm-debug-mode "polymode-debug" "\
Toggle Pm-Debug minor mode in all buffers.
With prefix ARG, enable Pm-Debug mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pm-Debug minor mode is enabled in all buffers where
`pm-debug-minor-mode-on' would do it.
See `pm-debug-minor-mode' for more information on Pm-Debug minor mode.

\(fn &optional ARG)" t nil)

(autoload 'pm-toggle-tracing "polymode-debug" "\
Toggle polymode tracing.
With numeric prefix toggle tracing for that LEVEL. Currently
universal argument toggles maximum level of tracing (4). Default
level is 3.

\(fn LEVEL)" t nil)

(autoload 'pm-trace "polymode-debug" "\
Trace function FN.
Use `untrace-function' to untrace or `untrace-all' to untrace all
currently traced functions.

\(fn FN)" t nil)

(autoload 'pm-debug-print-relevant-variables "polymode-debug" "\
Print values of relevant hooks and other variables.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("poly-lock.el" "polymode-base.el" "polymode-classes.el"
;;;;;;  "polymode-compat.el" "polymode-export.el" "polymode-methods.el"
;;;;;;  "polymode-pkg.el" "polymode-tangle.el" "polymode-test-utils.el"
;;;;;;  "polymode-weave.el") (23577 6634 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; polymode-autoloads.el ends here
