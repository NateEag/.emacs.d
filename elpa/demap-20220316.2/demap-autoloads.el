;;; demap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "demap" "demap.el" (0 0 0 0))
;;; Generated autoloads from demap.el

(autoload 'demap-open "demap" "\
Open minimap in a side window.
makes a minimap buffer and shows it. if
MINIMAP-OR-NAME is non-nil or a minimap with the
name in `demap-minimap-default-name' exists, show
that minimap instead. if the minimap is already
being shown, nothing happens.

FRAME specifies what frame to look for windows
that already show the minimap. it should be a live
frame or one of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

\(fn &optional MINIMAP-OR-NAME FRAME)" t nil)

(autoload 'demap-close "demap" "\
Close side window showing a minimap.
close a side window showing MINIMAP-OR-NAME. has no
effect on normal windows showing MINIMAP-OR-NAME.

a side window is a window made by
`display-buffer-in-side-window' (the default method
used by `demap-open').

FRAME specifies what frame to look for side windows
showing a minimap. it should be a live frame or one
of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

could kill MINIMAP-OR-NAME if
`demap-minimap-close-kill-minimap-p' is non-nil.

if a window is removed returns t, otherwise nil.

\(fn &optional MINIMAP-OR-NAME FRAME)" t nil)

(autoload 'demap-toggle "demap" "\
Toggle side window showing a minimap.
opens MINIMAP-OR-NAME in a side window. if its
already showing, removes it instead.

FRAME specifies what frame to look for side windows
showing a minimap. it should be a live frame or one
of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

see `demap-open' and `demap-close'
for more information.

\(fn &optional MINIMAP-OR-NAME FRAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demap" '("demap-minimap-")))

;;;***

;;;### (autoloads nil "demap-minimap" "demap-minimap.el" (0 0 0 0))
;;; Generated autoloads from demap-minimap.el

(autoload 'demap-minimap-construct "demap-minimap" "\
Construct a minimap.
NAME    is the name of the buffer.
        defaults to `demap-minimap-default-name'.
SHOWING is the buffer that the minimap is showing.
        defaults to a blank buffer.

\(fn &optional NAME SHOWING)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demap-minimap" '("demap-")))

;;;***

;;;### (autoloads nil "demap-modes" "demap-modes.el" (0 0 0 0))
;;; Generated autoloads from demap-modes.el

(autoload 'demap-define-minimap-miner-mode "demap-modes" "\
Define miner mode for demap minimap buffers.
expanded version of `define-minor-mode'.
modes defined with this macro will only work in a
demap minimap buffer.

this macro also adds a few options:
:protect
        variable or list of variables to copy when
        minimap reconstructs its buffer. the mode
        variable is implicitly protected. notice,
        these variables are made unprotected when
        the mode is disabled, regardless of whether
        other modes are protecting them or not.
:init-func
        form evaluated to set the mode variable to
        true. can also be used to initialize any
        hooks used by this mode. if this form dose
        not set the mode variable to a non-nil
        value, then the mode is still considered
        disabled. this will not be called while the
        mode is active.
:kill-func
        form evaluated to set the mode variable to
        nil. can also be used to uninitialized any
        hooks used by this mode. this form is also
        evaluated if the mode is active when the
        buffer is killed. if this dose not set the
        mode variable to nil then the mode is
        considered still activated. this will not
        be called while the mode is not active.
:set-func
        a function that sets the value of the mode
        variable. this option overrides :init-func
        and :kill-func. it should be a function
        that accepts one argument (STATE). STATE is
        the state that the mode variable should be
        set to. if the mode variable dose not
        change then nether dose the modes state.

the rest of the arguments are passed to
`define-minor-mode'.

\(fn MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)" nil t)

(function-put 'demap-define-minimap-miner-mode 'doc-string-elt '2)

(function-put 'demap-define-minimap-miner-mode 'lisp-indent-function '1)

(demap-define-minimap-miner-mode demap-track-window-mode "\
Minimap miner mode to make minimap show the active window.
makes the minimap this is active in show the buffer
in the currently active window. will not show the
window if `demap-track-window-mode-update-p-func'
returns nil.

this mode can only be used in a demap minimap buffer." :package-version '(demap . "1.0.0") :group 'demap :init-func (progn (setf demap-track-window-mode t) (->> (demap-buffer-minimap) (apply-partially #'demap-track-window-mode-update-as) (add-hook 'window-state-change-hook))) :kill-func (progn (->> (demap-buffer-minimap) (apply-partially #'demap-track-window-mode-update-as) (remove-hook 'window-state-change-hook)) (kill-local-variable 'demap-track-window-mode)))

(demap-define-minimap-miner-mode demap-current-line-mode "\
Minimap miner mode to highlight the current line.
this will use `demap-current-line-face' to
highlight the line, or
`demap-current-line-inactive-face' when the window
the current minimap is showing is not active.

this mode can only be used in a demap minimap buffer." :package-version '(demap . "1.0.0") :group 'demap :init-func (progn (setq demap-current-line-mode (make-overlay 0 0)) (-as-> #'demap--current-line-mode-wake-if func (add-hook 'demap-minimap-window-set-hook func nil t)) (-as-> #'demap--current-line-mode-sleep func (add-hook 'demap-minimap-window-sleep-hook func nil t))) :kill-func (progn (demap--current-line-mode-sleep) (delete-overlay demap-current-line-mode) (kill-local-variable 'demap-current-line-mode) (-as-> #'demap--current-line-mode-wake-if func (remove-hook 'demap-minimap-window-set-hook func t)) (-as-> #'demap--current-line-mode-sleep func (remove-hook 'demap-minimap-window-sleep-hook func t))))

(demap-define-minimap-miner-mode demap-visible-region-mode "\
minimap miner mode to show the visible region in minimaps window.
this highlights the area in the minimap visible
from the window it is showing. when the window
shown is active, the face
`demap-visible-region-face' is used, otherwise
`demap-visible-region-inactive-face' is used.

this mode can only be used in a demap minimap buffer." :package-version '(demap . "1.0.0") :group 'demap :init-func (progn (setq demap-visible-region-mode (make-overlay 0 0)) (-as-> #'demap--visible-region-mode-wake-if func (add-hook 'demap-minimap-window-set-hook func nil t)) (-as-> #'demap--visible-region-mode-rest func (add-hook 'demap-minimap-window-sleep-hook func nil t))) :kill-func (progn (demap--visible-region-mode-sleep) (delete-overlay demap-visible-region-mode) (kill-local-variable 'demap-visible-region-mode) (-as-> #'demap--visible-region-mode-wake-if func (remove-hook 'demap-minimap-window-set-hook func t)) (-as-> #'demap--visible-region-mode-rest func (remove-hook 'demap-minimap-window-sleep-hook func t))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demap-modes" '("demap-track-window-mode-update-p-func")))

;;;***

;;;### (autoloads nil "demap-tools" "demap-tools.el" (0 0 0 0))
;;; Generated autoloads from demap-tools.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demap-tools" '("demap--tools-")))

;;;***

;;;### (autoloads nil nil ("demap-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; demap-autoloads.el ends here
