;;; frame-cmds-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "frame-cmds" "frame-cmds.el" (21565 23619 0
;;;;;;  0))
;;; Generated autoloads from frame-cmds.el

(autoload 'save-frame-config "frame-cmds" "\
Save current frame configuration.
You can restore it with \\[jump-to-frame-config-register].

\(fn)" t nil)

(autoload 'jump-to-frame-config-register "frame-cmds" "\
Restore frame configuration saved in `frame-config-register'.

\(fn)" t nil)

(autoload 'deiconify-everything "frame-cmds" "\
Deiconify any iconified frames.

\(fn)" t nil)

(autoload 'iconify-everything "frame-cmds" "\
Iconify all frames of session at once.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'hide-everything "frame-cmds" "\
Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible.
Remembers frame configuration in register `C-l' (Control-L).
To restore this frame configuration, use `\\[jump-to-register] C-l'.

\(fn)" t nil)

(autoload 'show-hide "frame-cmds" "\
1 frame visible: `show-hide-show-function'; else: `hide-everything'.
This acts as a toggle between showing all frames and showing only an
iconified minibuffer frame.

\(fn)" t nil)

(autoload 'show-buffer-menu "frame-cmds" "\
Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after
being made invisible.

\(fn)" t nil)

(autoload 'mouse-show-hide-mark-unmark "frame-cmds" "\
In minibuffer: `show-hide'.  In dired: mark/unmark; else: buffer menu.

\(fn EVENT)" t nil)

(defalias 'iconify/map-frame 'iconify/show-frame)

(autoload 'iconify/show-frame "frame-cmds" "\
Iconify selected frame if now shown.  Show it if now iconified.
A non-negative prefix arg iconifies all shown frames.
A negative prefix arg deiconifies all iconified frames.

\(fn &optional ALL-ACTION)" t nil)

(defalias 'mouse-iconify/map-frame 'mouse-iconify/show-frame)

(autoload 'mouse-iconify/show-frame "frame-cmds" "\
Iconify frame you click, if now shown.  Show it if now iconified.

\(fn EVENT)" t nil)

(autoload 'delete-windows-for "frame-cmds" "\
`delete-window' or prompt for buffer and delete its windows.
With no prefix arg, delete the selected window.
With a prefix arg, prompt for a buffer and delete all windows, on any
  frame, that show that buffer.

\(fn &optional BUFFER)" t nil)

(autoload 'delete-windows-on "frame-cmds" "\
Delete windows showing BUFFER.
Optional arg BUFFER defaults to the current buffer.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If t, delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is t (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames).

\(fn &optional BUFFER FRAME)" t nil)

(defalias 'remove-window 'delete-window)

(autoload 'remove-windows-on "frame-cmds" "\
Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'mouse-remove-window "frame-cmds" "\
Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click.

\(fn EVENT)" t nil)

(autoload 'delete/iconify-window "frame-cmds" "\
Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is t, then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn &optional WINDOW FRAME-P)" t nil)

(autoload 'delete/iconify-windows-on "frame-cmds" "\
For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If t, treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is t, then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is t.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted.

\(fn BUFFER &optional FRAME FRAME-P)" t nil)

(autoload 'rename-frame "frame-cmds" "\
Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg non-nil means rename all frames named OLD-NAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'rename-non-minibuffer-frame "frame-cmds" "\
Unless OLD-NAME names the minibuffer frame, use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.
Prefix arg non-nil means rename all frames named OLD-NAME to NEW-NAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'.

\(fn &optional OLD-NAME NEW-NAME ALL-NAMED)" t nil)

(autoload 'name-all-frames-numerically "frame-cmds" "\
Rename all frames to numerals in 1,2,3...
With optional arg STARTOVER (prefix arg, interactively), rename all
starting over from 1.  Otherwise, numbering continues from the highest
existing frame number.

\(fn &optional STARTOVER)" t nil)

(autoload 'name-frame-numerically "frame-cmds" "\
Name FRAME (default, selected frame) to a numeral in 1,2,3...
If FRAME's name is already such a numeral, do nothing.
Else:
 Rename it to a numeral one greater than the max numeric frame name.
 Rename any other frames to numerals also.

To automatically name new frames numerically, you can do this in your
init file:

  (add-hook 'after-make-frame-functions 'name-frame-numerically)

\(fn &optional FRAME FRAMES)" t nil)

(autoload 'show-frame "frame-cmds" "\
Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name.

\(fn FRAME)" t nil)

(autoload 'hide-frame "frame-cmds" "\
Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible.

\(fn FRAME &optional PREFIX)" t nil)

(autoload 'show-a-frame-on "frame-cmds" "\
Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string).

\(fn BUFFER)" t nil)

(autoload 'show-*Help*-buffer "frame-cmds" "\
Raise a frame showing buffer *Help*, without selecting it.

\(fn)" t nil)

(autoload 'delete-1-window-frames-on "frame-cmds" "\
Delete all visible 1-window frames showing BUFFER.

\(fn BUFFER)" t nil)

(autoload 'delete-other-frames "frame-cmds" "\
Delete all frames except FRAME (default: selected frame).
Interactively, use a prefix arg (`\\[universal-argument]') to be prompted for FRAME.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-horizontally "frame-cmds" "\
Maximize selected frame horizontally.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame-vertically "frame-cmds" "\
Maximize selected frame vertically.

\(fn &optional FRAME)" t nil)

(autoload 'maximize-frame "frame-cmds" "\
Maximize selected frame horizontally, vertically, or both.
With no prefix arg, maximize both directions.
With a non-negative prefix arg, maximize vertically.
With a negative prefix arg, maximize horizontally.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to maximize.

\(fn &optional DIRECTION FRAME)" t nil)

(unless (fboundp 'restore-frame-horizontally) (defalias 'restore-frame-horizontally 'toggle-max-frame-horizontally))

(autoload 'toggle-max-frame-horizontally "frame-cmds" "\
Toggle maximization of FRAME horizontally.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `left' and `width' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(unless (fboundp 'restore-frame-vertically) (defalias 'restore-frame-vertically 'toggle-max-frame-vertically))

(autoload 'toggle-max-frame-vertically "frame-cmds" "\
Toggle maximization of FRAME vertically.
If used once, this restores the frame.  If repeated, it maximizes.
This affects the `top' and `height' frame parameters.

FRAME defaults to the selected frame.

\(fn &optional FRAME)" t nil)

(unless (fboundp 'restore-frame) (defalias 'restore-frame 'toggle-max-frame))

(autoload 'toggle-max-frame "frame-cmds" "\
Toggle maximization of FRAME horizontally, vertically, or both.
Reverses or (if restored) repeats the effect of the Emacs maximize
commands.  Does not restore from maximization effected outside Emacs.

With no prefix arg, toggle both directions.
With a non-negative prefix arg, toggle only vertically.
With a negative prefix arg, toggle horizontally.

When toggling both directions, each is toggled from its last maximize
or restore state.  This means that using this after
`maximize-frame-horizontally', `maximize-frame-vertically',
`toggle-max-frame-horizontally', or `toggle-max-frame-vertically' does
not necessarily just reverse the effect of that command.

In Lisp code:
 DIRECTION is the direction: `horizontal', `vertical', or `both'.
 FRAME is the frame to change.  It defaults to the selected frame.

\(fn &optional DIRECTION FRAME)" t nil)

(autoload 'tile-frames-horizontally "frame-cmds" "\
Tile frames horizontally.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'tile-frames-vertically "frame-cmds" "\
Tile frames vertically.
Interatively:
  With prefix arg, you are prompted for names of two frames to tile.
  With no prefix arg, all visible frames are tiled, except a
       standalone minibuffer frame, if any.
If called from a program, all frames in list FRAMES are tiled.

\(fn &optional FRAMES)" t nil)

(autoload 'enlarge-frame "frame-cmds" "\
Increase the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'enlarge-frame-horizontally "frame-cmds" "\
Increase the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame "frame-cmds" "\
Decrease the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'shrink-frame-horizontally "frame-cmds" "\
Decrease the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'move-frame-down "frame-cmds" "\
Move selected frame down.
Move it N times `frame-char-height', where N is the prefix arg.
In Lisp code, FRAME is the frame to move.

\(fn &optional N FRAME)" t nil)

(autoload 'move-frame-up "frame-cmds" "\
Move selected frame up.
Same as `move-frame-down', except movement is up.

\(fn &optional N FRAME)" t nil)

(autoload 'move-frame-right "frame-cmds" "\
Move frame to the right.
Move it N times `frame-char-width', where N is the prefix arg.
In Lisp code, FRAME is the frame to move.

\(fn &optional N FRAME)" t nil)

(autoload 'move-frame-left "frame-cmds" "\
Move frame to the left.
Same as `move-frame-right', except movement is to the left.

\(fn &optional N FRAME)" t nil)

(autoload 'move-frame-to-screen-top "frame-cmds" "\
Move FRAME (default: selected-frame) to the top of the screen.
With a prefix arg, offset it that many char heights from the top.

\(fn ARG &optional FRAME)" t nil)

(autoload 'move-frame-to-screen-bottom "frame-cmds" "\
Move FRAME (default: selected-frame) to the bottom of the screen.
With a prefix arg, offset it that many char heights from the bottom.

\(fn ARG &optional FRAME)" t nil)

(autoload 'move-frame-to-screen-left "frame-cmds" "\
Move FRAME (default: selected-frame) to the left side of the screen.
With a prefix arg, offset it that many char widths from the left.

\(fn ARG &optional FRAME)" t nil)

(autoload 'move-frame-to-screen-right "frame-cmds" "\
Move FRAME (default: selected-frame) to the right side of the screen.
With a prefix arg, offset it that many char widths from the right.

\(fn ARG &optional FRAME)" t nil)

(autoload 'move-frame-to-screen-top-left "frame-cmds" "\
Move FRAME (default: selected-frame) to the top and left of the screen.
With a prefix arg, offset it that many char widths from the edges.

Note: You can use this command to move an off-screen (thus not
visible) frame back onto the screen.

\(fn ARG &optional FRAME)" t nil)

(autoload 'enlarge-font "frame-cmds" "\
Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'set-frame-alist-parameter-from-frame "frame-cmds" "\
Set PARAMETER of frame alist ALIST to its current value in FRAME.
FRAME defaults to the selected frame.  ALIST is a variable (symbol)
whose value is an alist of frame parameters.

\(fn ALIST PARAMETER &optional FRAME)" t nil)

(autoload 'set-all-frame-alist-parameters-from-frame "frame-cmds" "\
Set frame parameters of ALIST to their current values in FRAME.
Unless optional argument REALLY-ALL-P (prefix arg) is non-nil, the
frame parameters in list `frame-parameters-to-exclude' are
excluded: they are not copied from FRAME to ALIST.
ALIST is a variable (symbol) whose value is an alist of frame parameters.
FRAME defaults to the selected frame.

\(fn ALIST &optional FRAME REALLY-ALL-P)" t nil)

(autoload 'tell-customize-var-has-changed "frame-cmds" "\
Tell Customize to recognize that VARIABLE has been set (changed).
VARIABLE is a symbol that names a user option.

\(fn VARIABLE)" t nil)

(autoload 'other-window-or-frame "frame-cmds" "\
`other-frame', if `one-window-p'; otherwise, `other-window'.

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; frame-cmds-autoloads.el ends here
