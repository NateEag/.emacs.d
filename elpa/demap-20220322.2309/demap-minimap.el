;;; demap-minimap.el --- Demap minimap core file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Created: January 03, 2022
;; Modified: March 22, 2022
;; Version: 1.4.0
;; Keywords: lisp convenience
;; Homepage: https://gitlab.com/sawyerjgardner/demap.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; this file contains the core of demap.el's implementation. this includes the
;; definitions for the minimap object, buffer, custom variables, faces, and
;; hooks. wile there is little here meant for the 'user' to care about (besides
;; the faces or custom variables), custom modes or external packages can use the
;; functions defined here to change or add features to minimaps.
;;
;; demap minimaps are objects constructed from `demap-minimap-construct'. these objects
;; are associated with a buffer (obtained with `demap-minimap-buffer') often
;; referred to has the minimap-buffer. minimaps also have a buffer it is showing
;; (obtained or set with `demap-minimap-showing'). while not required,
;; `demap-minimap-window' can set a minimap to show the buffer in a window, and
;; pass the window to `demap-minimap-window-set-hook'.
;;
;; sometimes when a minimap changes what it is showing, it needs to reconstruct
;; its buffer. to help with this are the hooks `demap-minimap-change-functions',
;; `demap-minimap-change-major-mode-hook' and the list
;; `demap-minimap-protected-variables'.
;;
;; examples on how to use minimap objects can be found in 'demap-modes.el'.
;;
;; code layout:
;;      variables               ; custom variables and hooks
;;      minimap struct          ; minimap object definition
;;              protect         ; management of protected variables
;;              buffer          ; management of minimap buffers
;;              construct       ; construct minimap object
;;              showing         ; the buffer minimap is showing
;;              window          ; window minimap is showing
;;      obsolete
;;              window current  ; window the current minimap is showing
;;
;;; Code:

(require 'demap-tools)


(eval-and-compile
  (require 'cl-lib)
  (require 'subr-x)
  (when (>= emacs-major-version 28)
    ;;window.el doesn't provide 'window before version 28
    (require 'window) ))


;;variables

(defface demap-minimap-font-face
  '((t :family  "DejaVu Sans Mono" :height  30))
  "Face used for demap minimap buffers."
  :package-version '(demap . "1.0.0")
  :group 'demap )

(defcustom demap-minimap-default-name "*Minimap*"
  "The default name for demap minimaps."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type 'string )

(defcustom demap-minimap-construct-hook '(demap-track-window-mode
                                          demap-current-line-mode
                                          demap-visible-region-mode )
  "Normal hook ran after construction of a demap minimap.
this hook is ran has the buffer used by the new minimap."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-change-functions 'permanent-local t)
(defcustom demap-minimap-change-functions nil
  "Hook ran when changing a demap minimap's buffer.
when a minimap needs to rebuild its buffer,
it will copy all protected variables to the new
buffer then run this hook has the old buffer.
the functions in this hook should take one argument
\(MINIMAP). MINIMAP is the minimap that is changing."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-kill-hook 'permanent-local t)
(defcustom demap-minimap-kill-hook nil
  "Normal hook ran when killing a demap minimap.
note that minimaps sometimes need to rebuild
its buffer. when this happens, `buffer-kill-hook'
gets called but not `demap-minimap-kill-hook'."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-change-major-mode-hook 'permanent-local t)
(defcustom demap-minimap-change-major-mode-hook nil
  "Normal hook ran in `change-major-mode-hook'.
this is only activated inside demap minimap
buffers. the only reason to use this over
`change-major-mode-hook' is that this hook is
marked has a permanent-local variable and it's
local value is not cleared when the minimap
rebuilds its buffer."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-window-set-hook 'permanent-local t)
(defcustom demap-minimap-window-set-hook nil
  "Normal hook ran when demap minimaps set the window it is showing.
you can get the window the minimap is set to show
from `demap-minimap-window'."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-window-sleep-hook 'permanent-local t)
(defcustom demap-minimap-window-sleep-hook nil
  "Normal hook ran when demap minimaps rests.
this is called when the window demap minimap is
showing is no longer the active window."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  'hook )

(put 'demap-minimap-protected-variables 'permanent-local t)
(defcustom demap-minimap-protected-variables nil
  "List of variables copied when demap minimaps rebuilds their buffer.
the buffer-local values of these variables are
copied from the old buffer to the new one when
the minimap rebuilds its buffer. use
`demap-minimap-protect-variables' and
`demap-minimap-unprotect-variables' to add and
remove variables from this list."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :type  '(repeat variable) )


(put 'demap--current-minimap 'permanent-local t)
(defvar-local demap--current-minimap nil
  "The minimap associated with this buffer.
see `demap-buffer-minimap'." )

(put 'demap--minimap-window 'permanent-local t)
(defvar-local demap--minimap-window nil
  "The window that the current minimap is showing.
see `demap-minimap-window'." )


;;;buffer

(defun demap--buffer-new-name(&optional name ignore)
  "Return a string that is the name of no existing buffer based on NAME.
name defaults to the value in
`demap-minimap-default-name' otherwise its
identical to
\(`generate-new-buffer-name' NAME IGNORE)"
  (generate-new-buffer-name (or name demap-minimap-default-name) ignore) )

(defun demap--make-indirect-buffer(base-buffer name &optional clone)
  "Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.
if BASE-BUFFER is nil then return a blank buffer,
otherwise this is identical to
\(`make-indirect-buffer' BASE-BUFFER NAME CLONE)."
  (if base-buffer
      (make-indirect-buffer base-buffer name clone)
    (generate-new-buffer name) ))

(defun demap--buffer-init(buffer)
  "Set buffer local values in minimap buffer BUFFER.
sets the buffer local values a new minimap buffer
should have by default.
returns BUFFER."
  (with-current-buffer buffer
    (buffer-face-set 'demap-minimap-font-face)
    (setq-local auto-hscroll-mode   nil
                vertical-scroll-bar nil
                truncate-lines      t
                buffer-read-only    t ))
  buffer )

(defun demap--buffer-construct(&optional name show)
  "Construct a buffer for a demap minimap.
using name NAME and showing buffer SHOW."
  (thread-last
    (demap--buffer-new-name name)
    (demap--make-indirect-buffer show)
    (demap--buffer-init) ))

(defun demap--buffer-change-showing(buffer &optional show)
  "Reconstruct indirect BUFFER to show SHOW.
returns the reconstructed buffer. BUFFER will be
left in an undefined state."
  (let ((new (thread-first
               (demap--tools-buffer-steal-name buffer)
               (demap--buffer-construct show) )))
    (demap--tools-window-replace-buffer buffer new t)
    new ))


;;;minimap struct

(cl-defstruct (demap-minimap
               (:copier nil)
               (:constructor demap--minimap-construct-blank) )
  (-buffer nil
           :type 'buffer
           :documentation "The buffer associated with the minimap.
this slot is read only." ))

(defun demap-minimap-live-p(minimap)
  "Return t if MINIMAP is a live demap minimap.
Value is nil if MINIMAP is not a demap minimap or if it has been killed."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap--buffer minimap)) ))

(defun demap-normalize-minimap(minimap-or-name)
  "Return the demap minimap specified by MINIMAP-OR-NAME.
MINIMAP-OR-NAME must be a live minimap, a live
minimap buffer, a string naming a live minimap or
nil which means to return the minimap for the
current buffer."
  (cond ((demap-minimap-p minimap-or-name) minimap-or-name)
        ((demap-buffer-minimap minimap-or-name))
        (t (error "No such demap-minimap: %s"
                  (or minimap-or-name
                      (window-normalize-buffer nil) )))))

;;minimap protect

(defun demap--minimap-protected-copy-variables(new-buffer)
  "Copy the current buffers protected variables to NEW-BUFFER.
the protected variables are the ones listed in
`demap-minimap-protected-variables' along with a
few hard coded variables."
  (dolist (v (list 'demap-minimap-change-functions
                   'demap-minimap-kill-hook
                   'demap-minimap-window-set-hook
                   'demap-minimap-window-sleep-hook
                   'demap-minimap-protected-variables
                   'demap--minimap-window
                   'demap-minimap-change-major-mode-hook ))
    (demap--tools-copy-local-variable v nil new-buffer) )
  (demap--tools-dolist-hook (v 'demap-minimap-protected-variables)
    (demap--tools-copy-local-variable v nil new-buffer) ))

(defun demap-minimap-protect-variables(local &rest vars)
  "Add VARS to the list of protected variables.
protected variables are listed in
`demap-minimap-protected-variables'. VARS are not
added if already present.

if LOCAL is non-nil, VARS are added to the
buffer-local value of
`demap-minimap-protected-variables'. t is also
added to signal to use the buffer-local and global
value of `demap-minimap-protected-variables'. this
option is only useful if the current buffer is a
demap minimap buffer."
  (dolist (v vars)
    (add-hook 'demap-minimap-protected-variables v nil local) ))

(defun demap-minimap-unprotect-variables(local &rest vars)
  "Remove VARS from the list of protected variables.
protected variables are listed in
`demap-minimap-protected-variables'. if VARS are
not present then nothing happens.

if LOCAL is non-nil then VARS are removed from the
buffer-local value of
`demap-minimap-protected-variables'."
  (dolist (v vars)
    (remove-hook 'demap-minimap-protected-variables v local) ))

;;minimap buffer

(defun demap-minimap-buffer(minimap-or-name)
  "Return the buffer used by MINIMAP-OR-NAME."
  (demap-minimap--buffer (demap-normalize-minimap minimap-or-name)) )

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not a minimap buffer, then
this returns nil."
  (thread-last
    (window-normalize-buffer buffer-or-name)
    (buffer-local-value 'demap--current-minimap) ))

(defun demap--minimap-changeing-major-mode()
  "Function ran when a minimap is changeing its major mode.
ran in the minimaps buffer."
  (run-hooks 'demap-minimap-change-major-mode-hook) )

(defun demap--minimap-killed()
  "Function ran when a minimap is being killed.
ran in the minimaps buffer."
  (run-hooks 'demap-minimap-kill-hook) )

(defun demap--minimap-buffer-init(minimap)
  "Setup the connection betwean MINIMAP and its buffer."
  (with-current-buffer (demap-minimap-buffer minimap)
    (let ((kill-func   #'demap--minimap-killed)
          (change-func #'demap--minimap-changeing-major-mode)
          (showing     (demap-minimap-showing minimap)) )
      (setq-local demap--current-minimap minimap)
      (add-hook 'kill-buffer-hook       kill-func   nil t)
      (add-hook 'change-major-mode-hook change-func nil t)
      (when showing
        ;;FIXME: might cause problems if the base
        ;;buffer is killed in demap-minimap-change-hook
        (let* ((func  (apply-partially #'demap-minimap-showing-set minimap nil))
               (clean (demap--tools-smart-add-hook-local 'kill-buffer-hook
                                                         func nil showing )))
          (add-hook 'kill-buffer-hook clean nil t) )))))

(defun demap--minimap-buffer-uninit(minimap)
  "Close any connection betwean MINIMAP and its buffer.
undoes effects caused by `demap--minimap-buffer-init'."
  (with-current-buffer (demap-minimap-buffer minimap)
    (let ((kill-func   #'demap--minimap-killed)
          ;;(change-func #'demap--minimap-changeing-major-mode)
          ;;(showing     (demap-minimap-showing minimap))
          )
      (kill-local-variable 'demap--current-minimap)
      (remove-hook 'kill-buffer-hook       kill-func   t)
      ;;removed for optimization reasons:
      ;; (remove-hook 'change-major-mode-hook change-func t)
      ;; (when showing
      ;; (let* ((func  (apply-partially #'demap-minimap-showing-set minimap nil))
      ;;        ;; (clean (demap--tools-smart-add-hook-local 'kill-buffer-hook
      ;;        ;;                                            func nil showing )))
      ;;   (remove-hook 'kill-buffer-hook clean nil t) )
      )))

(defun demap--minimap-buffer-changed(minimap old-buffer)
  "Function called when MINIMAP has changed its buffer from OLD-BUFFER."
  (with-current-buffer old-buffer
    (demap--minimap-protected-copy-variables (demap-minimap-buffer minimap))
    (with-demoted-errors "error in demap-minimap-change-functions: %s"
      (run-hook-with-args 'demap-minimap-change-functions minimap) )))

(defun demap--minimap-buffer-set(minimap new-buffer)
  "Set the buffer used by MINIMAP to NEW-BUFFER.
MINIMAP's old buffer is killed."
  (let ((old-buffer (demap-minimap--buffer minimap)))
    (demap--minimap-buffer-uninit minimap)
    (setf (demap-minimap--buffer minimap) new-buffer)
    (demap--minimap-buffer-init minimap)
    (demap--minimap-buffer-changed minimap old-buffer) ))

;;minimap construct

(defun demap--minimap-constructed(minimap)
  "Function called after MINIMAP has been constructed."
  (with-current-buffer (demap-minimap-buffer minimap)
    (with-demoted-errors "error in demap-minimap-construct-hook: %s"
      (run-hooks 'demap-minimap-construct-hook) )))

(defun demap--minimap-construct-quiet(&optional name showing)
  "Construct a demap minimap.
NAME    is the name of the minimap.
        defaults to `demap-minimap-default-name'.
SHOWING is the buffer that the minimap should show.
        defaults to a blank buffer."
  (let ((minimap (demap--minimap-construct-blank))
        (buffer  (demap--buffer-construct name showing)) )
    (setf (demap-minimap--buffer minimap) buffer)
    (demap--minimap-buffer-init minimap)
    (demap--minimap-constructed minimap)
    minimap ))

;;;###autoload
(defun demap-minimap-construct(&optional name showing)
  "Construct a demap minimap interactively.
NAME    is the name of the minimap.
        defaults to `demap-minimap-default-name'.
SHOWING is the buffer that the minimap should show.
        defaults to a blank buffer."
  (interactive (list (read-string
                      (format "Construct minimap (minimap name) (default %s): "
                              demap-minimap-default-name )
                      nil nil demap-minimap-default-name )
                     nil ))
  (let ((minimap (demap--minimap-construct-quiet name showing)))
    (when (called-interactively-p 'any)
      (message "constructed demap minimap %s" minimap) )
    minimap ))

;;minimap showing

(defun demap-minimap-showing(&optional minimap-or-name)
  "Access the buffer that MINIMAP-OR-NAME is showing.
if MINIMAP-OR-NAME is blank or dead, return nil.
note that a demap minimap can have a blank buffer
without being dead, don't rely on live minimaps
returning a buffer.

this value can be set with setf but it is preferred
to set `demap-minimap-window' instead when appropriate."
  (let ((minimap (demap-normalize-minimap minimap-or-name)))
    (when (demap-minimap-live-p minimap)
      (buffer-base-buffer (demap-minimap--buffer minimap)) )))

(defun demap-minimap-showing-set-unchecked(minimap &optional buffer)
  "Set the buffer that MINIMAP is showing to BUFFER.
Version of (`demap-minimap-showing-set' MINIMAP BUFFER)
but without checking argument types or whether
BUFFER is already being shown.

if BUFFER is nil, then MINIMAP will show a blank
buffer."
  (let ((old-buffer (demap-minimap-buffer minimap)))
    (thread-last
      (demap--buffer-change-showing old-buffer buffer)
      (demap--minimap-buffer-set minimap) )
    (kill-buffer old-buffer) ))

(defun demap-minimap-showing-set(&optional minimap-or-name buffer-or-name)
  "Set the buffer that minimap MINIMAP-OR-NAME is showing to BUFFER-OR-NAME.
if BUFFER-OR-NAME is nil, then MINIMAP-OR-NAME will
show a blank buffer. if BUFFER-OR-NAME is already
being shown, nothing happens.
this is equivalent to
\(setf (`demap-minimap-showing' MINIMAP-OR-NAME) BUFFER-OR-NAME)"
  (let ((minimap (demap-normalize-minimap minimap-or-name))
        (new-show (when-let ((name buffer-or-name))
                    (thread-first
                      (window-normalize-buffer name)
                      (demap--tools-real-buffer) ))))
    (unless (eq (demap-minimap-showing minimap) new-show)
      (demap-minimap-showing-set-unchecked minimap new-show) ))
  buffer-or-name )

(gv-define-setter demap-minimap-showing(buffer-or-name &optional minimap-or-name)
  `(demap-minimap-showing-set ,minimap-or-name ,buffer-or-name) )

;;minimap window

(defun demap--minimap-window-changed(minimap)
  "Function called when MINIMAP changed the window it is showing."
  (with-current-buffer (demap-minimap-buffer minimap)
    (with-demoted-errors "error in demap-minimap-window-set-hook: %s"
      (run-hooks 'demap-minimap-window-set-hook) )))

(defun demap-minimap-window(&optional minimap-or-name)
  "Access the window that MINIMAP-OR-NAME is showing.
MINIMAP-OR-NAME is not guaranteed to be showing the
buffer in this window.

you can use setf to set this value. setting this
value changes what buffer MINIMAP-OR-NAME is
showing to the buffer in window. setting this to
nil will make MINIMAP-OR-NAME show a blank buffer."
  (thread-last
    (demap-minimap-buffer minimap-or-name)
    (buffer-local-value 'demap--minimap-window) ))

(defun demap-minimap-window-set(&optional minimap-or-name window)
  "Set the window MINIMAP-OR-NAME is showing to WINDOW.
setting this value changes what buffer MINIMAP-OR-NAME
is showing to the buffer in window. setting this to
nil will make MINIMAP-OR-NAME show a blank buffer.

this is the same has
\(setf (demap-minimap-window MINIMAP-OR-NAME) WINDOW)."
  (let* ((minimap  (demap-normalize-minimap minimap-or-name))
         (m-buffer (demap-minimap-buffer minimap)))
    (setf (buffer-local-value 'demap--minimap-window m-buffer) window
          (demap-minimap-showing minimap) (window-buffer window) )
    ;;minimaps buffer may have changed
    (demap--minimap-window-changed minimap) ))

(gv-define-setter demap-minimap-window(window &optional minimap-or-name)
  `(demap-minimap-window-set ,minimap-or-name ,window) )

(defun demap-minimap-window-sleep(&optional minimap-or-name)
  "Run hook `demap-minimap-window-sleep-hook' has MINIMAP-OR-NAME."
  (with-current-buffer (demap-minimap-buffer minimap-or-name)
    (with-demoted-errors "error in demap-minimap-window-sleep-hook: %s"
      (run-hooks 'demap-minimap-window-sleep-hook) )))

(defun demap-minimap-window-showing-p(&optional minimap-or-name)
  "Return wether MINIMAP-OR-NAME is showing the buffer in its window."
  (let* ((minimap (demap-normalize-minimap minimap-or-name))
         (window  (demap-minimap-window    minimap)))
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window))
             (demap-minimap-showing minimap) ))))


;;;obsolete

;;minimap window current

(defun demap-current-minimap-window()
  "The window the current demap-minimap is showing.
demap-minimap is not guaranteed to be showing the
buffer in this window.

you can use setf to set this value. setting this
value changes what buffer the current minimap is
showing to the buffer in window. setting this to
nil will make minimap show a blank buffer.

the minimap might rebuild its buffer when this is
set."
  (declare (obsolete demap-minimap-window "Demap version 1.3.0"))
  (demap-minimap-window (demap-buffer-minimap)) )

(defun demap-current-minimap-window-set(window)
  "Set the window the current demap-minimap is showing to WINDOW.
setting this value changes what buffer the current
minimap is showing to the buffer in window. setting
this to nil will make minimap show a blank buffer.

the minimap might rebuild its buffer when this is
set.

this is the same has
\(setf (demap-current-minimap-window) WINDOW)."
  (declare (obsolete demap-minimap-window-set "Demap version 1.3.0"))
  (demap-minimap-window-set (demap-buffer-minimap) window) )

(gv-define-simple-setter demap-current-minimap-window
                         demap-current-minimap-window-set )

(defun demap-current-minimap-window-sleep()
  "Run hook `demap-minimap-window-sleep-hook'."
  (declare (obsolete demap-minimap-window-sleep "Demap version 1.3.0"))
  (demap-minimap-window-sleep (demap-buffer-minimap)) )



(provide 'demap-minimap)
(demap--tools-define-partof-demap)
;;(provide 'demap)
;;; demap-minimap.el ends here
