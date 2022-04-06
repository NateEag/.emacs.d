;;; demap.el --- Detachable minimap package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Created: November 25, 2021
;; Modified: March 22, 2022
;; Version: 1.4.0
;; Keywords: lisp tools convenience
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
;; this package adds a minimap that shows a zoomed out view of the active window's
;; buffer. you can toggle showing the minimap in a side window with
;; `demap-toggle'.
;;
;; this package has a few advantages over other minimap packages.
;; - support for detaching minimaps and having them on a different frame then the
;;      active window.
;; - support for multiple minimap buffers, with there own buffer local definitions
;;      on what buffers it can show and how to show them.
;; - person preference, but having the minimap on the side of the frame rather
;;      then on the side of the active window by default.
;;
;; see the README for more information on the demap package and customization
;; options.
;;
;;; Code:

(eval-and-compile
  (require 'demap-tools)
  (demap--tools-define-demap-start)
  (require 'demap-minimap)
  (require 'cl-lib)
  (require 'demap-modes)
  (require 'subr-x) )

(defcustom demap-minimap-close-kill-minimap-p t
  "Whether `demap-close' can kill minimap buffers.
`demap-close' will only kill the minimap
buffer if it is not in any other window."
  :package-version '(demap . "1.0.0")
  :type  'boolean
  :group 'demap )

(defcustom demap-minimap-window-side 'right
  "The side of the frame `demap-open' opens a window on."
  :package-version '(demap . "1.0.0")
  :type  '(radio (const right)
                 (const left) )
  :group 'demap )

(defcustom demap-minimap-window-width 20
  "The width of the window `demap-open' opens."
  :package-version '(demap . "1.0.0")
  :type  'number
  :group 'demap )

;;;###autoload
(defun demap-open(&optional minimap-or-name frame)
  "Open minimap in a side window.
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
    0        for any visible."
  (interactive)
  (let* ((display-buffer-overriding-action
          `((display-buffer-in-side-window)
            (side          . ,demap-minimap-window-side)
            (window-width  . ,demap-minimap-window-width)
            (preserve-size . (t . nil)) ))
         (window (thread-first
                   (or minimap-or-name
                       (get-buffer demap-minimap-default-name)
                       (demap-minimap-construct) )
                   (demap-normalize-minimap)
                   (demap-minimap-buffer)
                   (display-buffer nil frame) )))
    (set-window-parameter   window 'no-other-window t)
    (set-window-dedicated-p window t)
    (window-preserve-size   window t t) ))

;;;###autoload
(defun demap-close(&optional minimap-or-name frame)
  "Close the side window showing a minimap.
close the side window showing MINIMAP-OR-NAME. has
no effect on normal windows showing
MINIMAP-OR-NAME.

a side window is a window made by
`display-buffer-in-side-window' or `demap-open'.

FRAME specifies what frame to look for side windows
in. it should be:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

could kill MINIMAP-OR-NAME if
`demap-minimap-close-kill-minimap-p' is non-nil.

if a window is removed returns t, otherwise nil."
  (interactive)
  (when-let ((minimap-buffer (ignore-errors
                               (thread-first
                                 (or minimap-or-name
                                     demap-minimap-default-name )
                                 (demap-normalize-minimap)
                                 (demap-minimap--buffer) ))))
    (cl-dolist (window (get-buffer-window-list minimap-buffer nil frame) nil)
      (when (demap--tools-side-window-p window)
        (delete-window window)
        (when (and (thread-first
                     'demap-minimap-close-kill-minimap-p
                     (buffer-local-value minimap-buffer) )
                   (not (get-buffer-window minimap-buffer t)) )
          (kill-buffer minimap-buffer) )
        (cl-return t) ))))

;;;###autoload
(defun demap-toggle(&optional minimap-or-name frame)
  "Toggle side window showing a minimap.
opens MINIMAP-OR-NAME in a side window. if its
already showing, removes it instead.

FRAME specifies what frame to look for side windows
in. it should be:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

see `demap-open' and `demap-close' for more
information."
  (interactive)
  (unless (demap-close minimap-or-name frame)
    (demap-open minimap-or-name frame) ))


(provide 'demap)
;;; demap.el ends here
