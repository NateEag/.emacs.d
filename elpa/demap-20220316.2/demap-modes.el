;;; demap-modes.el --- Modes for demap minimaps -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Created: January 04, 2022
;; Modified: March 9, 2022
;; Version: 1.2.0
;; Keywords: lisp convenience
;; Homepage: https://gitlab.com/sawyerjgardner/demap.el
;; Package-Requires: ((emacs "24.4") (dash "2.18.0"))
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
;; Addon for demap.el that adds modes meant to run in demap-minimap buffers.
;; minimap modes (defined with `demap-define-minimap-miner-mode') are modes that
;; can only be activated in a minimap. the minimap mode `demap-track-window-mode'
;; updates the window that the minimap is showing while
;; `demap-current-line-mode' and `demap-visible-region-mode' display information
;; on the shown window. if you wish to make a minimap mode, use these as
;; examples.
;;
;; code layout:
;;      define-minimap-miner-mode       ; helper macro for making modes for minimaps
;;      track-window-mode               ; minimap mode to fallow the active window
;;              update                  ; update track-window-mode
;;      current-line-mode               ; minimap mode to highlight the current line
;;              update                  ; update current-line-mode's overlay
;;              wake                    ; wake up current-line-mode
;;      visible-region-mode             ; minimap mode to highlight the window's area
;;              update                  ; update visible-region-mode's overlay
;;              wake                    ; wake up visible-region-mode
;;
;;; Code:

(require 'demap-tools)
(require 'demap-minimap)
(require 'dash)
(require 'cl-lib)
(require 'hl-line)


;;;define-minimap-miner-mode

;;;###autoload
(defmacro demap-define-minimap-miner-mode(mode doc &rest body)
  "Define miner mode for demap minimap buffers.
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

\(fn MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)"
  (declare (doc-string 2)
           (indent     1)
           (debug (&define name string-or-null-p
                   [&optional [&not keywordp] sexp
                    &optional [&not keywordp] sexp
                    &optional [&not keywordp] sexp ]
                   [&rest [keywordp sexp]]
                   def-body )))
  (let (globalp
        init-value
        lighter
        keymap
        (construct-variable t)
        (getter    mode)
        (setter    `(setf ,mode))
        func
        after-hook
        (rest      '())

        (protect   '())
        init-func
        kill-func
        set-func )
    ;;optional args
    (and
     (unless (keywordp (car body))
       (setq init-value (pop body)) )
     (unless (keywordp (car body))
       (setq lighter    (pop body)) )
     (unless (keywordp (car body))
       (setq keymap     (pop body)) ))
    ;;process keys
    (while (keywordp (car body))
      (let ((key (pop body))
            (val (pop body)) )
        (pcase key
          (:global     (setq globalp    val));TODO: make this more accurate
          (:init-value (setq init-value val))
          (:lighter    (setq lighter    val))
          (:keymap     (setq keymap     val))
          (:after-hook (setq after-hook val))
          (:variable
           (setq construct-variable nil)
           (let ((cdr-val (cdr-safe val)))
             (if (and cdr-val
                      (or (symbolp cdr-val)
                          (functionp cdr-val) ))
                 (setq getter (car val)
                       setter `(funcall #',cdr-val) )
               (setq getter val
                     setter `(setf ,val) ))))
          ;;new
          (:init-func  (setq init-func  val))
          (:kill-func  (setq kill-func  val))
          (:set-func   (setq set-func   val))
          (:protect
           (if (symbolp val)
               (push `',val protect)
             (--> (mapcar (lambda(x) `',x) val)
                  (append it protect)
                  (setq protect it) )))
          ;;rest
          (_
           (--> (list val key)
                (append it rest)
                (setq rest it) )))))
    (setq rest (nreverse rest))
    (when (and construct-variable (not globalp))
      (push `',mode protect) )
    (when (and globalp construct-variable)
      (->> `(progn
              (when (called-interactively-p 'any)
                (customize-mark-as-set ',mode) )
              ,@(when after-hook
                  `(,after-hook) ))
           (setq after-hook) ))
    (--> (or set-func
             (let ((state-var (make-symbol "-state")))
               `(lambda(,state-var)
                  (if ,state-var
                      ,(or init-func `(,@setter t  ))
                    ,(or kill-func `(,@setter nil)) ))))
         (if (symbolp it)
             `(,it)
           `(funcall ,it) )
         (setq set-func it) )
    (->> (let ((state-var (make-symbol "-state"))
               (error-msg "%s can only be used in a demap-minimap buffer")
               (kill-hook 'demap-minimap-kill-hook)
               (chng-hook 'demap-minimap-change-major-mode-hook)
               (kill-func `(lambda()
                             (,mode 0) ))
               (chng-func `(lambda()
                             (unless (get ',mode 'permanent-local)
                               (,mode 0) ))))
           `(lambda(,state-var)
              (cl-assert (demap-buffer-minimap) nil ,error-msg ',mode)
              (when (xor ,state-var ,getter)
                (,@set-func ,state-var)
                (unless (xor ,state-var ,getter)
                  (if ,state-var
                      (progn
                        (demap-minimap-protect-variables t ,@protect)
                        (add-hook ',kill-hook #',kill-func nil t)
                        (add-hook ',chng-hook #',chng-func nil t) )
                    (demap-minimap-unprotect-variables t ,@protect)
                    (remove-hook ',kill-hook #',kill-func t)
                    (remove-hook ',chng-hook #',chng-func t) )))))
         (setq func) )
    `(progn
       ,@(when construct-variable
           `((demap--tools-define-mode-var ,mode ,init-value
                                           ,globalp ,(and body t) nil
                                           ,@rest )))
       (define-minor-mode ,mode
         ,doc
         ,init-value
         ,lighter
         ,keymap
         ,@(when after-hook
             `(:after-hook ,after-hook) )
         :global ,globalp
         :variable (,getter . ,func)
         ,@rest
         ,@body ))))

;;;track-window-mode

(defcustom demap-track-window-mode-update-p-func
  #'demap-track-window-mode-update-p-func-default
  "Function to determine if demap-minimap should show the selected window.
the function should accept no arguments. it should
return nil if the current minimap should not show the
selected window."
  :package-version '(demap . "1.0.0")
  :type 'function
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-track-window-mode
  "Minimap miner mode to make minimap show the active window.
makes the minimap this is active in show the buffer
in the currently active window. will not show the
window if `demap-track-window-mode-update-p-func'
returns nil.

this mode can only be used in a demap minimap buffer."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :init-func (progn
               (setf demap-track-window-mode t)
               (->> (demap-buffer-minimap)
                    (apply-partially #'demap-track-window-mode-update-as)
                    (add-hook 'window-state-change-hook) ))
  :kill-func (progn
               (->> (demap-buffer-minimap)
                    (apply-partially #'demap-track-window-mode-update-as)
                    (remove-hook 'window-state-change-hook) )
               (kill-local-variable 'demap-track-window-mode) ))

;;track-window-mode update

(defun demap-track-window-mode-update-p-func-default()
  "Determine if track-w mode can fallow the active window.
default value for `demap-track-window-mode-update-p-func'.
returns nil if the active window's buffer is not a
file buffer."
  (buffer-file-name (window-buffer)) )

(defun demap-track-window-mode-update-p-func-any()
  "Determine if track-w mode can fallow the active window.
meant to be a value for `demap-track-window-mode-update-p-func'.
returns true for nearly any window, ignoring other minimaps"
  (not (demap-buffer-minimap)) )

(defun demap--track-window-mode-update()
  "Update the window the current minimap is showing.
if the current minimap should not be showing the
active window then tell minimap to sleep."
  (if (funcall demap-track-window-mode-update-p-func)
      (setf (demap-current-minimap-window) (selected-window))
    (demap-current-minimap-window-sleep) ))

(defun demap-track-window-mode-update-as(minimap)
  "Update the window that MINIMAP is showing.
if the MINIMAP should not be showing the active
window then tell it to sleep."
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--track-window-mode-update) ))


;;;current-line-mode

(defface demap-current-line-face
  '((t (:inherit hl-line
        :extend  t )))
  "Face used to highlight the current line in demap-minimap."
  :package-version '(demap . "1.0.0")
  :group 'demap )

(defface demap-current-line-inactive-face
  '((t (:inherit demap-current-line-face
        :extend  t )))
  "Face used to highlight the current line in demap-minimap when not active."
  :package-version '(demap . "1.0.0")
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-current-line-mode
  "Minimap miner mode to highlight the current line.
this will use `demap-current-line-face' to
highlight the line, or
`demap-current-line-inactive-face' when the window
the current minimap is showing is not active.

this mode can only be used in a demap minimap buffer."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :init-func (progn
               (setq demap-current-line-mode (make-overlay 0 0))
               (-as-> #'demap--current-line-mode-wake-if func
                      (add-hook 'demap-minimap-window-set-hook   func nil t) )
               (-as-> #'demap--current-line-mode-sleep func
                      (add-hook 'demap-minimap-window-sleep-hook func nil t) ))
  :kill-func (progn
               (demap--current-line-mode-sleep)
               (delete-overlay demap-current-line-mode)
               (kill-local-variable 'demap-current-line-mode)
               (-as-> #'demap--current-line-mode-wake-if func
                      (remove-hook 'demap-minimap-window-set-hook   func t) )
               (-as-> #'demap--current-line-mode-sleep func
                      (remove-hook 'demap-minimap-window-sleep-hook func t) )))

;;current-line-mode update

(defun demap--current-line-mode-update()
  "Update the position of current-line mode's overlay."
  (let ((w-buffer (window-buffer (demap-current-minimap-window)))
        (m-buffer (current-buffer))
        (showing  (demap-minimap-showing (demap-buffer-minimap)))
        (ov       demap-current-line-mode) )
    (when (eq showing (demap--tools-real-buffer w-buffer))
      (with-current-buffer w-buffer
        (move-overlay ov
                      (line-beginning-position)
                      (+ (line-end-position) 1)
                      m-buffer )))))

(defun demap--current-line-mode-update-has(minimap)
  "Update the position of current-line mode's overlay in MINIMAP."
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--current-line-mode-update) ))

;;current-line-mode wake

(defun demap--current-line-mode-wake()
  "Wake up `demap-current-line-mode'."
  (overlay-put demap-current-line-mode 'face 'demap-current-line-face)
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--current-line-mode-update-has)
       (add-hook 'post-command-hook) )
  (demap--current-line-mode-update) )

(defun demap--current-line-mode-sleep()
  "Set `demap-current-line-mode' to sleep."
  (overlay-put demap-current-line-mode 'face 'demap-current-line-inactive-face)
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--current-line-mode-update-has)
       (remove-hook 'post-command-hook) ))

(defun demap--current-line-mode-wake-if()
  "Set `demap-current-line-mode' awake if minimap is showing a window."
  (if (demap-current-minimap-window)
      (demap--current-line-mode-wake)
    (demap--current-line-mode-sleep) ))


;;;visible-region-mode

(defface demap-visible-region-face
  '((t (:inherit region
        :extend  t )))
  "Face used to highlight the visible region in demap-minimap."
  :package-version '(demap . "1.0.0")
  :group 'demap )

(defface demap-visible-region-inactive-face
  '((t (:inherit demap-visible-region-face
        :extend  t )))
  "Face used to highlight the visible region in demap-minimap when not active."
  :package-version '(demap . "1.0.0")
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-visible-region-mode
  "minimap miner mode to show the visible region in minimaps window.
this highlights the area in the minimap visible
from the window it is showing. when the window
shown is active, the face
`demap-visible-region-face' is used, otherwise
`demap-visible-region-inactive-face' is used.

this mode can only be used in a demap minimap buffer."
  :package-version '(demap . "1.0.0")
  :group 'demap
  :init-func (progn
               (setq demap-visible-region-mode (make-overlay 0 0))
               (-as-> #'demap--visible-region-mode-wake-if func
                      (add-hook 'demap-minimap-window-set-hook   func nil t) )
               (-as-> #'demap--visible-region-mode-rest func
                      (add-hook 'demap-minimap-window-sleep-hook func nil t) ))
  :kill-func (progn
               (demap--visible-region-mode-sleep)
               (delete-overlay demap-visible-region-mode)
               (kill-local-variable 'demap-visible-region-mode)
               (-as-> #'demap--visible-region-mode-wake-if func
                      (remove-hook 'demap-minimap-window-set-hook   func t) )
               (-as-> #'demap--visible-region-mode-rest func
                      (remove-hook 'demap-minimap-window-sleep-hook func t) )))

;;visible-region-mode update

(defun demap--visible-region-made-active-p()
  "Determine if `demap-visible-region-mode' should be active or not.
returns true if the current minimap is showing the active window."
  (let ((window  (demap-current-minimap-window)))
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window))
             (demap-minimap-showing) ))))

;;TODO: re-render minimap when changing on a different frame
(defun demap--visible-region-mode-update()
  "Update the position of `demap-visible-region-mode''s overlay.
minimap will scroll if overlay goes off screen."
  (if (demap--visible-region-made-active-p)
      (let* ((window (demap-current-minimap-window))
             (ov-start (window-start window))
             (ov-end   (window-end window t)) )
        (move-overlay demap-visible-region-mode
                      ov-start
                      ov-end
                      (current-buffer) )
        (dolist (w (get-buffer-window-list (current-buffer) nil t))
          (when (>= (window-start w) ov-start)
            (set-window-point w ov-start) )
          (when (<= (window-end w t) ov-end)
            (set-window-point w ov-end) )))
    (demap--visible-region-mode-sleep) ));TODO: see if this line can be removed

(defun demap--visible-region-mode-update-has(minimap &rest ignored)
  "Update the position of `demap-visible-region-mode''s overlay in MINIMAP.
IGNORED is ignored for function hooks."
  (ignore ignored)
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--visible-region-mode-update) ))

(defun demap--visible-region-mode-update-window-as(minimap window &rest ignored)
  "Update the position of `demap-visible-region-mode''s overlay in MINIMAP.
if MINIMAP is not showing WINDOW then nothing happens.
IGNORED is ignored for function hooks."
  (ignore ignored)
  (when (eq window (demap-minimap-window minimap))
    (with-current-buffer (demap-minimap-buffer minimap)
      (demap--visible-region-mode-update) )))

;;visible-region-mode wake

(defun demap--visible-region-mode-wake()
  "Wake up `demap-visible-region-mode'.
set face and add hooks to update overlay."
  (overlay-put demap-visible-region-mode 'face 'demap-visible-region-face)
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--visible-region-mode-update-window-as)
       (add-hook 'window-scroll-functions) )
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--visible-region-mode-update-has)
       (add-hook 'window-size-change-functions) )
  (demap--visible-region-mode-update) )

(defun demap--visible-region-mode-sleep()
  "Put `demap-visible-region-mode' to sleep.
set face and remove hooks that update overlay."
  (->> 'demap-visible-region-inactive-face
       (overlay-put demap-visible-region-mode 'face) )
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--visible-region-mode-update-window-as)
       (remove-hook 'window-scroll-functions) )
  (->> (demap-buffer-minimap)
       (apply-partially #'demap--visible-region-mode-update-has)
       (remove-hook 'window-size-change-functions) ))

(defun demap--visible-region-mode-wake-if()
  "Set `demap-visible-region-mode' awake if minimap is showing a window."
  (if (demap-current-minimap-window)
      (demap--visible-region-mode-wake)
    (demap--visible-region-mode-sleep) ))

(defun demap--visible-region-mode-rest()
  "Change `demap-visible-region-mode' overlay's face to reflect minimap state."
  (->> 'demap-visible-region-inactive-face
       (overlay-put demap-visible-region-mode 'face) ))


(provide 'demap-modes)
(demap--tools-define-partof-demap)
;;(provide 'demap)
;;; demap-modes.el ends here
