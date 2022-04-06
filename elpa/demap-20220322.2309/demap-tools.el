;;; demap-tools.el --- Private tools used by the demap package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Created: January 04, 2022
;; Modified: March 22, 2022
;; Version: 1.4.0
;; Keywords: lisp extensions internal local tools
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
;; this file contains general purpose functions used by demap.el.
;; these functions are made so that at some point, they can be replace with
;; external packages or future implementations of EMACS, and are not specialized
;; to the demap package. everything defined here is still private and should not
;; be loaded or used outside of the demap package.
;;
;;; Code:

(eval-and-compile
  (require 'subr-x)
  (when (>= emacs-major-version 28)
    ;;window.el doesn't provide 'window before version 28
    (require 'window) ))

;;define

(defvar demap--tools-demap-defined-start-p nil
  "Whether demap.el has started loading." )

(defun demap--tools-define-demap-start()
  "Mark that demap.el has started loading.
for use with `demap--tools-define-partof-demap'."
  (setq demap--tools-demap-defined-start-p t) )

(defun demap--tools-define-partof-demap()
  "Define this file has part of demap.
load demap.el if it hasn't start loading yet. meant
to be put at the end of files. this exists so that
if this file is auto-loaded, also load demap.el"
  (unless demap--tools-demap-defined-start-p
    (require 'demap) ))

;;buffer

(defun demap--tools-window-replace-buffer(buffer-or-name
                                          new-buffer-or-name
                                          &optional dedicated)
  "Replace BUFFER-OR-NAME in all window showing it with NEW-BUFFER-OR-NAME.
if the optional third argument DEDICATED is
non-nil, then this function will force the change
and proserve the windows dedicated property."
  (dolist (window (get-buffer-window-list buffer-or-name t t))
    (let ((d (window-dedicated-p window)))
      (if (and d dedicated)
          (progn
            (set-window-dedicated-p window nil)
            (set-window-buffer window new-buffer-or-name t)
            (set-window-dedicated-p window d) )
        (set-window-buffer window new-buffer-or-name t) ))))

(defun demap--tools-buffer-steal-name(buffer-or-name)
  "Rename BUFFER-OR-NAME and return its old name.
BUFFER-OR-NAME's new name is undefined."
  (with-current-buffer buffer-or-name
    (let ((name (buffer-name)))
      (rename-buffer "-old minimap buffer-" t)
      name )))

(defun demap--tools-real-buffer(buffer)
  "Return base buffer of BUFFER.
if Buffer is not an indirect buffer, return BUFFER.
see `buffer-base-buffer'."
  (or (buffer-base-buffer buffer)
      buffer ))

(defun demap--tools-side-window-p(window)
  "Whether WINDOW is a side window."
  (or (window-parameter window 'window-side)
      (when-let (x (window-parent window))
        (window-parameter x 'window-side) )))

;;variables

(defun demap--tools-delete-redundant-keys(key seq)
  "Remove KEY and its value from SEQ, skipping the last one."
  (let ((spot     seq)
        (old-spot nil) )
    (while spot
      (when (eq (car spot) key)
        (when old-spot
          (setf (car old-spot) (nth    2 old-spot)
                (cdr old-spot) (nthcdr 3 old-spot) ))
        (setq old-spot spot) )
      (setq spot (nthcdr 2 spot)) ))
  seq )

(defun demap--tools-copy-local-variable(variable from-buffer to-buffer)
  "Copy the buffer-local value of VARIABLE in FROM-BUFFER to TO-BUFFER.
if VARIABLE is not buffer local in FROM-BUFFER, then
it will no longer be buffer local in TO-BUFFER."
  (setq from-buffer (or from-buffer (current-buffer))
        to-buffer   (or to-buffer   (current-buffer)) )
  (if (local-variable-p variable from-buffer)
      (setf (buffer-local-value variable to-buffer)
            (buffer-local-value variable from-buffer) )
    (with-current-buffer to-buffer
      (kill-local-variable variable) )))

(defun demap--tools-list-p(obj)
  "Determine if OBJ is a list.
if OBJ is a list and not a lambda or nil, return t,
otherwise nil."
  (and obj
       (listp obj)
       (not (functionp obj)) ))

;;dolist

(defmacro demap--tools-dolist-hook(spec &rest body)
  "Loop over all the functions in HOOK.
SPEC

\(fn (VAR HOOK [RESULT]) BODY...)"
  (declare (indent 1))
  `(progn
     (run-hook-wrapped ,(nth 1 spec)
                       (lambda(,(nth 0 spec))
                         ,@body
                         nil ))
     ,(nth 2 spec) ))

(defmacro demap--tools-dolist(spec &rest body)
  "Loop over a list or object SPEC.
Evaluate BODY with VAR bound to each car from LIST,
in turn. Then evaluate RESULT to get return value,
default nil. if LIST is not a list then evaluate
BODY with VAR bound to the value of LIST.
see `dolist'.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1))
  (let ((list-var (make-symbol "list-var")))
    `(let ((,list-var ,(nth 1 spec)))
       (dolist (,(nth 0 spec)
                (if (demap--tools-list-p ,list-var)
                    ,list-var
                  (list ,list-var) )
                ,(nth 2 spec) )
         ,@body ))))

(defmacro demap--tools-dolists-unsafe(specs &rest body)
  "Loop over all SPECS without type checking.
unsafe version of `demap--tools-dolists'.

\(fn ((VAR LIST [STEP])...) BODY...)"
  (declare (indent 1))
  (if specs
      `(demap--tools-dolist ,(car specs)
         (demap--tools-dolists-unsafe ,(cdr specs)
           ,@body ))
    `(progn
       ,@body )))

(defmacro demap--tools-dolists(specs &rest body)
  "Loop over lists or objects in SPECS.
Evaluate BODY with VAR bound to each car from LIST,
in turn. if LIST is not a list then evaluate BODY
with VAR bound to the value of LIST. this process
is stacked for each VAR and LIST given, evaluating
BODY with every combination a LIST elements. STEP
is evaluated each time the end of LIST is reached.
returns the value of STEP in the first spec.
see `dolist'.

\(fn ((VAR LIST [STEP])...) BODY...)"
  (declare (indent 1))
  (unless (listp specs)
    (signal 'wrong-type-argument (list 'consp specs)) )
  (unless (<= 1 (length specs))
    (signal 'wrong-number-of-arguments (list 1 (length specs))) )
  `(demap--tools-dolists-unsafe ,specs
     ,@body ))

;;hooks

(defalias 'demap--tools-add-hook    #'add-hook)

(defun demap--tools-remove-hook(hook function &optional local)
  "Wrapper for `remove-hook'.
passes HOOK FUNCTION and LOCAL to `remove-hook'."
  (remove-hook hook function local))

(defun demap--tools-add-hooks(hooks funcs &optional depth local)
  "Add to the value of HOOKS the functions FUNCS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-dolists ((hook hooks)
                         (func funcs) )
    (demap--tools-add-hook hook func depth local) ))

(defun demap--tools-remove-hooks(hooks funcs &optional local)
  "Remove FUNCS from the value of HOOKS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

if LOCAL is non-nil then HOOKS are buffer local.
see `remove-hook'."
  (demap--tools-dolists ((hook hooks)
                         (func funcs) )
    (demap--tools-remove-hook hook func local) ))

(defun demap--tools-smart-add-hook(hook func &optional depth local)
  "Add to the value of HOOK the function FUNC and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-add-hook hook func depth local)
  (if local
      (thread-first
        #'demap--tools-remove-hook-local
        (apply-partially hook func (current-buffer)) )
    (apply-partially #'demap--tools-remove-hook hook func) ))

(defun demap--tools-smart-add-hooks(hooks funcs &optional depth local)
  "Add to the values of HOOKS the functions FUNCS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-add-hooks hooks funcs depth local)
  (if local
      (thread-first
        #'demap--tools-remove-hooks-local
        (apply-partially hooks funcs (current-buffer)) )
    (apply-partially #'demap--tools-remove-hooks hooks funcs) ))


(defun demap--tools-add-hook-local(hook func &optional depth buffer)
  "Add the function FUNC to the buffer-local value of HOOK as BUFFER.
see `add-hook'."
  (with-current-buffer buffer
    (demap--tools-add-hook hook func depth t) ))

(defun demap--tools-remove-hook-local(hook func &optional buffer)
  "Remove the functions FUNC from the buffer-local values of HOOK as BUFFER.
see `remove-hook'."
  (with-current-buffer buffer
    (demap--tools-remove-hook hook func t) ))

(defun demap--tools-add-hooks-local(hooks funcs &optional depth buffer)
  "Add the functions FUNCS to the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-add-hooks hooks funcs depth t) ))

(defun demap--tools-remove-hooks-local(hooks funcs &optional buffer)
  "Remove the functions FUNCS from the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

see `remove-hook'."
  (with-current-buffer buffer
    (demap--tools-remove-hooks hooks funcs t) ))

(defun demap--tools-smart-add-hook-local(hook func &optional depth buffer)
  "Add FUNC to the buffer-local value of HOOK and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-smart-add-hook hook func depth t) ))

(defun demap--tools-smart-add-hooks-local(hooks funcs &optional depth buffer)
  "Add FUNCS to the buffer-local values of HOOKS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-smart-add-hooks hooks funcs depth t) ))

;;modes

(eval-and-compile
  (defun demap--tools-define-mode-var-get-doc(var
                                              &optional globalp funcp
                                              mode-func mode-pretty-name )
    "Return the documentation that define-miner-mode would give to a mode var.
VAR       is the variable symbol.
GLOBALP   is wether the variable is global or local by default.
FUNCP     is wether the variable should be set by a function.
MODE-FUNC is the function to set VAR (defaults to VAR).
MODE-PRETTY-NAME is the pretty version of the mode's name."
    (let ((doc-g1 "Non-nil if %s is enabled.
See the `%s' command
for a description of this minor mode." )
          (doc-g2 "
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `%s'." )
          (doc-local "Non-nil if %s is enabled.
Use the command `%s' to change this variable." ))
      (setq mode-func        (or mode-func
                                 var )
            mode-pretty-name (or mode-pretty-name
                                 mode-func ));TODO: set to a real pretty name
      (if globalp
          (concat (format doc-g1 mode-pretty-name mode-func)
                  (when funcp (format doc-g2 mode-func)) )
        (format doc-local mode-pretty-name mode-func) ))))

(defmacro demap--tools-define-mode-var(var init-value
                                           &optional globalp funcp doc
                                           &rest args )
  "Define a variable VAR the same way define-miner-mode would.
INIT-VALUE default value.
GLOBALP    is wether the variable is global or local by default.
FUNCP      is wether the variable should be set by a function.
DOC        if not nil, override generated documentation.
ARGS       arguments, see `define-miner-mode'."
  (declare (doc-string 5))
  (if globalp
      (progn
        (setq args (demap--tools-delete-redundant-keys :require args))
        `(defcustom ,var ,init-value
           ,(or doc (demap--tools-define-mode-var-get-doc var t funcp))
           ,@(unless (memq :group args)
               '(:group ',(->> (symbol-name var)
                               (replace-regexp-in-string "-mode\\'" "")
                               (intern) )))
           ,@(unless (memq :set args)
               '(:set #'custom-set-minor-mode) )
           ,@(unless (memq :initialize args)
               '(:initialize 'custom-initialize-default) )
           ,@(unless (memq :type args)
               '(:type 'boolean) )
           ,@args ))
    `(progn
       :autoload-end
       (defvar-local ,var ,init-value
         ,(or doc (demap--tools-define-mode-var-get-doc var)) ))))

;;scroll

(defun demap--tools-scroll-to-region(window start end)
  ""
  (when (>= (window-start window) start)
    (set-window-point window start) )
  (when (<= (window-end window t) end)
    (set-window-point window end) ))

(defun demap--tools-scroll-buffer-to-region(buffer-or-name
                                            start end
                                            &optional minibuf frame )
  ""
  (dolist (window (get-buffer-window-list buffer-or-name minibuf frame))
    (demap--tools-scroll-to-region window start end) ))


(provide 'demap-tools)
;;(provide 'demap)
;;; demap-tools.el ends here
