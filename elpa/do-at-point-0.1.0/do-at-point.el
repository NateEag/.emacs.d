;;; do-at-point.el --- Generic context-sensitive action dispatcher.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; URL: https://git.sr.ht/~pkal/do-at-point
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The command `do-at-point' is a generalised `find-file-at-point',
;; both in the sense that it can understand more than just files, and
;; do more than just open a file.  Depending on the "thing" at point,
;; different "actions" can be dispatched, e.g. opening a url using
;; `browse-url' or occurring a symbol at point.

;; The entry point of this package is `do-at-point'.  Bind it to a
;; convenient key:
;;
;;   (global-set-key (kbd "C-'") #'do-at-point)
;;
;; Most of the behaviour is controlled via the user option
;; `do-at-point-actions' and `do-at-point-user-actions'.  A mode may
;; use `do-at-point-local-actions' to add additional things and/or
;; actions.

;;; Code:

(eval-when-compile (require 'pcase))
(require 'seq)
(require 'thingatpt)

(defgroup do-at-point '()
  "Generic context-sensitive action dispatcher."
  :group 'convenience)

(defconst do-at-point--actions-type
  '(alist :value-type
	  (alist :value-type
		 (choice
		  (const :tag "Inherit" nil)
		  (list :tag "Action"
			(string :tag "Description") function))
		 :key-type character)
	  :key-type symbol)
  "User option type for `do-at-point' actions.")

(defcustom do-at-point-user-actions '()
  "Custom association of things and their respective actions.
Refer to the user option `do-at-point-actions' for details on the
structure of the values of this user option."
  :type do-at-point--actions-type)

(defvar-local do-at-point-local-actions '()
  "Actions that can be added by a major or minor mode.
These are prioritised to the user option `do-at-point-actions',
but not `do-at-point-user-actions'.  Refer to the user option
`do-at-point-actions' for details on the structure of the values
of this variable.")

(defcustom do-at-point-actions
  `((region
     (?\s "Mark" ,(lambda (start end)
		    (set-mark start)
		    (goto-char end)))
     (?\C-i "Indent" ,#'indent-region)
     (?s "Isearch"
	 ,(lambda (str)
	    (isearch-mode t)
	    (isearch-yank-string str)))
     (?o "Occur" ,(lambda (str) (occur (regexp-quote str))))
     (?w "Kill-Save" ,#'kill-new)
     (?W "Write region"
	 ,(lambda (beg end)
	    (let ((file (read-file-name "File: ")))
	      (write-region beg end file))))
     (?k "Kill" ,#'kill-region)
     (?n "Narrow" ,#'narrow-to-region)
     (?$ "Spell check" ,#'ispell-region)
     (?| "Pipe command"
	 ,(lambda (beg end)
	    (let ((cmd (read-shell-command "Command: ")))
	      (shell-command-on-region beg end cmd nil t))))
     (?! "Shell command" ,(if (fboundp 'shell-command+)
			      #'shell-command+
			    (lambda (cmd) (shell-command cmd)))))
    (email
     (?m "Compose message" ,(lambda (to) (compose-mail to))))
    (existing-filename
     (?f "Find file" ,#'find-file)
     (?4 "Find file other window" ,#'find-file-other-window))
    (url
     (?b "Browse" ,#'browse-url)
     (?d "Download" ,#'(lambda (url)
			 (start-process "*Download*" nil "wget" "-q" "-c" url)))
     (?e "eww" ,#'eww-browse-url))
    (number
     (?* "Calc" ,(lambda () (calc-embedded '(t)))))
    (word
     (?$ "Spell check" ,(lambda () (ispell-word)))
     (?d "Dictionary" ,#'dictionary-search))
    (symbol
     (?. "Xref" ,#'xref-find-definitions)
     (?o "Occur" ,(lambda (str)
		    (occur (concat "\\_<\\(" (regexp-quote str) "\\)\\_>")))))
    (string) (sexp) (line) (paragraph (?$))
    (defun
	(?e "Evaluate" ,(lambda () (eval-defun nil)))))
  "Association of things and their respective actions.
Each element of the list has the form (THING . ACTIONS), where
THING is a symbol as interpreted by `thing-at-point' and ACTIONS
have the form (KEY NAME FUNC), where KEY is a dispatch character,
NAME is a brief description of the action and FUNC is a function
that will be dispatched when KEY is selected.  FUNC can take
zero, one or two arguments, which `do-at-point' will respectively
interpret as function that is invoked without any arguments, or
with a buffer substring or the bounds of THING.  Actions listed
under the \"thing\" `region' are shared among all \"things\".  An
entry in ACTIONS can omit NAME and FUNC, and it will instead
fallback into the entry for `region'.  This is why a an entry
does not require any actions to be associated with it, if it just
serves as a specific kind of region worth selecting.  The order
of element in the list correspond to the order in which
`do-at-point' will prompt the user for possible things at point.
Note that the user option `do-at-point-user-actions' and the
variable `do-at-point-local-actions' take precedence over this
user option."
  :type do-at-point--actions-type)

(defcustom do-at-point-quick-bindings t
  "Non-nil means that quick bindings are enabled.
Quick bindings allow for the user to operate on a selection
without having to have confirmed the first."
  :type 'boolean)

(defcustom do-at-point-selection-face 'highlight
  "Face to use to highlight the selected thing."
  :type 'face)

(defvar do-at-point--quick-map (make-sparse-keymap))

(defun do-at-point--actions (thing)
  "Return possible actions for THING.
The function consults `do-at-point-user-actions',
`do-at-point-local-actions' and the user option
`do-at-point-actions' in this order and inherits actions from
more to less specific entries."
  (seq-reduce
   (lambda (accum ent)
     (let ((prev (assq (car ent) accum)))
       (cons (list (car ent)
		   (or (cadr ent) (cadr prev))
		   (or (caddr ent) (caddr prev))
		   (or (cadddr ent) (cadddr prev)))
	     (delq prev accum))))
   (reverse (append
	     (alist-get thing do-at-point-user-actions)
	     (alist-get 'region do-at-point-user-actions)
	     (alist-get thing do-at-point-local-actions)
	     (alist-get 'region do-at-point-local-actions)
	     (alist-get thing do-at-point-actions)
	     (alist-get 'region do-at-point-actions)))
   '()))

(defvar-local do-at-point--overlay nil
  "Buffer-local overlay object to display the selection overlay.
The overlay is also used to store properties like the current
thing being selected and the key used to invoke `do-at-point'.")

(defun do-at-point--update ()
  "Ensure a consistent state for the \"thing\" at point.
This means updating and moving the selection overlay and ensuring
that the repeat key, i.e. the key which was used to initially
invoke `do-at-point' is bound transiently."
  (let ((thing (or (overlay-get do-at-point--overlay 'do-at-point-thing)
		   (do-at-point--next-thing t))))
    (let ((bound (bounds-of-thing-at-point thing)))
      (if bound
	  (move-overlay do-at-point--overlay (car bound) (cdr bound))
	(delete-overlay do-at-point--overlay)))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector (overlay-get do-at-point--overlay 'do-at-point-key))
		   #'do-at-point--next-thing)
       map))))

(defun do-at-point-confirm (&optional quick)
  "Dispatch an action on the current \"thing\" being selected.
If the optional argument QUICK is non-nil, the first applicable
action is selected."
  (interactive)
  (unless (and do-at-point--overlay
	       (overlay-start do-at-point--overlay)
	       (overlay-end do-at-point--overlay))
    (user-error "No selected thing"))
  (let* ((thing (overlay-get do-at-point--overlay 'do-at-point-thing))
	 (options (do-at-point--actions thing))
	 (choice (cond
		  (quick (car options))
		  ((assq last-command-event options))
		  ((read-multiple-choice
		    (format "Action on %s" thing)
		    (mapcar
		     (pcase-lambda (`(,key ,short ,_func ,long))
		       (list key short long))
		     options)))))
	 (func (cadr (alist-get (car choice) options)))
	 (bound (cons (overlay-start do-at-point--overlay)
		      (overlay-end do-at-point--overlay))))
    (do-at-point--mode -1)
    (message nil)		;clear mini buffer
    (pcase (car (func-arity func))
      (0 (funcall func))
      (1 (funcall func (buffer-substring (car bound) (cdr bound))))
      (2 (funcall func (car bound) (cdr bound)))
      (_ (error "Unsupported signature: %S" func)))))

;; We add an alias for to avoid confusing `substitute-key-definition'
;; later on.
(defalias 'do-at-point-confirm* #'do-at-point-confirm)

(defun do-at-point-confirm-quick ()
  "Quickly select the first action for the selected \"thing\".
See the function `do-at-point-confirm' for more details."
  (interactive)
  (do-at-point-confirm t))

(defun do-at-point-quit ()
  "Quit the selection mode and defer to \\[keyboard-quit]."
  (interactive)
  (do-at-point--mode -1)
  (keyboard-quit))

(defun do-at-point--applicable-things ()
  "Return a list of things that are applicable at point."
  (let ((actions (append do-at-point-user-actions
			 do-at-point-local-actions
			 do-at-point-actions)))
    (seq-filter #'thing-at-point (mapcar #'car actions))))

(defun do-at-point--next-thing (&optional no-update)
  "Select the next possible \"thing\".
If NO-UPDATE is nil, then the selection overlay is also updated.
Otherwise the next \"thing\" is just determined.  The return
value of the function is always the new \"thing\"."
  (interactive)
  (let* ((things (do-at-point--applicable-things))
	 (thing (overlay-get do-at-point--overlay 'do-at-point-thing)))
    (setq thing (or (cadr (memq thing things)) (car things)))
    (prog1 (overlay-put do-at-point--overlay 'do-at-point-thing thing)
      ;; clear and reinitialise the shortcut map
      (setcdr do-at-point--quick-map nil)
      (when do-at-point-quick-bindings
	(dolist (key (mapcar #'car (do-at-point--actions thing)))
	  (define-key do-at-point--quick-map (vector key) #'do-at-point-confirm))
	(define-key do-at-point--quick-map (kbd "<return>")
		    #'do-at-point-confirm-quick))
      (let ((default (cadar (do-at-point--actions thing))))
	(message
	 (substitute-command-keys
	  "Act on `%s' (%s by default), select using \\[do-at-point-confirm*].")
	 thing default))
      (unless no-update
	(do-at-point--update)))))

(defun do-at-point--lighter ()
  "Determine the lighter for `do-at-point--mode'.
The lighter depends on the current \"thing\" being selected."
  (let ((thing (overlay-get do-at-point--overlay 'do-at-point-thing)))
    (and thing (format " Do-At-Point/%s" thing))))

(defvar do-at-point--mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map do-at-point--quick-map)
    (define-key map (kbd "C-<return>") #'do-at-point-confirm*)
    (define-key map [remap keyboard-quit] #'do-at-point-quit)
    (define-key map (kbd "M-n") #'do-at-point-forward)
    (define-key map (kbd "M-p") #'do-at-point-backward)
    map))

(define-minor-mode do-at-point--mode
  "Minor mode that implements the selection for `do-at-point'.
This is an internal implementation detail and shouldn't be
invoked or bound directly.  Use the command `do-at-point'
instead."
  :lighter ((:eval (do-at-point--lighter)))
  :interactive nil
  (if do-at-point--mode
      (let ((ov (let ((ov (make-overlay 0 0)))
		  (overlay-put ov 'face do-at-point-selection-face)
		  (delete-overlay ov)
		  ov)))
	(overlay-put ov 'do-at-point-key last-command-event)
	(add-hook 'post-command-hook #'do-at-point--update 90 t)
	(setq do-at-point--overlay ov)
	(do-at-point--update))
    (remove-hook 'post-command-hook #'do-at-point--update t)
    (overlay-put do-at-point--overlay 'do-at-point-thing nil)
    (delete-overlay do-at-point--overlay)
    (setq do-at-point--overlay nil)))

(defun do-at-point-forward (n)
  "Move focus N things ahead.
By default, this will move one thing ahead."
  (interactive "p")
  (forward-thing (overlay-get do-at-point--overlay 'do-at-point-thing) n))

(defun do-at-point-backward (n)
  "Move focus N things back.
Refer to the command `do-at-point-forward' for more details."
  (interactive "p")
  (do-at-point-forward (- (or n 1))))

;;;###autoload
(defun do-at-point (&optional thing)
  "Focus on a THING at point.
If invoked interactively with a prefix argument or with the
optional argument THING, one can set the initial thing to be
selected."
  (interactive
   (let ((things (do-at-point--applicable-things)))
     (if (null current-prefix-arg) '()
       (unless things
	 (user-error "Nothing applicable at point to choose"))
       (list (intern (completing-read "Thing: " things nil t))))))
  (when thing
    (setq do-at-point--overlay (copy-overlay do-at-point--overlay))
    (overlay-put do-at-point--overlay 'do-at-point-thing thing))
  (when do-at-point--mode
    (do-at-point--mode -1))
  (do-at-point--mode 1))

(provide 'do-at-point)
;;; do-at-point.el ends here
