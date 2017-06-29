;;; svn-msg.el --- major mode for Subversion commit messages

;; Copyright (C) 2005-2008 Austin Clements

;; Authors:    Austin Clements (amdragon@mit.edu)
;; Maintainer: Austin Clements (amdragon@mit.edu)
;; Created:    09-Jul-2005
;; Version:    0.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;; For installation instructions, see svn-msg-load.el.

;; To do
;; * Make whitespace trimming a customizable option
;; * If svnci is going to be used from within a long-running emacs,
;;   this needs to clean up its buffer better and play more nicely
;;   with window layout
;; * When a file is added with history ("A  +"), visit the diff
;;   instead of the file

;;; Code:

(eval-when-compile (require 'cl))

(defgroup svn-msg-mode nil
  "Major mode for editing Subversion commit messages.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;

(defconst svn-msg-file-face 'svn-msg-file-face)
(defface svn-msg-file-face
  '((((class color) (background dark))
     (:weight bold))
    (t (:inherit default)))
  "Face used for files."
  :group 'svn-msg-mode)

(defconst svn-msg-added-face 'svn-msg-added-face)
(defface svn-msg-added-face
  '((((class color))
     (:foreground "green"))
    (t (:inherit svn-msg-file-face)))
  "Face used for added files."
  :group 'svn-msg-mode)

(defconst svn-msg-modified-face 'svn-msg-modified-face)
(defface svn-msg-modified-face
  '((((class color))
     (:foreground "yellow"))
    (t (:inherit svn-msg-file-face)))
  "Face used for modified files."
  :group 'svn-msg-mode)

(defconst svn-msg-deleted-face 'svn-msg-deleted-face)
(defface svn-msg-deleted-face
  '((((class color))
     (:foreground "red"))
    (t (:inherit svn-msg-file-face)))
  "Face used for removed files."
  :group 'svn-msg-mode)

(defconst svn-msg-conflict-face 'svn-msg-conflict-face)
(defface svn-msg-conflict-face
  '((((class color))
     (:background "red"))
    (t (:inherit svn-msg-file-face)))
  "Face used for conflict files."
  :group 'svn-msg-mode)

(defconst svn-msg-path-face 'svn-msg-path-face)
(defface svn-msg-path-face
  '((((supports :underline t)) :underline t)
    (t (:foreground "lightblue")))
  "Face used to highlight paths in the commit message.

This is layered on top of the added/modified/etc face appropriate
for a given status line."
  :group 'svn-msg-mode)

(defconst svn-msg-info-face 'svn-msg-info-face)
(defface svn-msg-info-face
  '((((class color) (min-colors 9))
     (:inherit font-lock-comment-face))
    ;; font-lock-comment-face is completely unhelpful in low color
    ;; situations
    (((class color) (min-colors 8))
     (:foreground "red"))
    (t (:inherit font-lock-comment-face)))
  "Face used for the ignored line in the commit message."
  :group 'svn-msg-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;

(defcustom svn-msg-mode-hook nil
  "Normal hook run when entering svn commit mode."
  :type 'hook
  :group 'svn-msg-mode)

(defcustom svn-msg-offer-to-restore t
  "Offer to restore the contents of failed commit messages.

When a commit message is loaded, check if an old commit message was
left over by a earlier failed commit and offer to restore its contents
into this commit message."
  :type 'boolean
  :group 'svn-msg-mode)

(defcustom svn-msg-show-old-message t
  "Display the old commit message when offering to restore it.

While offering to restore an old commit message, split the window
and display the old commit message."
  :type 'boolean
  :group 'svn-msg-mode)

(defcustom svn-msg-delete-old-message t
  "Delete the old commit message on save.

If an old commit message was restored, offer to delete the old
commit message file when this commit message is saved.  If 'auto,
then don't bother prompting."
  :type '(choice
          (const :tag "Don't delete" nil)
          (const :tag "Offer to delete" t)
          (const :tag "Automatically delete" auto))
  :group 'svn-msg-mode)

(defcustom svn-msg-diff-args ()
  "Additional arguments to pass to svn diff."
  :type '(repeat string)
  :group 'svn-msg-mode)

(defcustom svn-msg-diff-mode 'diff-mode
  "Major mode to use when viewing the results of svn diff."
  :type 'function
  :group 'svn-msg-mode)

(defcustom svn-msg-diff-height 3
  "Height of the commit message window when viewing a diff."
  :type 'integer
  :group 'svn-msg-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status lines and font lock
;;

(defconst svn-msg-info-regexp
  "^--This line, and those below, will be ignored--\n"
  "The regexp to match at the beginning of the svn commit message's
ignore block.")

(defvar svn-msg-stat-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'svn-msg-mouse-visit-diff)
    (define-key map "\r" #'svn-msg-visit-diff)
    map)
  "Keymap applied to status lines in the commit message's ignore
block.  By default, these bindings visit diffs when status lines
are selected.")

(defvar svn-msg-stat-line nil
  "A variable whose properties are used as the default properties
of status lines in the commit message's ignore block.")
(put 'svn-msg-stat-line 'mouse-face 'highlight)
(put 'svn-msg-stat-line 'help-echo
     (lambda (w o p)
       (let ((f (get-char-property p 'svn-msg-msg o)))
         (and f (funcall f o p)))))
(put 'svn-msg-stat-line 'follow-link t)
(when (eq window-system 'x)
  (put 'svn-msg-stat-line 'pointer x-sensitive-text-pointer-shape))

(defvar svn-msg-status-faces
  '(("\\(?:C[ CM]\\|[ ADIMRX?!~]C\\)" svn-msg-conflict-face)
    ("[AR][ M]" svn-msg-added-face)
    ("D[ M]" svn-msg-deleted-face)
    ("\\(?:M[ M]\\| M\\)" svn-msg-modified-face)
    ("[!~][ M]" svn-msg-conflict-face)
    ("[IX?][ M]" default))
  "An association list mapping regular expressions to faces.
Each regular expression should match against the first two
characters of a status line in the svn commit message (or,
equivalently, the first two characters in the output of svn
status) and must not contain any binding groups.  When
`svn-msg-mode' is entered, it will compute and append keywords to
a buffer-local copy of `svn-msg-font-lock-keywords' to reflect
these faces.")

(defvar svn-msg-font-lock-keywords
  `((,svn-msg-info-regexp . svn-msg-info-face))
  "Font lock keywords for `svn-msg-mode'.  Note that this will
be made buffer-local and appended to when entering svn-msg-mode
in order to enable hyperlinks and status line highlighting
according to `svn-msg-status-faces'.")

(defvar svn-msg-stat-line-regexp "\\([ ACDIMRX?!~]\\)[ CM][ L][ +] \\(.*\\)"
  "A regular expression that should match a status line in the
commit message information block.  It must contain two groups.
The first must match the status character and the second must
match the path.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;

(defmacro svn-msg-without-modifying (&rest body)
  "Evaluate BODY without changing the buffer's modified status or
its undo list."
  (let ((modified-var (gensym "modified")))
    `(let ((buffer-undo-list t)
           (,modified-var (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p ,modified-var)))))

(defun svn-msg-immutate (regexp)
  "Search for regexp from the beginning of the buffer and make
everything from the point one before the beginning of the regexp
to the end of the buffer read-only."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      ;; Changing text properties updates the undo list and modifies
      ;; the buffer; inhibit this
      (svn-msg-without-modifying
        (let ((inhibit-read-only t))
          ;; Make the ignore block immutable.  We also grab the
          ;; newline before the ignore block to make it impossible to
          ;; insert text at the beginning of the ignore line.  We
          ;; could use a front-sticky property, but then users could
          ;; delete the initial blank line and wind up with a
          ;; completely read-only buffer.
          (add-text-properties (- (match-beginning 0) 1) (point-max)
                               '(read-only t)))))))

(defun svn-cleanup-whitespace ()
  "Remove any extra whitespace between the user text and the ignore
block."
  (save-excursion
    (goto-char (point-min))
    ;; Nuke all of the whitespace leading up to the ignore block
    (when (re-search-forward svn-msg-info-regexp nil t)
      (goto-char (match-beginning 0))
      ;; Leave the ignore line at the beginning of the line
      (if (not (bobp))
          (forward-char -1))
      ;; Scan over extraneous whitespace and delete it
      (let ((end (point)))
        (skip-chars-backward " \n\t")
        ;; Allow trailing whitespace (not doing this is annoying with,
        ;; for example, templates that have unfilled-in labels at the
        ;; bottom)
        (end-of-line)
        (delete-region (point) end))))
  ;; Report that this hook did not save the buffer
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit message restoration
;;

(defun svn-find-old-commit-message (&optional name-base)
  "Search for an old commit message that was left over by a failed
previous commit.  Returns the file name of the old commit message
if found, or nil otherwise.  name-base should specify the base of
the file name to search for and exists to make this function
useful to related modes.  It defaults to \"svn-commit\"."
  (unless name-base
    (setq name-base "svn-commit"))
  (let ((n 0)
        old-message)
    ;; Iterate from 0 to 9
    (dotimes (n 10 old-message)
      ;; Compose the old message name.  Message 0 has no number.
      (let ((filename (format "%s%s.tmp" name-base
                              (if (= n 0) "" (format ".%d" n)))))
        ;; Is it there and not the one that's open?
        (if (and (file-readable-p filename)
                 (not (eq (get-file-buffer filename)
                          (current-buffer))))
            ;; Found one
            (setq old-message filename))))))

(defvar svn-msg-restored-filename nil
  "If this commit message was restored from an old commit message,
this contains the filename of the commit message it was restored
from.  This is used to delete the old message when this message
is saved.  nil if there was no restoration (or if the old message
has been deleted).")
(make-variable-buffer-local 'svn-msg-mode)

(defun svn-load-old-commit-message (filename)
  "Load the contents of an old commit message into this commit
message.  When called interactively, looks for old commit
messages and, if found, prompts the user to restore the old
message.  The user is also prompted for whether or not they want
to delete the old message.  If so, then the name of the old
message is remembered so it can be deleted later by the save hook
`svn-delete-old-commit-message'."
  ;; If there is an old commit message, prompt to restore it
  (interactive
   (let ((old (svn-find-old-commit-message)))
     (if (and old
              (progn
                (save-window-excursion
                  ;; Show the old message in a split window
                  (if svn-msg-show-old-message
                      ;; Make sure we don't get into a restore loop
                      ;; when the other window goes into
                      ;; svn-msg-mode
                      (let ((svn-msg-offer-to-restore nil))
                        (find-file-other-window old)))
                  ;; Restore?
                  (y-or-n-p
                   (format "Old commit message found in %s.  Restore? "
                           old)))))
         ;; If so, pass the old file name
         (list old)
       ;; If not, or there is no old file, pass nil
       (list nil))))

  (when filename
    (let* ((buf (find-file-noselect filename))
           (contents
            (with-current-buffer buf
              (goto-char (point-min))
              (buffer-substring
               (point)
               (progn
                 (goto-char
                  (if (re-search-forward svn-msg-info-regexp nil t)
                      (match-beginning 0)
                    (point-max)))
                 (skip-chars-backward " \n\t")
                 (point))))))
      (insert contents)
      (when (and svn-msg-delete-old-message
                 (or (eq svn-msg-delete-old-message 'auto)
                     (y-or-n-p
                      (format "Delete old commit message %s on save? "
                              filename))))
        (setq svn-msg-restored-filename filename)
        (add-hook 'after-save-hook
                  (function svn-delete-old-commit-message))))))

(defun svn-delete-old-commit-message ()
  "If this buffer was restored from an old commit message by
`svn-load-old-commit-message', then delete the old commit
message."
  (interactive)
  ;; Are we in a commit mode buffer that was restored from an old
  ;; message?
  (when (and (eq major-mode 'svn-msg-mode)
             svn-msg-restored-filename)
    ;; Delete it
    (delete-file svn-msg-restored-filename)
    (setq svn-msg-restored-filename nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visiting diffs
;;

(defun svn-msg-mouse-visit-diff (event)
  "Call `svn-msg-visit-diff' on the file selected by the mouse."
  (interactive "e")
  (let ((posn (event-start event)))
    (select-window (posn-window posn) t)
    (svn-msg-visit-diff (posn-point posn))))

(defun svn-msg-visit-diff (pos)
  "Visit the SVN diff of the file selected at point.  If taking
the diff of the selected file doesn't make sense (for example,
the file was just added), then simply visit the file."
  (interactive "d")
  (when (save-excursion
          (beginning-of-line)
          (looking-at svn-msg-stat-line-regexp))
    (let* ((type (aref (match-string 1) 0))
           (path (match-string 2))
           (name (concat "diff " path)))
      ;; Split the message window and create a buffer for the diff
      (delete-other-windows)
      (select-window (split-window nil (max svn-msg-diff-height
                                            window-min-height)))
      (if (memq type '(?A ?C ?I ?X ?? ?~))
          ;; Simply visit the file.
          (find-file path)
        (switch-to-buffer (concat "*" name "*"))
        (setq buffer-read-only nil)
        (erase-buffer)
        ;; Put this buffer in to diff mode
        (funcall svn-msg-diff-mode)
        ;; Make the buffer read-only
        (setq buffer-read-only t)
        ;; Define 'q' to destroy the window and return to the message
        (let ((ro-map (assq 'buffer-read-only minor-mode-overriding-map-alist)))
          (when ro-map
            (define-key (cdr ro-map) "q" #'svn-msg-quit-diff)))
        ;; Retrieve the diff, asynchronously
        (let ((proc (apply #'start-file-process name (current-buffer)
                           "svn" "diff" (append svn-msg-diff-args
                                                (list path)))))
          ;; Filter the process in the normal way, but don't move
          ;; point
          (set-process-filter proc
                              (lambda (p s)
                                (with-current-buffer (process-buffer p)
                                  (save-excursion
                                    (goto-char (process-mark p))
                                    (let ((inhibit-read-only t))
                                      (insert s))
                                    (set-marker (process-mark p) (point))))))
          ;; Put the process status in the echo area instead of the
          ;; buffer
          (set-process-sentinel proc
                                (lambda (p e)
                                  (message "svn diff %s" e))))))))

(defun svn-msg-quit-diff ()
  "Quit the currently visited diff.

Deletes the current window, kills its buffer, and recenters the
message buffer."
  (interactive)

  (let ((buf (current-buffer)))
    (delete-window)
    (kill-buffer buf)
    (recenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode
;;

;;;###autoload
(define-derived-mode svn-msg-mode text-mode "svn message"
  "Major mode for editing svn commit log messages.

This major mode provides support for editing Subversion commit
messages.  It enhances interface integration by hyperlinking file
status lines to diff's that show the changes being checked in.
It enhances editing by customizing paragraph filling,
automatically cleaning up whitespace, and providing font locking.
Finally, it understands Subversion's behavior when commits fail,
offering to restore the failed commit message and clean up the
stale file.

Features
* Modifies paragraph filling to not wrap the information block
  Subversion appends to commit messages
* Automatically removes extraneous trailing whitespace when
  saving so that logs don't contain extra vertical space
* Gratuitous color
* Previews and offers to restore and delete old commit messages
  left by failed commits
* File status lines are hyperlinked to provide easy access to
  diff's between the working copy and the base revision
* Disables backup files and the saveplace package when editing
  commit messages and inhibits flyspell in the file status block"

  ;; Update the font lock keywords with the status faces and the
  ;; necessary text properties to make hyperlinks work
  (let ((new-keywords
         (mapcar (lambda (re-face)
                   `(,(concat "^" (first re-face) "[ L][ +] \\(.*\\)")
                     (0 '(face ,(second re-face)
                          category svn-msg-stat-line))
                     (1 '(face svn-msg-path-face
                          svn-msg-path t) prepend)))
                 svn-msg-status-faces)))
    (set (make-local-variable 'svn-msg-font-lock-keywords)
         (append svn-msg-font-lock-keywords new-keywords)))

  ;; Enable font lock
  (set (make-local-variable 'font-lock-defaults)
       '(svn-msg-font-lock-keywords nil nil nil nil))

  ;; Disable saveplace, since it's pretty close to meaningless and
  ;; very annoying on log messages
  (when (featurep 'saveplace)
    (setq save-place nil)
    ;; saveplace may already have a position for this filename.  Since
    ;; saveplace will ignore being disabled if it knows the filename,
    ;; take precautions against this.
    (or save-place-loaded (load-save-place-alist-from-file))
    (let ((cell (assoc buffer-file-name save-place-alist)))
      (if cell
          (setq save-place-alist (remq cell save-place-alist)))))

  ;; And disable backups, since these files are meant to be transient
  ;; and the backup files just clutter up the workspace
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)

  ;; Fix paragraph filling
  (make-variable-buffer-local 'paragraph-separate)
  (setq paragraph-separate
        (concat svn-msg-info-regexp "\\|" paragraph-separate))

  ;; Cleanup extraneous whitespace on save (really, svn should do
  ;; this, but judging by the flamewars around this issue, it's not
  ;; going to happen)
  (add-hook 'local-write-file-hooks (function svn-cleanup-whitespace))

  ;; Activate the hyperlinks
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward svn-msg-info-regexp nil t)
      (let ((ov (make-overlay (point) (point-max))))
        (overlay-put ov 'keymap svn-msg-stat-line-map)
        (overlay-put ov 'svn-msg-msg
                     (lambda (obj pos)
                       (substitute-command-keys "\\<svn-msg-stat-line-map>\
\\[svn-msg-mouse-visit-diff], \\[svn-msg-visit-diff]: visit file/diff"))))))

  ;; Make the info block immutable
  (svn-msg-immutate svn-msg-info-regexp)

  ;; Display help echo based on point motion
  (set (make-local-variable 'svn-msg-help-last-point) (point))
  (set (make-local-variable 'svn-msg-help-last-msg) nil)
  (run-with-idle-timer 0.1 t
    (lambda ()
      (when (and (boundp 'svn-msg-help-last-msg)
                 (boundp 'svn-msg-help-last-point)
                 (/= svn-msg-help-last-point (point)))
        (setq svn-msg-help-last-point (point))
        (let ((f (get-char-property (point) 'help-echo)))
          (if f
              (let ((msg (funcall f nil (current-buffer) (point))))
                (when (not (string= msg svn-msg-help-last-msg))
                  (setq svn-msg-help-last-msg msg)
                  (message "%s" msg)))
            (setq svn-msg-help-last-msg nil))))))

  ;; Prompt to load an old commit message, if there is one.  This is
  ;; run off an idle timer so Emacs gets a chance to actually display
  ;; this buffer.  Unfortunately, this means it will pop up a dialog,
  ;; so we explicitly inhibit this.
  (if svn-msg-offer-to-restore
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (let ((last-nonmenu-event t))
                               (call-interactively
                                (function
                                 svn-load-old-commit-message)))))))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "svn-commit\\(\\.[0-9]+\\)?\\.tmp"
                   #'svn-msg-mode))

;; Disable flyspell in the information block
(put 'svn-msg-mode 'flyspell-mode-predicate 'svn-msg-flyspell-verify)
(defun svn-msg-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in
svn-msg-mode.  Causes flyspell to ignore everything in the
info block."
  (save-excursion
    (save-match-data
      (re-search-forward svn-msg-info-regexp nil t))))

(provide 'svn-msg)
