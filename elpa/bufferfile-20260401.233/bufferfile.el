;;; bufferfile.el --- Rename/Delete/Copy Files and Associated Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Package-Version: 20260401.233
;; Package-Revision: 05b893762ede
;; URL: https://github.com/jamescherti/bufferfile.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; This package provides helper functions to delete and rename buffer files:
;; - bufferfile-rename: Renames the file visited by the current buffer and
;;   updates the buffer name for all associated buffers, including clones and
;;   indirect buffers. It also ensures that buffer-local features referencing
;;   the file, such as Eglot, are correctly updated to reflect the new file
;;   name.
;; - bufferfile-delete: Delete the file associated with a buffer and kill all
;;   buffers visiting the file, including clones/indirect buffers.
;; - bufferfile-copy: Copies the file visited by the current buffer to a new
;;   file.
;;
;; (The functions above also ensures that any modified buffers are saved prior
;; to executing operations like renaming, deleting, or copying.)
;;
;; Installation from MELPA
;; -----------------------
;; (use-package bufferfile)

;;; Code:

(require 'hi-lock)

;;; Customizations

(defgroup bufferfile nil
  "Delete or rename buffer files."
  :group 'bufferfile
  :prefix "bufferfile-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/bufferfile.el"))

(defcustom bufferfile-use-vc nil
  "If non-nil, enable using version control (VC) when available.
When this option is enabled and the file being deleted or renamed is under VC,
the renaming operation will be handled by the VC backend."
  :type 'boolean
  :group 'bufferfile)

(defcustom bufferfile-verbose nil
  "If non-nil, display messages during file renaming operations.
When this option is enabled, messages will indicate the progress
and outcome of the renaming process."
  :type 'boolean
  :group 'bufferfile)

(defcustom bufferfile-delete-switch-to 'parent-directory
  "Specifies the action taken after deleting a file and killing its buffer.

Possible values are:
- \\='parent-directory
  Open the parent directory containing the deleted file.

- \\='previous-buffer
  Switch back to the previous buffer.

- nil
  Take no automatic action; allow Emacs to determine the next buffer after
  deleting the file."
  :type '(choice (const :tag "Open parent directory" parent-directory)
                 (const :tag "Switch to previous buffer" previous-buffer)
                 (const :tag "Do nothing" nil))
  :group 'bufferfile)

(defcustom bufferfile-make-target-directory t
  "If non-nil, ensure the destination directory exists.
This applies to file operations such as renaming or copying."
  :type 'boolean
  :group 'bufferfile)

;;; Variables

(defvar bufferfile-dired-integration t
  "Whether to enable Dired integration.
This refreshes all Dired buffers containing the affected files.
Dired must be loaded for this option to have any effect.")

(defvar bufferfile-eglot-integration t
  "Whether to enable Eglot integration.
This reloads Eglot to ensure it references the updated file name.
Eglot must be loaded for this option to have any effect.")

(defvar bufferfile-recentf-integration t
  "Whether to enable recentf integration.
This updates `recentf-list' to ensure it references the updated file name.
`recentf-mode' must be enabled for this option to have any effect.")

(defvar bufferfile-pre-rename-functions nil
  "Hook run before renaming a file.
Each function receives three arguments: (previous-path new-path list-buffers).")

(defvar bufferfile-post-rename-functions nil
  "Hook run after renaming a file.
Each function receives three arguments: (previous-path new-path list-buffers).")

(defvar bufferfile-pre-delete-functions nil
  "Hook run before deleting a file.
Each function receives two arguments: (path list_buffers).")

(defvar bufferfile-post-delete-functions nil
  "Hook run after deleting a file.
Each function receives two arguments: (path list_buffers).")

(defvar bufferfile-message-prefix "[bufferfile] "
  "Prefix used for messages and errors related to bufferfile operations.")

;; Internal

(defvar-local bufferfile--dired-file-selected nil)

;;; Helper functions

(defmacro bufferfile--error (&rest args)
  "Signal an error with `bufferfile-message-prefix' followed by formatted ARGS.
ARGS are formatted as in `format'."
  (declare (indent 0) (debug t))
  `(user-error "%s%s" bufferfile-message-prefix (format ,@args)))

(defun bufferfile--message (&rest args)
  "Display a message with '[bufferfile]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat bufferfile-message-prefix (car args)) (cdr args)))

(defun bufferfile--get-list-buffers (filename)
  "Return a list of buffers visiting the specified FILENAME.

FILENAME is the absolute path of the file to check for associated buffers.

Iterates through all buffers and performs the following check: If a buffer is
visiting the specified FILENAME (based on the true file path), it is added to
the resulting list.

Returns a list of buffers that are associated with FILENAME."
  (let ((filename (file-truename filename))
        (list-buffers nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let* ((base-buf (or (buffer-base-buffer buf) buf))
               (buf-filename (when base-buf (buffer-file-name base-buf))))
          (when (and buf-filename
                     (string-equal filename (file-truename buf-filename)))
            (push buf list-buffers)))))
    list-buffers))

(defun bufferfile--get-buffer-filename (&optional buffer)
  "Return the absolute file path of BUFFER.
If BUFFER is nil, use the current buffer.
Return nil if the buffer is not associated with a file."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((filename (buffer-file-name (buffer-base-buffer))))
        (unless filename
          (bufferfile--error "The buffer '%s' is not associated with a file"
                             (buffer-name)))

        (unless (file-regular-p filename)
          (bufferfile--error "The file '%s' does not exist on disk"
                             filename))

        (expand-file-name filename)))))

(defun bufferfile--read-dest-file-name (filename prompt-prefix)
  "Prompt for a destination file name different from FILENAME.
PROMPT-PREFIX: The text prepended to the user input prompt."
  (let* ((basename (file-name-nondirectory filename))
         (new-filename (read-file-name
                        (format "%s%s'%s' to: "
                                bufferfile-message-prefix
                                prompt-prefix
                                basename)
                        (file-name-directory filename)
                        nil
                        nil
                        basename
                        #'(lambda(filename)
                            (file-regular-p filename)))))
    (unless new-filename
      (bufferfile--error "A new file name must be specified"))

    (when (string= (file-truename filename)
                   (file-truename new-filename))
      (bufferfile--error
        "Ignored because the destination is the same as the source"))
    new-filename))

(defun bufferfile--read-dest-file-name-rename (filename ok-if-already-exists)
  "Prompt for a destination file name different from FILENAME.
Signal an error if a filename NEWNAME already exists unless OK-IF-ALREADY-EXISTS
is non-nil."
  (let ((new-filename (bufferfile--read-dest-file-name filename "Rename ")))
    (when (and (not ok-if-already-exists)
               (file-exists-p new-filename))
      (unless (y-or-n-p
               (format
                "Destination file '%s' already exists. Do you want to overwrite it?"
                new-filename))
        (bufferfile--error
          "Rename failed: Destination filename already exists: %s"
          new-filename)))

    new-filename))

(defun bufferfile--vc-delete-file (file)
  "Delete file and mark it as such in the version control system.
If called interactively, read FILE, defaulting to the current
buffer's file name if it's under version control."
  (interactive (list (read-file-name "VC delete file: " nil
                                     (when (vc-backend buffer-file-name)
                                       buffer-file-name)
                                     t)))
  (setq file (expand-file-name file))
  (let ((buf (get-file-buffer file))
        (backend (vc-backend file)))
    (unless backend
      (error "File %s is not under version control"
             (file-name-nondirectory file)))
    (unless (vc-find-backend-function backend 'delete-file)
      (error "Deleting files under %s is not supported in VC" backend))
    (when (and buf (buffer-modified-p buf))
      (error "Please save or undo your changes before deleting %s" file))
    (let ((state (vc-state file)))
      (when (eq state 'edited)
        (error "Please commit or undo your changes before deleting %s" file))
      (when (eq state 'conflict)
        (error "Please resolve the conflicts before deleting %s" file)))
    (unless (or (file-directory-p file) (null make-backup-files)
                (not (file-exists-p file)))
      (with-current-buffer (or buf (find-file-noselect file))
        (let ((backup-inhibited nil))
          (backup-buffer))))
    ;; Bind `default-directory' so that the command that the backend
    ;; runs to remove the file is invoked in the correct context.
    (let ((default-directory (file-name-directory file)))
      (vc-call-backend backend 'delete-file file))
    ;; If the backend hasn't deleted the file itself, let's do it for him.
    (when (file-exists-p file) (delete-file file))
    ;; Forget what VC knew about the file.
    (vc-file-clearprops file)
    ;; Make sure the buffer is deleted and the *vc-dir* buffers are
    ;; updated after this.
    (when (fboundp 'vc-resynch-buffer)
      (funcall 'vc-resynch-buffer file nil t))))

(defun bufferfile--get-dired-buffers-visiting (directory)
  "Get all Dired buffers visiting DIRECTORY."
  (let ((result nil))
    (when directory
      (let ((dir (file-truename directory)))
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (and (derived-mode-p 'dired-mode)
                         (string= (file-truename default-directory)
                                  dir))
                (push buf result)))))))
    result))

(defun bufferfile--refresh-dired-buffers (directory &optional goto-file)
  "Refresh all Dired buffers visiting DIRECTORY.
GOTO-FILE, if provided, moves the cursor to this file in each refreshed Dired
buffer."
  (when goto-file
    (setq goto-file (expand-file-name goto-file)))
  (when directory
    (dolist (buf (bufferfile--get-dired-buffers-visiting directory))
      (with-current-buffer buf
        (when (fboundp 'dired-revert)
          (when bufferfile-verbose
            (bufferfile--message "dired-revert: %s" default-directory))
          (ignore-errors
            (funcall 'dired-revert))))))

  ;; Walk windows
  (walk-windows
   (lambda (window)
     (when window
       (with-selected-window window
         (when (derived-mode-p 'dired-mode)
           (when (and goto-file
                      (fboundp 'dired-goto-file)
                      bufferfile--dired-file-selected)
             (when bufferfile-verbose
               (bufferfile--message "dired-goto-file: %s" goto-file))
             (funcall 'dired-goto-file goto-file))))))
   ;; Exclude the minibuffer
   nil
   ;; Apply to all frames
   t))

;;; Rename file

(defun bufferfile--generate-buffer-name (&optional newname)
  "Return a unique buffer name based on NEWNAME or base buffer name.
Normalizes any trailing <n> suffix. Does not rename the current buffer."
  (setq newname (or newname (buffer-name (buffer-base-buffer))))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (setq newname (generate-new-buffer-name newname))
  newname)

(defun bufferfile--rename-all-buffers (old-filename new-filename)
  "Update buffer names and files they are visiting to reflect the renaming.
OLD-FILENAME and NEW-FILENAME are absolute paths as returned by
`expand-file-name'.

For all buffers associated with OLD-FILENAME, update the buffer names to use
NEW-FILENAME.

This includes indirect buffers whose names are derived from the old filename."
  (setq old-filename (expand-file-name old-filename))
  (setq new-filename (expand-file-name new-filename))

  ;; Update the file name associated with buffers visiting files
  (let (file-visiting-buffers
        indirect-buffers
        (old-filename-truename (file-truename old-filename)))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((base-buffer (buffer-base-buffer)))
            (if base-buffer
                (push buf indirect-buffers)
              (push buf file-visiting-buffers))

            ;; File-visiting buffer
            (when (not base-buffer)
              (when (and buffer-file-name
                         (string= (file-truename buffer-file-name)
                                  old-filename-truename))
                (set-visited-file-name new-filename t t)))))))

    ;; Update the names of file visiting buffer and indirect buffers (clones)
    ;; associated with buffers visiting the renamed files.
    (let ((new-filename-truename (file-truename new-filename)))
      (dolist (buf indirect-buffers)
        (with-current-buffer buf
          (when-let* ((base-buffer (buffer-base-buffer)))
            (let ((base-buffer-filename (buffer-file-name base-buffer)))
              (when (and base-buffer-filename
                         (string= (file-truename base-buffer-filename)
                                  new-filename-truename))
                (rename-buffer (bufferfile--generate-buffer-name) t)))))))))

(defun bufferfile-rename-file (filename
                               new-filename
                               &optional ok-if-already-exists
                               confirm-overwrite)
  "Rename FILENAME to NEW-FILENAME.

This function updates:
- The filename name on disk,
- The buffer name,
- All the indirect buffers or other buffers associated with the old filename.

Hooks in `bufferfile-pre-rename-functions' and
`bufferfile-post-rename-functions' are run before and after the renaming
process.

When CONFIRM-OVERWRITE is non-nil, prompt the user for confirmation before
overwriting existing files.

Signal an error if NEW-FILENAME already exists unless OK-IF-ALREADY-EXISTS is
non-nil."
  (let ((src-truename (file-truename filename))
        (dst-truename (file-truename new-filename))
        list-buffers)
    (when (string= src-truename dst-truename)
      (bufferfile--error "Source and destination are the same file: '%s'"
                         src-truename))

    (unless (file-exists-p filename)
      (bufferfile--error "Source file '%s' does not exist; cannot move to '%s'"
                         filename new-filename))

    (when (file-exists-p new-filename)
      (if (and (not ok-if-already-exists)
               confirm-overwrite)
          (if (y-or-n-p
               (format "Destination file '%s' already exists. Overwrite? "
                       new-filename))
              (setq ok-if-already-exists t)
            (bufferfile--error "Rename aborted"))
        (bufferfile--error "Destination file '%s' already exists"
                           new-filename)))

    (setq list-buffers (bufferfile--get-list-buffers filename))

    (run-hook-with-args 'bufferfile-pre-rename-functions
                        filename
                        new-filename
                        list-buffers)

    (when bufferfile-use-vc
      (require 'vc))

    ;; Dired
    (when bufferfile-dired-integration
      (let ((parent-dir-path (file-name-directory (expand-file-name filename)))
            (filename-truename (file-truename filename)))
        (dolist (buf (bufferfile--get-dired-buffers-visiting parent-dir-path))
          (with-current-buffer buf
            (when (fboundp 'dired-get-file-for-visit)
              (let ((file (funcall 'dired-get-file-for-visit)))
                (setq-local bufferfile--dired-file-selected
                            (and file (string= (file-truename file)
                                               filename-truename)))))))))

    ;; Ensure that the destination directory exists
    (when bufferfile-make-target-directory
      (when-let* ((dest-dir (file-name-directory new-filename)))
        (make-directory dest-dir t)))

    (if (and bufferfile-use-vc
             (vc-backend filename)
             (let ((root1
                    (let ((default-directory (file-name-directory filename)))
                      (vc-root-dir)))
                   (root2
                    (let ((default-directory (file-name-directory new-filename)))
                      (vc-root-dir))))
               (and root1
                    root2
                    (string= root1 root2))))
        (progn
          (when bufferfile-verbose
            (bufferfile--message
             "VC Rename: %s -> %s"
             (abbreviate-file-name filename)
             (abbreviate-file-name new-filename)))
          (when (and ok-if-already-exists (file-exists-p new-filename))
            ;; If the destination file exists, `vc-rename-file' cannot perform
            ;; the rename; the destination must be deleted first.
            (delete-file new-filename))
          ;; VC Rename
          (vc-rename-file filename new-filename))
      ;; Rename the file
      (rename-file filename new-filename ok-if-already-exists)
      (when bufferfile-verbose
        (bufferfile--message "Rename: %s -> %s"
                             (abbreviate-file-name filename)
                             (abbreviate-file-name new-filename))))

    ;; Update all buffers pointing to the old filename Broken
    (bufferfile--rename-all-buffers filename new-filename)

    (when (and bufferfile-recentf-integration
               (bound-and-true-p recentf-mode)
               (boundp 'recentf-list)
               (fboundp 'recentf-string-member)
               (fboundp 'recentf-add-file))
      (let* ((expanded (expand-file-name filename))
             (truename (file-truename expanded))
             (list-targets (list filename
                                 expanded
                                 (abbreviate-file-name expanded)
                                 truename
                                 (abbreviate-file-name truename))))
        (dolist (target list-targets)
          (let ((member (recentf-string-member target recentf-list)))
            (when member
              (setq recentf-list (delq (car member) recentf-list))))))

      (recentf-add-file new-filename))

    (when bufferfile-eglot-integration
      (dolist (buf list-buffers)
        (with-current-buffer buf
          ;; Fix Eglot
          (when (and (fboundp 'eglot-current-server)
                     (fboundp 'eglot-shutdown)
                     (fboundp 'eglot-managed-p)
                     (fboundp 'eglot-ensure)
                     (funcall 'eglot-managed-p))
            (let ((server (funcall 'eglot-current-server)))
              (when server
                ;; Restart eglot
                (let ((inhibit-message t))
                  (funcall 'eglot-shutdown server))
                (funcall 'eglot-ensure)))))))

    (when bufferfile-dired-integration
      ;; Refresh previous directory (special case: moving files)
      ;; TODO: check if the directory if filename is different from new-filename
      (let ((parent-dir-path (file-name-directory (expand-file-name filename))))
        (bufferfile--refresh-dired-buffers parent-dir-path))

      ;; Refresh the dired buffer
      (let ((parent-dir-path (file-name-directory (expand-file-name new-filename))))
        (bufferfile--refresh-dired-buffers parent-dir-path new-filename)))

    (run-hook-with-args 'bufferfile-post-rename-functions
                        filename
                        new-filename
                        list-buffers)))

;;;###autoload
(defun bufferfile-rename (&optional buffer)
  "Rename the file visited by the current buffer.
If BUFFER is provided, operate on the file visited by that buffer instead.

This command updates:
- The file name on disk,
- the buffer name,
- all the indirect buffers or other buffers associated with the old file.

Hooks in `bufferfile-pre-rename-functions' and
`bufferfile-post-rename-functions' are run before and after the renaming
process."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (let* ((filename (bufferfile--get-buffer-filename))
           (original-buffer (or (buffer-base-buffer) (current-buffer)))
           (ok-if-already-exists t))
      (with-current-buffer original-buffer
        (when (buffer-modified-p)
          (let ((save-silently (not bufferfile-verbose)))
            (save-buffer)))

        (let ((new-filename
               (bufferfile--read-dest-file-name-rename filename
                                                       ok-if-already-exists)))
          (bufferfile-rename-file filename new-filename nil t))))))

;;; Dired do rename

(defun bufferfile-dired-do-rename (&optional arg)
  "Rename the current file or all marked files in Dired.
ARG is the same argument as `dired-do-rename'.
If one file is marked, delegate to `bufferfile-rename'.
If multiple files are marked, delegate to `dired-do-rename'."
  (interactive "P")
  (when (and (derived-mode-p 'dired-mode)
             (fboundp 'dired-get-marked-files)
             (fboundp 'dired-post-do-command))
    (let* ((marked-files (dired-get-marked-files nil arg))
           new-filename
           old-filename)
      (if (> (length marked-files) 1)
          (when (fboundp 'dired-do-rename)
            (dired-do-rename arg))
        (progn
          (when (or (not marked-files) (= (length marked-files) 0))
            (bufferfile--error "You need to select at least one file"))

          (setq old-filename (car marked-files))
          (setq new-filename
                (bufferfile--read-dest-file-name-rename old-filename t))

          (when new-filename
            (bufferfile-rename-file old-filename new-filename t)))))))

;;; Delete file

;;;###autoload
(defun bufferfile-delete (&optional buffer)
  "Kill the current buffer and delete the file associated with it.
Delete the file associated with a buffer and kill all buffers visiting the file,
including indirect buffers or clones.
If BUFFER is nil, operate on the current buffer.

Hooks in `bufferfile-pre-delete-functions' and
`bufferfile-post-delete-functions' are run before and after the renaming
process."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let* ((buffer (or buffer (current-buffer)))
           (filename nil))
      (unless (buffer-live-p buffer)
        (bufferfile--error "The buffer '%s' is not alive"
                           (buffer-name buffer)))

      (setq filename (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
      (unless filename
        (bufferfile--error "The buffer '%s' is not visiting a file"
                           (buffer-name buffer)))
      (setq filename (expand-file-name filename))

      (when (yes-or-no-p (format "Delete file '%s'?"
                                 (file-name-nondirectory filename)))
        (when bufferfile-use-vc
          (require 'vc))
        (let* ((vc-managed-file (when bufferfile-use-vc
                                  (vc-backend filename)))
               (list-buffers (bufferfile--get-list-buffers filename))
               (parent-dir-path (file-name-directory filename)))
          (unless parent-dir-path
            (bufferfile--error "Cannot find the parent directory of: %s"
                               filename))
          (dolist (buf list-buffers)
            (with-current-buffer buf
              (when (buffer-modified-p)
                (let ((save-silently t))
                  (save-buffer)))))

          (run-hook-with-args 'bufferfile-pre-delete-functions
                              filename list-buffers)

          (when vc-managed-file
            ;; Revert version control changes before killing the buffer;
            ;; otherwise, `vc-delete-file' will fail to delete the file
            (when (not (vc-up-to-date-p filename))
              (with-current-buffer buffer
                (if (fboundp 'vc-revert-file)
                    (vc-revert-file filename)
                  (bufferfile--error "'vc-revert-file' has not been declared")))))

          ;; Kill buffer
          (when bufferfile-eglot-integration
            (dolist (buf list-buffers)
              (with-current-buffer buf
                (when (and (fboundp 'eglot-current-server)
                           (fboundp 'eglot-shutdown)
                           (fboundp 'eglot-managed-p)
                           (funcall 'eglot-managed-p))
                  (let ((server (funcall 'eglot-current-server)))
                    (when server
                      ;; Do not display errors such as:
                      ;; [jsonrpc] (warning) Sentinel for EGLOT
                      ;; (ansible-unused/(python-mode python-ts-mode)) still
                      ;; hasn't run, deleting it!
                      ;; [jsonrpc] Server exited with status 9
                      (let ((inhibit-message t))
                        (funcall 'eglot-shutdown server))))))
              (kill-buffer buf)))

          ;; Find file first
          (cond
           ((eq bufferfile-delete-switch-to 'parent-directory)
            (let ((parent-dir-buffer (find-file parent-dir-path)))
              (when (buffer-live-p parent-dir-buffer)
                (with-current-buffer parent-dir-buffer
                  (when (and (derived-mode-p 'dired-mode)
                             (fboundp 'dired-goto-file))
                    (dired-goto-file filename)))
                (switch-to-buffer parent-dir-buffer nil t))))

           ((eq bufferfile-delete-switch-to 'previous-buffer)
            (previous-buffer)))

          (when (file-exists-p filename)
            (if (and bufferfile-use-vc
                     vc-managed-file)
                ;; VC delete
                (bufferfile--vc-delete-file filename)
              ;; Delete
              (delete-file filename delete-by-moving-to-trash)))

          (when bufferfile-verbose
            (bufferfile--message "Deleted: %s" (abbreviate-file-name filename)))

          ;; Refresh dired buffers
          (when bufferfile-dired-integration
            (bufferfile--refresh-dired-buffers parent-dir-path))

          (run-hook-with-args 'bufferfile-post-delete-functions
                              filename
                              list-buffers))))))

;;; Copy

(defun bufferfile-copy (&optional buffer)
  "Copy the current file of that BUFFER is visiting."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (let* ((filename (bufferfile--get-buffer-filename))
           (original-buffer (or (buffer-base-buffer) (current-buffer))))
      ;; Save
      (with-current-buffer original-buffer
        (when (buffer-modified-p)
          (let ((save-silently (not bufferfile-verbose)))
            (save-buffer)))

        ;; Prompt user
        (let ((new-filename (bufferfile--read-dest-file-name filename
                                                             "Copy ")))
          (when bufferfile-verbose
            (bufferfile--message "Copy: %s -> %s"
                                 (abbreviate-file-name filename)
                                 (abbreviate-file-name new-filename)))

          ;; Ensure that the destination directory exists
          (when bufferfile-make-target-directory
            (when-let* ((dest-dir (file-name-directory new-filename)))
              (make-directory dest-dir t)))

          (copy-file filename new-filename t)

          ;; Refresh dired buffers
          (when bufferfile-dired-integration
            ;; Refresh the dired buffer
            (let ((parent-dir-path (file-name-directory (expand-file-name new-filename))))
              (bufferfile--refresh-dired-buffers parent-dir-path new-filename))))))))

;;; Provide
(provide 'bufferfile)
;;; bufferfile.el ends here
