;;; buffer-guardian.el --- Automatically Save Buffers Without Manual Intervention -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "25.1"))
;; Keywords: maint
;; Package-Version: 20260428.135
;; Package-Revision: 4407655c9a39
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

;; The `buffer-guardian' package provides a global mode that automatically saves
;; buffers without requiring manual intervention.
;;
;; By default, `buffer-guardian-mode' saves file-visiting buffers when:
;; - Switching to another buffer.
;; - Switching to another window or frame.
;; - The window configuration changes (e.g., window splits).
;; - The minibuffer is opened.
;; - Emacs loses focus.
;;
;; In addition to regular file-visiting buffers, `buffer-guardian-mode' also
;; handles specialized editing buffers used for inline code blocks, such as
;; `org-src' (for Org mode) and `edit-indirect' (commonly used for Markdown
;; source code blocks). These temporary buffers are linked to an underlying
;; parent buffer. Automatically saving them ensures that modifications made
;; within these isolated code environments are correctly propagated back to the
;; original Org or Markdown file.
;;
;; Features disabled by default but can be enabled:
;; - Save the buffer even if a window change results in the same buffer being
;;   selected.
;; - Save all file-visiting buffers periodically at a specific interval.
;; - Save all file-visiting buffers after a period of user inactivity.
;; - Prevent auto-saving remote files.
;; - Prevent saving files that do not exist on disk.
;; - Set a maximum buffer size limit for auto-saving.
;; - Ignore buffers whose names match specific regular expressions.
;; - Use custom predicate functions to determine if a buffer should be saved.
;;
;; (Buffer Guardian runs in the background without interrupting the workflow.
;; For example, the package safely aborts the auto-save process if the file is
;; read-only, if the file's parent directory does not exist, or if the file was
;; modified externally. Additionally, it gracefully catches and logs errors if a
;; third-party hook attempts to request user input, ensuring that the editor
;; never freezes during an automatic background save.)

;;; Code:

(require 'seq)

;;; Customizations

(defgroup buffer-guardian nil
  "Customization options for `buffer-guardian-mode'."
  :group 'buffer-guardian
  :prefix "buffer-guardian-")

(defcustom buffer-guardian-verbose nil
  "Enable verbose mode to log when a buffer is automatically saved."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-focus-loss t
  "Save the current buffer when Emacs loses focus."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (boundp 'after-focus-change-function)
             (with-no-warnings
               (if (and value (bound-and-true-p buffer-guardian-mode))
                   (add-function :after after-focus-change-function
                                 #'buffer-guardian--on-focus-change)
                 (remove-function after-focus-change-function
                                  #'buffer-guardian--on-focus-change)))
           ;; Emacs <= 26
           (with-no-warnings
             (if (and value (bound-and-true-p buffer-guardian-mode))
                 (add-hook 'focus-out-hook #'buffer-guardian--on-focus-change)
               (remove-hook 'focus-out-hook #'buffer-guardian--on-focus-change)))))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-minibuffer-setup t
  "Save the current buffer when the minibuffer is opened."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value (bound-and-true-p buffer-guardian-mode))
             (add-hook 'minibuffer-setup-hook
                       #'buffer-guardian--minibuffer-setup-hook)
           (remove-hook 'minibuffer-setup-hook
                        #'buffer-guardian--minibuffer-setup-hook)))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-buffer-switch t
  "Save the current buffer when `window-buffer-change-functions' runs."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value (bound-and-true-p buffer-guardian-mode))
             (add-hook 'window-buffer-change-functions
                       #'buffer-guardian--window-buffer-change-functions)
           (remove-hook 'window-buffer-change-functions
                        #'buffer-guardian--window-buffer-change-functions)))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-window-selection-change t
  "Save the current buffer when `window-selection-change-functions' runs."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value (bound-and-true-p buffer-guardian-mode))
             (progn
               (add-hook 'window-selection-change-functions
                         #'buffer-guardian--window-selection-change)
               (add-hook 'mouse-leave-buffer-hook
                         #'buffer-guardian--mouse-leave-buffer-hook))
           (remove-hook 'window-selection-change-functions
                        #'buffer-guardian--window-selection-change)
           (remove-hook 'mouse-leave-buffer-hook
                        #'buffer-guardian--mouse-leave-buffer-hook)))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-window-configuration-change t
  "Save the current buffer when `window-configuration-change-hook' runs."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value (bound-and-true-p buffer-guardian-mode))
             (add-hook 'window-configuration-change-hook
                       #'buffer-guardian--window-configuration-change)
           (remove-hook 'window-configuration-change-hook
                        #'buffer-guardian--window-configuration-change)))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-on-same-buffer-window-change nil
  "Save the buffer even if the window change results in the same buffer."
  :type 'boolean
  :group 'buffer-guardian)

(defvar buffer-guardian--save-all-buffers-timer nil
  "Internal Timer object for saving all buffers.")

(defvar buffer-guardian--save-all-buffers-idle-timer nil
  "Internal timer object for saving all buffers when the user is idle.")

(defcustom buffer-guardian-save-all-buffers-interval nil
  "Interval in seconds for automatically saving all buffers.
This allows you to periodically save all file-visiting buffers at once,
repeating the operation at the specified interval.

If set to nil, this feature is disabled."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when buffer-guardian--save-all-buffers-timer
           (cancel-timer buffer-guardian--save-all-buffers-timer)
           (setq buffer-guardian--save-all-buffers-timer nil))
         (when (and value (bound-and-true-p buffer-guardian-mode))
           (setq buffer-guardian--save-all-buffers-timer
                 (run-with-timer value value #'buffer-guardian-save-all-buffers))))
  :group 'buffer-guardian)

(defcustom buffer-guardian-save-all-buffers-idle nil
  "Seconds for automatically saving all buffers when the user is idle.
This allows you to save all file-visiting buffers at once, repeating the
operation at the specified interval.

If set to nil, this feature is disabled."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when buffer-guardian--save-all-buffers-idle-timer
           (cancel-timer buffer-guardian--save-all-buffers-idle-timer)
           (setq buffer-guardian--save-all-buffers-idle-timer nil))
         (when (and value (bound-and-true-p buffer-guardian-mode))
           (setq buffer-guardian--save-all-buffers-idle-timer
                 (run-with-idle-timer
                  value value #'buffer-guardian-save-all-buffers))))
  :group 'buffer-guardian)

(defcustom buffer-guardian-inhibit-saving-remote-files t
  "If non-nil, `buffer-guardian' will not auto-save remote files.
When set to nil, remote files will be included in the auto-save process. This
setting is used by `buffer-guardian--predicate'."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-inhibit-saving-nonexistent-files t
  "If non-nil, `buffer-guardian' will not save files that do not exist on disk.
When set to nil, buffers visiting nonexistent files can still be saved.
This setting is used by `buffer-guardian--predicate'."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-exclude-regexps nil
  "A list of regexps for buffer file names excluded from buffer-guardian.
When a buffer file name matches any of the regexps it is ignored."
  :group 'buffer-guardian
  :type '(repeat regexp))

(defcustom buffer-guardian-max-buffer-size nil
  "Maximal size of buffer (in characters), for which buffer-guardian works.
Exists mostly because saving constantly huge buffers can be slow in some cases.
Set to nil to disable."
  :group 'buffer-guardian
  :type 'integer)

(defcustom buffer-guardian-predicate-functions nil
  "Predicates, which return nil, when the buffer doesn't need to be saved.
Predicate functions don't take any arguments. If a predicate doesn't know
whether this buffer needs to be saved or not, then it must return t."
  :group 'buffer-guardian
  :type '(repeat function))

(defcustom buffer-guardian-save-all-buffers-trigger-hooks nil
  "List of hook symbols that trigger saving of all modified buffers.
When any of these hooks run, all buffers are saved."
  :group 'buffer-guardian
  :type '(repeat hook)
  :set (lambda (symbol value)
         (let ((old-value (when (boundp symbol)
                            (default-value symbol))))
           (set-default symbol value)
           (when old-value
             (dolist (hook old-value)
               (remove-hook hook #'buffer-guardian-save-all-buffers)))
           (when (bound-and-true-p buffer-guardian-mode)
             (dolist (hook value)
               (add-hook hook #'buffer-guardian-save-all-buffers))))))

(defvar buffer-guardian--list-advised-functions nil
  "Internal list of advised functions.")

(defcustom buffer-guardian-save-trigger-functions nil
  "List of function symbols to be advised by `buffer-guardian'.

A :before advice is added to each function in this list so that the current
buffer is saved before the function executes.

This mechanism allows automatic buffer saving to be triggered by specific
commands or operations (e.g., window switching or navigation).

Set this variable to nil to disable advising altogether."
  :group 'buffer-guardian
  :type '(repeat function)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when buffer-guardian--list-advised-functions
           (dolist (func buffer-guardian--list-advised-functions)
             (when (fboundp func)
               (advice-remove
                func
                #'buffer-guardian--before-advice-save-current-buffer))))
         (setq buffer-guardian--list-advised-functions (copy-sequence value))
         (when (bound-and-true-p buffer-guardian-mode)
           (dolist (func value)
             (when (fboundp func)
               (advice-add
                func :before
                #'buffer-guardian--before-advice-save-current-buffer))))))

(defcustom buffer-guardian-handle-org-src t
  "Enable automatic saving for `org-src' buffers.
When non-nil, `buffer-guardian' will automatically save the temporary buffers
used to edit Org mode source code blocks. This ensures that changes made in the
specialized code editor are propagated back to the parent Org file."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-handle-edit-indirect t
  "Enable automatic saving for `edit-indirect' buffers.
When non-nil, `buffer-guardian' will automatically save buffers managed by the
`edit-indirect' package. These are commonly used to edit source code blocks in
Markdown files (`markdown-mode') or other multi-layered documents. Saving these
buffers ensures modifications are committed back to the original parent buffer."
  :type 'boolean
  :group 'buffer-guardian)

(defcustom buffer-guardian-debounce-delay 1.0
  "Delay in seconds before saving after a trigger event (e.g., window change).

This prevents excessive saving if events fire rapidly. Because Emacs is
single-threaded and disk I/O operations are expensive, saving buffers on every
single trigger would cause the editor to freeze.

Certain actions trigger hooks aggressively. For example, resizing a window with
the mouse or packages that automatically rebuild layouts can trigger the change
hook dozens of times in a single second.

This delay makes buffer-guardian wait for a period of absolute user inactivity
before executing the global save operation. If another trigger event occurs
before the delay expires, the countdown resets to zero."
  :type 'number
  :group 'buffer-guardian)

(defvar buffer-guardian--debounce-timer nil
  "Internal timer used to debounce save operations.")

;;; Internal functions

(defun buffer-guardian--exclude-regexps-p (filename)
  "Return non-nil if FILENAME matches any of `buffer-guardian-exclude-regexps'."
  (and filename
       (seq-some (lambda (regexp)
                   (string-match-p regexp filename))
                 buffer-guardian-exclude-regexps)))

(defun buffer-guardian--predicate (&optional include-non-file-visiting)
  "Determine if the current buffer should be automatically saved.

If INCLUDE-NON-FILE-VISITING is non-nil, the predicate recognizes and returns
specialized symbols for \='org-src and \='edit-indirect buffers.

Returns: \='org-src, \='edit-indirect, t, or nil."
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and (buffer-modified-p)
               (not (buffer-guardian--exclude-regexps-p file-name))
               (or (not buffer-guardian-max-buffer-size)
                   (< buffer-guardian-max-buffer-size 0)
                   (<= (buffer-size) buffer-guardian-max-buffer-size))
               (if buffer-guardian-predicate-functions
                   (seq-every-p (lambda (pred)
                                  (when (functionp pred)
                                    (condition-case err
                                        (funcall pred)
                                      (error
                                       (display-warning
                                        'buffer-guardian
                                        (format "Predicate failed: %S" err)
                                        :warning)
                                       nil))))
                                buffer-guardian-predicate-functions)
                 t))
      (cond
       ;; Specialized buffers
       ((and include-non-file-visiting
             buffer-guardian-handle-org-src
             (fboundp 'org-src-edit-buffer-p)
             (org-src-edit-buffer-p))
        'org-src)

       ((and include-non-file-visiting
             buffer-guardian-handle-edit-indirect
             (bound-and-true-p edit-indirect--overlay))
        'edit-indirect)

       ;; Standard File-visiting logic
       (file-name
        (cond
         ;; Fast string check: Short-circuit if remote saving is disabled
         ((and buffer-guardian-inhibit-saving-remote-files
               (file-remote-p file-name))
          (when buffer-guardian-verbose
            (message (concat "[buffer-guardian] Warning: Automatic save "
                             "skipped for '%s' because it is a remote file.")
                     file-name))
          ;; Return nil
          nil)

         ;; Disk check: Verify file existence
         ((and buffer-guardian-inhibit-saving-nonexistent-files
               (not (file-exists-p file-name)))
          (when buffer-guardian-verbose
            (message (concat "[buffer-guardian] Warning: Automatic save "
                             "skipped for '%s' because the file does not "
                             "exist.")
                     file-name))
          ;; Return nil
          nil)

         ;; Passed all checks
         (t
          ;; Return t
          t)))))))

(defun buffer-guardian--before-advice-save-current-buffer (&rest _)
  "Save current buffers."
  (when (bound-and-true-p buffer-guardian-mode)
    (buffer-guardian-save-buffer-maybe (current-buffer))))

(defun buffer-guardian--on-focus-change ()
  "Run `buffer-guardian-save-all-buffers' when Emacs loses focus."
  (when (and (bound-and-true-p buffer-guardian-mode)
             buffer-guardian-save-on-focus-loss
             ;; The frame is unfocused
             (not (when (fboundp 'frame-focus-state)
                    (frame-focus-state))))
    (buffer-guardian-save-all-buffers)))

(defun buffer-guardian--minibuffer-setup-hook ()
  "Save the buffer whenever the minibuffer is open."
  (when (and (bound-and-true-p buffer-guardian-mode)
             buffer-guardian-save-on-minibuffer-setup)
    (let* ((window (minibuffer-selected-window))
           (buffer (when window
                     (window-buffer window))))
      (when (buffer-live-p buffer)
        (buffer-guardian-save-buffer-maybe buffer)))))

(defun buffer-guardian--mouse-leave-buffer-hook ()
  "Save the current buffer when the mouse clicks on another buffer."
  (when (bound-and-true-p buffer-guardian-mode)
    (buffer-guardian-save-buffer-maybe (current-buffer))))

(defvar buffer-guardian--previous-buffer nil
  "Internal. Tracks the last seen buffer for auto-saving on window changes.

Because window change hooks execute after the focus has already shifted, the
`buffer-guardian--previous-buffer' is the most reliable way to track the buffer
the user just left, regardless of whether they switched windows, split windows,
or moved to a new frame.")

(defun buffer-guardian--on-buffer-change (&optional object)
  "Function called by `window-buffer-change-functions'.
OBJECT can be a frame or a window."
  (let* ((is-frame (frame-live-p object))
         (frame (if is-frame object (selected-frame)))
         (window (cond
                  (is-frame (frame-selected-window object))
                  ((window-live-p object) object)
                  (t (selected-window)))))
    (when (and frame window)
      (let ((buffer (window-buffer window)))
        (when (buffer-live-p buffer)
          (when (or buffer-guardian-save-on-same-buffer-window-change
                    (not (eq buffer buffer-guardian--previous-buffer)))
            ;; Save previous buffer
            (when (buffer-live-p buffer-guardian--previous-buffer)
              (buffer-guardian-save-buffer-maybe
               buffer-guardian--previous-buffer))
            ;; Update tracker to current buffer
            (setq buffer-guardian--previous-buffer buffer)))))))

(defun buffer-guardian--window-buffer-change-functions (object)
  "Run on window change in OBJECT (frame or window)."
  (when (and buffer-guardian-save-on-buffer-switch
             (bound-and-true-p buffer-guardian-mode))
    (buffer-guardian--on-buffer-change object)))

(defun buffer-guardian--window-selection-change (object)
  "Run on window change in OBJECT (frame or window)."
  (when (and buffer-guardian-save-on-window-selection-change
             (bound-and-true-p buffer-guardian-mode))
    (buffer-guardian--on-buffer-change object)))

(defun buffer-guardian-save-all-buffers-debounced ()
  "Debounced version of `buffer-guardian-save-all-buffers'."
  (when buffer-guardian--debounce-timer
    (cancel-timer buffer-guardian--debounce-timer))
  (setq buffer-guardian--debounce-timer
        (run-with-timer buffer-guardian-debounce-delay nil
                        #'buffer-guardian-save-all-buffers)))

(defun buffer-guardian--window-configuration-change ()
  "Run on window configuration change."
  (when (and buffer-guardian-save-on-window-configuration-change
             (bound-and-true-p buffer-guardian-mode))
    (buffer-guardian-save-all-buffers-debounced)))

;;; Functions

;;;###autoload
(defun buffer-guardian-save-buffer-maybe (&optional buffer)
  "Save BUFFER if it is visiting a file that is existing on the disk.
By default, it only saves when the file exists on the disk."
  (let ((target-buffer (or buffer (current-buffer))))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (let ((predicate-result (buffer-guardian--predicate t)))
          (when predicate-result
            (cond
             ((and (eq predicate-result 'org-src)
                   (fboundp 'org-edit-src-save))
              (org-edit-src-save)
              (when buffer-guardian-verbose
                (message "[buffer-guardian] Org-src Save: '%s'"
                         (buffer-name))))

             ((and (eq predicate-result 'edit-indirect)
                   (fboundp 'edit-indirect--commit))
              (edit-indirect--commit)
              (when buffer-guardian-verbose
                (message "[buffer-guardian] Edit-indirect Save: '%s'"
                         (buffer-name))))

             ;; File-visiting buffers
             (t
              (let* ((file-name (buffer-file-name (buffer-base-buffer)))
                     (file-dir (when file-name
                                 (file-name-directory file-name))))
                (cond
                 ((not file-name)
                  (when buffer-guardian-verbose
                    (message
                     (concat "[buffer-guardian] Automatic save skipped "
                             "for '%s' because it is not visiting a file")
                     (buffer-name))))

                 ;; Disk check: Missing parent directory.
                 ;; Skip saving to avoid Emacs prompting interactively
                 ;; to create the missing directory.
                 ((and file-dir (not (file-directory-p file-dir)))
                  (when buffer-guardian-verbose
                    (message
                     (concat "[buffer-guardian] Warning: Automatic "
                             "save skipped for '%s' because the parent "
                             "directory does not exist.")
                     file-name)))

                 ((not (file-writable-p file-name))
                  (when buffer-guardian-verbose
                    (message
                     (concat "[buffer-guardian] Warning: Automatic save "
                             "skipped for '%s' because the file is not "
                             "writable.")
                     file-name)))

                 ((not (verify-visited-file-modtime (current-buffer)))
                  (when buffer-guardian-verbose
                    (message (concat "[buffer-guardian] Warning: Automatic "
                                     "save skipped for '%s' because the file "
                                     "was modified externally.")
                             file-name)))

                 (t
                  (condition-case err
                      (progn
                        (let ((inhibit-interaction t)
                              (inhibit-message (not buffer-guardian-verbose))
                              (save-silently (not buffer-guardian-verbose)))
                          (ignore inhibit-interaction)
                          (save-buffer))
                        (when buffer-guardian-verbose
                          (message "[buffer-guardian] Save: '%s'"
                                   file-name)))
                    (inhibited-interaction
                     (message
                      (concat
                       "[buffer-guardian] Error: 'save-buffer' attempted an "
                       "interactive prompt in buffer '%s'. It is expected to "
                       "be non-interactive. Please report this "
                       "issue to the `buffer-guardian' author.")
                      (buffer-name)))
                    (error
                     (when buffer-guardian-verbose
                       (message "[buffer-guardian] Failed to save '%s': %s"
                                (buffer-name)
                                (error-message-string err))))))))))))))))

;;;###autoload
(defun buffer-guardian-save-all-buffers (&optional buffer-list)
  "Save all modified buffers that are visiting files that exist on the disk.
BUFFER-LIST is the list of buffers."
  (when (bound-and-true-p buffer-guardian-mode)
    (dolist (buffer (or buffer-list (buffer-list)))
      (when (and (buffer-live-p buffer) (buffer-modified-p buffer))
        (buffer-guardian-save-buffer-maybe buffer)))))

;;; Mode

;;;###autoload
(define-minor-mode buffer-guardian-mode
  "Toggle `buffer-guardian-mode'."
  :global t
  :lighter " BGuardian"
  :group 'buffer-guardian
  (let ((settings '(buffer-guardian-save-on-focus-loss
                    buffer-guardian-save-on-minibuffer-setup
                    buffer-guardian-save-on-buffer-switch
                    buffer-guardian-save-on-window-selection-change
                    buffer-guardian-save-on-window-configuration-change
                    buffer-guardian-save-all-buffers-interval
                    buffer-guardian-save-all-buffers-idle
                    buffer-guardian-save-all-buffers-trigger-hooks
                    buffer-guardian-save-trigger-functions)))
    (dolist (setting settings)
      (funcall (or (get setting 'custom-set) #'set-default)
               setting (symbol-value setting)))))

(provide 'buffer-guardian)

;;; buffer-guardian.el ends here
