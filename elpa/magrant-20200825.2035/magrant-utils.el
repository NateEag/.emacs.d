;;; magrant-utils.el --- Additional utils for magrant  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Jordan Besly

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Util functions for package `magrant'.
;;
;; Some of them are built on top of the most primitive functions in magrant-core.el


;;; Code:



;; REQUIRES

(require 's)
(require 'dash)
(require 'tramp)
(require 'tablist)
(require 'transient)

(require 'magrant-core)



;; UTILS: BUFFERS

(defun magrant-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))



;; UTILS: TABLIST / TRANSIENT

(defun magrant-utils-tablist-entry-id (entry)
  "Get the id of a tablist ENTRY."
  (car entry))

(defun magrant-utils-get-marked-items-ids ()
  "Get the id part of `tablist-get-marked-items'."
  (-map #'magrant-utils-tablist-entry-id (tablist-get-marked-items)))

(defun magrant-utils-ensure-items ()
  "Ensure at least one item is selected."
  (unless (magrant-utils-get-marked-items-ids)
    (user-error "This action cannot be used on an empty list")))

(defmacro magrant-utils-transient-define-prefix (name arglist &rest args)
  "Macro for building transient.el command NAME taking ARGLIST and ARGS content.
To use for commands that can target multiple entries at once from tabulated-list"
  (declare (indent 1)
           (debug t)
           (doc-string 3))
  `(transient-define-prefix ,name ,arglist
     ,@args
     (interactive)
     (magrant-utils-ensure-items)
     (transient-setup ',name)))

(put 'magrant-utils-transient-define-prefix 'lisp-indent-function 'defun)

(defun magrant-utils-generic-actions-heading ()
  "Generate heading for transient.el command that can target multiple entries at once from tabulated-list."
  (let ((items (s-join ", " (magrant-utils-get-marked-items-ids))))
    (format "%s %s"
            (propertize "Actions on" 'face 'transient-heading)
            (propertize items        'face 'transient-value))))

(defun magrant-utils-get-transient-action ()
  "Automagically get the current vagrant action from the transient name.
The vagrant action is the first command line argument used when calling vagrant."
  (s-replace "-" " " (s-chop-prefix "magrant-" (symbol-name transient-current-command))))

(defun magrant-utils-generic-action (action args)
  "Perform ACTION on selected items with additional transient ARGS.
When called interactively, transient action and args are
retrieved automagically accorind to the current transient name
and the active transient args."
  (interactive (list (magrant-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (magrant-utils-get-marked-items-ids)
    (magrant-run-vagrant action args it))
  (tablist-revert))

;; REVIEW: transient.el might provide this in a more straightforward way?
(defun magrant-utils-generic-action-with-item-prefix (name-arg-prefix &optional action args)
  "Perform ACTION on selected items with additional transient ARGS.
NAME-ARG-PREFIX is an additional CLI arg with the item name as its value."
  (setq action (or action (magrant-utils-get-transient-action))
        args (or args (transient-args transient-current-command)))
  (--each (magrant-utils-get-marked-items-ids)
    (magrant-run-vagrant action args (concat name-arg-prefix it)))
  (tablist-revert))



;; FONT LOCK

(defconst magrant-utils-transient-font-lock-keywords
  (eval-when-compile
    `((,(concat "("
                (regexp-opt (list "magrant-utils-transient-define-prefix")
                            t)
                "\\_>[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))))
  "Additional Elisp font-lock definitions.")

(font-lock-add-keywords 'emacs-lisp-mode magrant-utils-transient-font-lock-keywords)




(provide 'magrant-utils)

;;; magrant-utils.el ends here
