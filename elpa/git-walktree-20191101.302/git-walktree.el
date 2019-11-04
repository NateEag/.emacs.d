;;; git-walktree.el --- Browse Git tree and blob objects   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-walktree-el
;; Version: 0.1.0
;; Keywords: vc utility git
;; Package-Requires: ((emacs "26.1") (git "0.1.1") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `git-walktree' is a Git `ls-tree` browser.
;; It has following features:

;; - Browse tree objects of arbitrary revisions without checking them out
;;   - Go subtrees
;;   - Visit blob objects
;;   - Go Parent and child revisions
;; - Checkout (dump) tree or blob object to current working directory



;;; Code:

(require 'cl-lib)
(require 'ansi-color)

(require 'git-walktree-utils)
(require 'git-walktree-mode)
(require 'git-walktree-read)

(defgroup git-walktree nil
  "Git Walktree."
  :tag "GitWalktree"
  :prefix "git-walktree-"
  :group 'tools)

(defcustom git-walktree-dont-reuse-tree-buffer nil
  "Non-nil not to reuse buffer for treeish objects.

When set to nil, reuse one buffer for treeish objects of the same paths."
  :type 'boolean
  :group 'git-walktree)

(defcustom git-walktree-dont-reuse-blob-buffer nil
  "Non-nil not to reuse buffer for blob objects.

When set to nil, reuse one buffer for blob objects of
the same file names."
  :type 'boolean
  :group 'git-walktree)

(defcustom git-walktree-try-cd t
  "Try to cd if directory exists in current working directory if non-nil.
Otherwise use repository root for gitwalktree buffer's `default-directory'."
  :type 'boolean
  :group 'git-walktree)


;; See gitglossary(7) for git terminology
;; https://git-scm.com/docs/gitglossary

(defvar git-walktree-tree-buffers-for-reuse-hash (make-hash-table :test 'equal)
  "Buffer to use when `git-walktree-reuse-tree-buffer' is non-nil.")

(defvar git-walktree-blob-buffers-for-reuse-hash (make-hash-table :test 'equal)
  "Buffers to use when `git-walktree-reuse-blob-buffer' is non-ni.")

(defvar-local git-walktree-current-commitish nil
  "Commitish name of currently browsing.")
(put 'git-walktree-current-commitish
     'permanent-local
     t)

;; (defvar-local git-walktree-current-commit-full-sha1 nil
;;   "Object name in full sha1 format of current buffer.
;; This should be the result of git rev-parse `git-walktree-current-commitish'^{}."
;;   nil)
;; (put 'git-walktree-current-commit-full-sha1
;;      'permanent-local
;;      t)

(defvar-local git-walktree-current-path nil
  "Path name currently visiting without leading and trailing slash.
This path is always relative to repository root.")
(put 'git-walktree-current-path
     'permanent-local
     t)

(defvar-local git-walktree-buffer-file-name nil
  "Psudo filename of current buffer.")
(put 'git-walktree-buffer-file-name
     'permanent-local
     t)

(defvar-local git-walktree-object-full-sha1 nil
  "Object name in full sha1 format of current buffer.
This should be a blob or tree object name.")
(put 'git-walktree-object-full-sha1
     'permanent-local
     t)

(defvar-local git-walktree-repository-root nil
  "Repository root path of current buffer.")
(put 'git-walktree-repository-root
     'permanent-local
     t)

(defun git-walktree--get-create-blob-buffer (commitish path)
  "Create and return buffer for COMMITISH:PATH blob object."
  (git-walktree--assert-path path)
  (let* ((root (git-walktree--git-plumbing "rev-parse"
                                           "--show-toplevel"))
         (commitish-display (git-walktree--commitish-fordisplay commitish))
         (buffer-name (format "*Blob<%s:%s>*"
                              (or commitish-display "")
                              (file-name-nondirectory path)))
         (hash-key (expand-file-name path
                                     root))
         (buffer nil))

    (unless git-walktree-dont-reuse-blob-buffer
      (setq buffer
            (gethash hash-key
                     git-walktree-blob-buffers-for-reuse-hash)))

    ;; When buffer has been killed set buffer to nil
    (setq buffer (and buffer
                      (buffer-name buffer)
                      buffer))

    (when buffer
      (with-current-buffer buffer
        (cl-assert (string= root
                            git-walktree-repository-root))
        (unless (string= buffer-name
                         (buffer-name))
          (rename-buffer (generate-new-buffer-name
                          buffer-name)))))

    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        (setq git-walktree-repository-root
              root))
      (puthash hash-key
               buffer
               git-walktree-blob-buffers-for-reuse-hash))

    (cl-assert buffer)
    (cl-assert (buffer-name buffer))
    buffer))

(defun git-walktree--get-create-tree-buffer (commitish path)
  "Create and return buffer for COMMITISH:PATH tree object."
  (git-walktree--assert-path path)
  (let* ((root (git-walktree--git-plumbing "rev-parse"
                                           "--show-toplevel"))
         (commitish-display (git-walktree--commitish-fordisplay commitish))
         (buffer-name (format "*Tree<%s:%s>*"
                              (or commitish-display "")
                              (file-name-nondirectory path)))
         (hash-key (expand-file-name path
                                     root))
         (buffer nil))

    (unless git-walktree-dont-reuse-tree-buffer
      (setq buffer
            (gethash hash-key
                     git-walktree-tree-buffers-for-reuse-hash)))

    ;; When buffer has been killed set buffer to nil
    (setq buffer (and buffer
                      (buffer-name buffer)
                      buffer))

    (when buffer
      (with-current-buffer buffer
        (cl-assert (string= root
                            git-walktree-repository-root))
        (unless (string= buffer-name
                         (buffer-name))
          (rename-buffer (generate-new-buffer-name
                          buffer-name)))))

    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        (setq git-walktree-repository-root
              root))
      (puthash hash-key
               buffer
               git-walktree-tree-buffers-for-reuse-hash))

    (cl-assert buffer)
    (cl-assert (buffer-name buffer))
    buffer))


(defun git-walktree--replace-into-buffer (target)
  "Replace TARGET buffer contents with that of current buffer.
It also copy text overlays."
  (let ((src (current-buffer)))
    (with-current-buffer target
      (let ((overlays (overlays-in (point-min) (point-max))))
        (dolist (o overlays)
          (delete-overlay o)))
      (replace-buffer-contents src)))

  ;; Copy color overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (dolist (o overlays)
      (let ((beg (overlay-start o))
            (end (overlay-end o)))
        (move-overlay (copy-overlay o)
                      beg
                      end
                      target)))))

(defvar git-walktree-tree-buffer-show-command
  '("show"
    ;; "--no-patch"
    "--color=always"
    "--pretty=short"
    "--decorate"
    "--stat"
    )
  "Command used show current commit object in tree buffer.")

(defun git-walktree--load-treeish (commitish path treeish &optional buffer)
  "Open git tree buffer of COMMITISH:PATH.

TREEISH should be a tree-ish object full-sha1 of COMMITISH:PATH.

When BUFFER is non-nil, use that buffer instead of get or create buffer.
This is used for buffer reverting."
  (git-walktree--assert-path path)
  (cl-assert treeish)
  (setq buffer
        (or buffer
            (git-walktree--get-create-tree-buffer commitish path)))
  (git-walktree--assert-resolved-type treeish
                                      '("commit" "tree"))
  (let ((point-tree-start nil))
    (with-current-buffer buffer
      (unless (and (string= commitish
                            git-walktree-current-commitish)
                   (string= treeish
                            git-walktree-object-full-sha1))
        (buffer-disable-undo)
        ;; For running git command go back to repository root
        (cd git-walktree-repository-root)
        (save-excursion
          (let ((inhibit-read-only t))
            ;; Remove existing overlays generated by ansi-color-apply-on-region
            (remove-overlays)
            (with-temp-buffer
              (if commitish
                  (progn (apply 'git-walktree--call-process nil
                                `(,@git-walktree-tree-buffer-show-command
                                  ,commitish))
                         (ansi-color-apply-on-region (point-min)
                                                     (point))
                         (insert "\n")
                         (insert (format "Contents of '%s:%s':\n"
                                         (git-walktree--commitish-fordisplay commitish)
                                         path)))
                (insert (format "Contents of treeish object '%s:\n"
                                treeish)))
              (setq point-tree-start (point))
              (git-walktree--call-process nil
                                          "-c"
                                          "core.quotePath=false"
                                          "ls-tree"
                                          ;; "-r"
                                          "--abbrev"
                                          treeish)
              (git-walktree--replace-into-buffer buffer))
            ))
        (git-walktree-mode)
        (set-buffer-modified-p nil)

        (setq git-walktree-current-commitish commitish)
        (setq git-walktree-current-path path)
        (setq git-walktree-object-full-sha1 treeish)
        (let ((dir (expand-file-name path git-walktree-repository-root)))
          (when (and git-walktree-try-cd
                     (file-directory-p dir))
            (cd dir)))
        (when (= (point) (point-min))
          (goto-char point-tree-start)
          (git-walktree-mode--move-point-to-file)
          )
        ))
    buffer))

(defun git-walktree--call-process (&optional infile &rest args)
  "Call git command with input from INFILE and args ARGS.
Result will be inserted into current buffer."
  (let ((status (apply 'call-process
                       git-walktree-git-executable
                       infile
                       t
                       nil
                       args)))
    (unless (eq 0
                status)
      (error "Failed to call git process %S %S"
             infile
             args))))

(defun git-walktree--load-blob (commitish path blob &optional buffer)
  "Open blob object of COMMITISH:PATH.

BLOB should be a blob object full-sha1 of COMMITISH:PATH.

When BUFFER is non-nil, use that buffer instead of get or create buffer.
This is used for buffer reverting."
  (git-walktree--assert-path path)
  (cl-assert blob)
  (setq buffer
        (or buffer
            (git-walktree--get-create-blob-buffer commitish path)))
  (git-walktree--assert-resolved-type blob
                                      '("blob"))
  (with-current-buffer buffer
    (unless (and (string= git-walktree-current-commitish
                          commitish)
                 (string= git-walktree-current-path
                          path)
                 (string= git-walktree-object-full-sha1
                          blob))

      (unless (string= blob
                       git-walktree-object-full-sha1)
        ;; For running git command, go to repository root
        (cd git-walktree-repository-root)
        (let ((go-beginning-after-insert (eq (point-min)
                                             (point-max)))
              (inhibit-read-only t))
          (with-temp-buffer
            (git-walktree--call-process nil
                                        "cat-file"
                                        "-p"
                                        blob)
            (git-walktree--replace-into-buffer buffer))
          ;; When buffer was empty before insertion, set point to
          ;; beginning of buffer
          (when go-beginning-after-insert
            (goto-char (point-min))))
        (setq buffer-file-name
              (concat git-walktree-repository-root "/" path))
        (normal-mode t)
        ;; For asking filename when C-xC-s
        (setq buffer-file-name nil)
        (set-buffer-modified-p t)
        (setq git-walktree-object-full-sha1 blob)
        (setq buffer-read-only t)
        (git-walktree-blob-mode 1))

      (setq git-walktree-buffer-file-name
            (concat git-walktree-repository-root "/git@" commitish ":" path))

      (setq git-walktree-current-commitish commitish)
      (setq git-walktree-current-path path)
      (let ((dir (expand-file-name (or (file-name-directory path)
                                       ".")
                                   git-walktree-repository-root)))
        (when (and git-walktree-try-cd
                   (file-directory-p dir))
          (cd dir)))

      ))
  buffer)

(defun git-walktree--open-noselect-safe-path (commitish &optional path)
  "Open git object of COMMITISH:PATH.
If PATH not found in COMMITISH tree, go up path and try again until found.
When PATH is omitted or nil, it is calculated from current file or directory."
  (cl-assert commitish)
  (git-walktree--assert-resolved-type commitish
                                      '("commit"))

  (setq path
        (or path
            git-walktree-current-path
            (git-walktree--path-in-repository (or buffer-file-name
                                                  default-directory))))
  (git-walktree--assert-path path)

  (let ((obj (git-walktree--resolve-object commitish path)))
    (while (not obj)
      (setq path
            (git-walktree--parent-directory path))
      (setq obj
            (git-walktree--resolve-object commitish path)))
    (git-walktree--open-noselect commitish
                                 path
                                 obj)))

(defcustom git-walktree-describe-commitish t
  "When non-nil, tries to find tag or ref for current commitish.
Use command  git describe --all --always COMMITISH."
  :type 'boolean
  :group 'git-walktree)

;; TODO: Store view history
;; Or add variable like -previously-opened or -referer?
(defun git-walktree--open-noselect (commitish &optional path object buffer)
  "Open buffer to view git object of COMMITISH:PATH.
When PATH was given and non-nil open that, otherwise open root tree.
When OBJECT was given and non-nil, assume that is the object full sha1  of
COMMITISH:PATH without checking it.

When BUFFER is non-nil, use that buffer instead of get or create buffer.
This is used for buffer reverting."
  (cl-assert commitish)
  (setq commitish
        (if git-walktree-describe-commitish
            (git-walktree--git-plumbing "describe" "--all" "--always" commitish)
          (git-walktree--rev-parse-deref-tags commitish)))

  (git-walktree--assert-resolved-type commitish
                                      '("commit"))

  (setq path (or path
                 "."))
  (git-walktree--assert-path path)

  (setq object (or object
                   (git-walktree--resolve-object commitish path)))
  (setq object (git-walktree--rev-parse-deref-tags object))
  (cl-assert object)

  (let ((type (git-walktree--assert-resolved-type object
                                                  '("commit" "tree" "blob"))))
    (pcase type
      ((or "commit" "tree")
       (git-walktree--load-treeish commitish path object buffer))
      ("blob"
       (git-walktree--load-blob commitish path object buffer)))))


;;;###autoload
(defun git-walktree-open (commitish &optional path)
  "Open git tree buffer of COMMITISH.
When PATH was given and non-nil open that, otherwise try to open current path.
If target path is not found in COMMITISH tree, go up path and try again until found."
  (interactive (list (git-walktree-read-branch-or-commit "Revision: ")))
  (switch-to-buffer (git-walktree--open-noselect-safe-path commitish path)))
;;;###autoload
(defalias 'git-walktree 'git-walktree-open)

(defun git-walktree-up (&optional commitish path)
  "Open parent directory of COMMITISH and PATH.
@If not given, value of current buffer will be used."
  (interactive)
  (setq commitish
        (or commitish git-walktree-current-commitish))
  (setq path
        (or path git-walktree-current-path))
  (let ((parent (git-walktree--parent-directory path)))
    (if parent
        (switch-to-buffer (git-walktree--open-noselect commitish
                                                       parent
                                                       nil))
      (message "Cannot find parent directory for current tree."))))


(defun git-walktree-parent-revision ()
  "Open parent revision of current path.
If current path was not found in the parent revision try to go up path."
  (interactive)
  (cl-assert git-walktree-current-commitish)
  (let* ((commit-full-sha1 (git-walktree--rev-parse-deref-tags
                            git-walktree-current-commitish))
         (parents (git-walktree--parent-full-sha1 commit-full-sha1)))
    (dolist (parent parents)
      (git-walktree--put-child parent
                               commit-full-sha1))
    (if (< (length parents)
           1)
        (message "This revision has no parent revision")
      (let* ((parent (git-walktree--choose-commitish "This revision has multiple parents. Which to open? (%s) "
                                                     parents))
             (path git-walktree-current-path))
        (cl-assert path)
        (switch-to-buffer (git-walktree--open-noselect-safe-path parent
                                                                 path))))))


(defun git-walktree-known-child-revision ()
  "Open known revision of current path."
  (interactive)
  (let* ((commit-full-sha1 (git-walktree--rev-parse-deref-tags
                            git-walktree-current-commitish))
         (children (git-walktree--get-children commit-full-sha1)))
    (if (< (length children)
           1)
        (message "There are no known child revision")
      (let* ((child (git-walktree--choose-commitish "There are multiple known childrens. Which to open? (%s)"
                                                    children))
             (path git-walktree-current-path))
        (cl-assert path)
        (switch-to-buffer (git-walktree--open-noselect-safe-path child
                                                                 path))))))



(provide 'git-walktree)

;;; git-walktree.el ends here
