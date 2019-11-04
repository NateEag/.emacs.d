;;; git-walktree-utils.el --- Utilities for git-walktree   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; Version: 0
;; URL: https://github.com/10sr/git-walktree-el
;; Package-Requires: ((emacs "26.1"))

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

;; Utilitiies for git-walktree.


;;; Code:

;; Rule: This file only includes definitions of functions that:
;;   - Do not depends on git-walktree functions and variables defined outside of this file
;;   - Do not create public buffers
;;   - Do not modify states of existing buffers
;;   - Do not modify variables that can be accessed from outside of this file
;; This file also has some variables.

(defcustom git-walktree-git-executable "git"
  "Git executable."
  :type 'string
  :group 'git-walktree)

(defun git-walktree--git-plumbing (&rest args)
  "Run git plubming command with ARGS.
Returns first line of output without newline."
  (with-temp-buffer
    (let ((status (apply 'call-process
                         git-walktree-git-executable
                         nil
                         t
                         nil
                         args)))
      (unless (eq 0
                  status)
        (error "Faild to run git %S:\n%s"
               args
               (buffer-substring-no-properties (point-min)
                                               (point-max))))
      (buffer-substring-no-properties (point-min)
                                      (progn
                                        (goto-char (point-min))
                                        (point-at-eol))))))
;; (git-walktree--git-plumbing "cat-file" "-t" "HEAD")

(defun git-walktree--assert-resolved-type (obj types)
  "Assert if OBJ is one of TYPES.

This function always resolves OBJ first using ^{} syntax and then
check type, so OBJ will never be detected as one of \"tag\" type.
This function returns the string of type of OBJ."
  (cl-assert obj)
  (cl-assert types)
  (let* ((resolved (git-walktree--rev-parse-deref-tags obj))
         (type (git-walktree--git-plumbing "cat-file"
                                           "-t"
                                           resolved)))
    (cl-assert (member type types)
               nil
               (format "Expected type of %s (%s) is one of %S, but actually %s"
                       resolved obj types type)
               )
    type)
  )

(defun git-walktree--commitish-fordisplay (commitish)
  "Convert COMMITISH and return is a suitable format for displaying."
  (if (and commitish
           (string-match-p "\\`[0-9a-f]+\\'"
                           commitish)
           (>= (length commitish) 32))
      (git-walktree--git-plumbing "rev-parse"
                                  "--short"
                                  commitish)
    commitish))

(defun git-walktree--rev-parse-deref-tags (obj)
  "Resolve OBJ using git rev-parse and return full sha-1 name.

This function just execute git rev-parse with ^{} syntax."
  (git-walktree--git-plumbing "rev-parse"
                              (concat obj "^{}")))

(defun git-walktree--resolve-object (commitish path)
  "Return object full sha1 name of COMMITISIH:PATH.
If path is equal to \".\" return COMMITISH's root tree object.
PATH will be always treated as relative to repository root."
  (cl-assert commitish)
  (git-walktree--assert-path path)

  ;; commitish can be a tag
  (setq commitish (git-walktree--rev-parse-deref-tags commitish))
  (git-walktree--assert-resolved-type commitish
                                      '("commit"))

  (if (string= path ".")
      (git-walktree--git-plumbing "show"
                                  "--no-patch"
                                  "--pretty=format:%T"
                                  commitish)
    (let ((info (git-walktree--parse-lstree-line (git-walktree--git-plumbing "ls-tree"
                                                                             "--full-tree"
                                                                             commitish
                                                                             path))))
      (cl-assert info nil "Cannot resolve object of %s %s" commitish path)
      (plist-get info :object))))


(defun git-walktree--choose-commitish (prompt-format collection)
  "Emit PROMPT-FORMAT and ask user to which commitish of COLLECTION to use.
When collection has just one element, return the first element without asking."
  (cl-assert collection)
  (if (< (length collection) 2)
      (car collection)
    (completing-read (format prompt-format
                             (mapconcat 'git-walktree--commitish-fordisplay
                                        collection
                                        " "))
                     collection
                     nil
                     t)))


;; Paths in repository

(defun git-walktree--assert-path (path)
  "Assert that PATH is in valid format for use in `git-walktree'."
  (cl-assert path)
  (cl-assert (not (string-match-p "\\`/" path)))
  (cl-assert (not (string-match-p "/\\'" path)))
  )

(defun git-walktree--path-in-repository (fullpath)
  "Convert FULLPATH into relative path to repository root.
Result will not have leading and trailing slashes."
  (with-temp-buffer
    (cd (if (file-directory-p fullpath)
            fullpath
          (file-name-directory fullpath)))
    (let* ((root (git-walktree--git-plumbing "rev-parse"
                                             "--show-toplevel"))
           (path (file-relative-name (directory-file-name fullpath)
                                     root)))
      (git-walktree--assert-path path)
      path)))

(defun git-walktree--parent-directory (path)
  "Return parent directory of PATH without trailing slash.
For root directory return \".\".
If PATH is equal to \".\", return nil."
  (git-walktree--assert-path path)
  (if (string-match-p "/" path)
      (directory-file-name (file-name-directory path))
    (if (string= "." path)
        nil
      ".")))

(defun git-walktree--join-path (name base)
  "Make path from NAME and BASE.

Args are same as `expand-file-name'.  For example,

  (git-walktree--join-path \"foo\" \"bar/baz\") -> \"bar/baz/foo\""
(git-walktree--assert-path name)
(git-walktree--assert-path base)
(if (string= base ".")
    name
  (concat base "/" name)))


;; Parents and Children of commits

(defun git-walktree--parent-full-sha1 (commitish)
  "Return list of parent commits of COMMITISH in sha1 string."
  (git-walktree--assert-resolved-type commitish
                                      '("commit"))
  (setq commitish
        (git-walktree--rev-parse-deref-tags commitish))
  (let ((parents (git-walktree--git-plumbing "show"
                                             "--no-patch"
                                             "--pretty=format:%P"
                                             commitish)))
    (split-string parents)))

(defvar git-walktree-known-child-revisions (make-hash-table :test 'equal)
  "Hash of already known pair of commitid -> list of child commitid.
Both values should be object full sha1 names.")

(defun git-walktree--put-child (parent child)
  "Register PARENT and CHILD relationship.
PARENT should be a full sha1 object name."
  (setq parent
        (git-walktree--rev-parse-deref-tags parent))
  (let ((current (gethash parent git-walktree-known-child-revisions)))
    (unless (member child current)
      (puthash parent
               (cons child
                     current)
               git-walktree-known-child-revisions))))

;; TODO: Add aggressive search mode
;; https://stackoverflow.com/a/9870218
(defun git-walktree--get-children (parent)
  "Get known children list of PARENT commit.
PARENT should be a full sha1 object name."
  (gethash parent git-walktree-known-child-revisions))


;; ls-tree output parsing

(defconst git-walktree-ls-tree-line-regexp
  "^\\([0-9]\\{6\\}\\) \\(\\w+\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for one line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-tree-regexp
  "^\\([0-9]\\{6\\}\\) \\(tree\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for tree line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-commit-regexp
  "^\\([0-9]\\{6\\}\\) \\(commit\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for commit line of output of git ls-tree.")
(defconst git-walktree-ls-tree-line-symlink-regexp
  "^\\(120[0-9]\\{3\\}\\) \\(blob\\) \\([0-9a-f]+\\)\t\\(.*\\)$"
  "Regexp for symlink line of output of git ls-tree.")

(defun git-walktree--parse-lstree-line (str)
  "Extract object info from STR.

STR should be a string like following without newline.:

100644 blob 6fd4d58202d0b46547c6fe43de0f8c878456f966	.editorconfig

Returns property list like (:mode MODE :type TYPE :object OBJECT :file FILE)."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward git-walktree-ls-tree-line-regexp
                               nil
                               t)
        (list :mode (match-string 1)
              :type (match-string 2)
              :object (match-string 3)
              :file (match-string 4))))))

(provide 'git-walktree-utils)

;;; git-walktree-utils.el ends here
