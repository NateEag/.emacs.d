;;; git-walktree-read.el --- Read commitish from minibuffer   -*- lexical-binding: t; -*-

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

;; Read commitish from minibuffer.


;;; Code:

(require 'git)
(defvar git-repo)

(defvar git-walktree-read--history nil
  "History for `git-walktree--read'.")

(defun git-walktree-read--read (prompt)
  "Read branch, tag or commit with PROMPT.
This function is a fallback used when `magit-read-branch-or-commit' is
 not defined."
  (with-temp-buffer
    (setq-local git-repo default-directory)
    (or (completing-read prompt  ; PROMPT
                         (nconc (git-branches) (git-tags))  ; COLLECTION
                         nil  ; PREDICATE
                         nil  ; REQUIRE-MATCH
                         (or (thing-at-point 'symbol t)  ; INITIAL-INPUT
                             (git-on-branch))
                         'git-walktree-read--history  ; HISTORY
                         )
        (user-error "Nothing selected"))))

(if (and (require 'magit nil t)
         (require 'magit-git nil t))
    (fset 'git-walktree-read-branch-or-commit
          'magit-read-branch-or-commit)
  (fset 'git-walktree-read-branch-or-commit
        'git-walktree-read--read)
  )


(provide 'git-walktree-read)

;;; git-walktree-read.el ends here
