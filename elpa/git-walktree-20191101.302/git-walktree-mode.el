;;; git-walktree-mode.el --- Major-mode and minor-mode for git-walktree   -*- lexical-binding: t; -*-

;; Author: 10sr <8.slashes [at] gmail [dot] com>
;; Version: 0
;; URL: https://github.com/10sr/git-walktree-el
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))

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

;; Major-mode and minor-mode for git-walktree buffer.

;; git-walktree-mode: Major-mode for git-walktree tree buffer
;; git-walktree-blob-mode: Minor-mode for git-walktree blob buffer


;;; Code:

(require 'cl-lib)

(require 'git-walktree-utils)

;; These variables are defined in git-walktree.el
(defvar git-walktree-current-commitish)
(defvar git-walktree-current-path)
(defvar git-walktree-object-full-sha1)
(defvar git-walktree-repository-root)

(declare-function git-walktree--open-noselect
                  "git-walktree")

;; TODO: Move definition to another file?
(defun git-walktree-checkout-blob (object dest)
  "Checkout OBJECT into path DEST.
This function overwrites DEST without asking."
  (let ((status (call-process git-walktree-git-executable
                              nil  ; INFILE
                              (list :file dest)  ; DESTINATION
                              nil  ; DISPLAY
                              "cat-file"  ; ARGS
                              "-p"
                              object)))
    (unless (eq status 0)
      (error "Checkout failed"))))

;; git read-tree --prefix=rescue --index-output=idx 2f9912a
;; GIT_INDEX_FILE=idx git checkout-index -a
;; Or
;; GIT_INDEX_FILE=idx git read-tree 2f9912a
;; GIT_INDEX_FILE=idx git checkout-index -a --prefix=rescue/  # Require last slash
(defun git-walktree-checkout-tree (treeish dest)
  "Checkout TREEISH into path DEST.
When DIST is an existing directory, its contents are overwritten without asking."
  (setq dest
        (expand-file-name dest))
  (cl-assert (not (file-regular-p dest)))
  (setq dest
        (file-name-as-directory dest))
  (with-temp-buffer
    (cd (git-walktree--git-plumbing "rev-parse" "--show-toplevel"))
    (let* ((process-environment (cl-copy-list process-environment))
           (gitdir (git-walktree--git-plumbing "rev-parse" "--absolute-git-dir"))
           (index-file (expand-file-name "tmp-git-walktree-checkout-tree.index" gitdir)))
      (setenv "GIT_INDEX_FILE" index-file)
      (unwind-protect
          (progn
            (git-walktree--git-plumbing "read-tree" treeish)
            (git-walktree--git-plumbing "checkout-index" "-a" "-f" "--prefix" dest))
        (delete-file index-file)))))


;; git-walktree-mode (major-mode)

(defun git-walktree-mode--move-point-to-file ()
  "Move point to file field of ls-tree output in current line.
This function do nothing when current line is not ls-tree output."
  (interactive)
  (save-match-data
    (when (save-excursion
            (goto-char (point-at-bol))
            (re-search-forward git-walktree-ls-tree-line-regexp
                               (point-at-eol) t))
      (goto-char (match-beginning 4)))))

(defun git-walktree-mode-next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines and move to file field if found.

For TRY-VSCROLL see doc of `move-line'."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move arg nil nil try-vscroll)
  (git-walktree-mode--move-point-to-file)
  )

(defun git-walktree-mode-previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines and move to file field if found.

For TRY-VSCROLL see doc of `move-line'."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (line-move (- arg) nil nil try-vscroll)
  (git-walktree-mode--move-point-to-file)
  )

(defun git-walktree-mode--get (&optional safe)
  "Get object entry info at current line.
This fucntion throws error If entry not available.

If optional arg SAFE is non-nil, do not error when no entry on current line,
instead return nil."
  (or (git-walktree--parse-lstree-line (buffer-substring-no-properties (point-at-bol)
                                                                       (point-at-eol)))
      (unless safe
        (error "No object entry on current line"))))


(defun git-walktree-mode-open-this ()
  "Open git object of current line."
  (interactive)
  (let ((info (git-walktree-mode--get)))
    (cl-assert info)
    (switch-to-buffer
     (if (string= (plist-get info
                             :type)
                  "commit")
         ;; For submodule cd to that directory and intialize
         (if (yes-or-no-p "Switch to submodule repository?")
             (with-temp-buffer
               (let* ((path git-walktree-repository-root)
                      (path (expand-file-name git-walktree-current-path
                                              path))
                      (path (expand-file-name (plist-get info :file)
                                              path)))
                 (cd path))
               (git-walktree--open-noselect (plist-get info
                                                       :object)
                                            nil
                                            (plist-get info
                                                       :object)))
           (message "Canceld by user"))
       (git-walktree--open-noselect git-walktree-current-commitish
                                    (git-walktree--join-path (plist-get info
                                                                        :file)
                                                             git-walktree-current-path)
                                    (plist-get info
                                               :object))))))

(defalias 'git-walktree-mode-goto-revision
  'git-walktree-open)

(defun git-walktree-mode--revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `git-walktree' buffer.

Git-walktree sets `revert-buffer-function' to this function.  The args
_IGNORE-AUTO and _NOCONFIRM, passed from `revert-buffer', are ignored."
  ;; TODO: Do nothing when current commitish is a full-sha1 object
  (git-walktree--open-noselect git-walktree-current-commitish
                               git-walktree-current-path
                               nil
                               (current-buffer)))

(cl-defun git-walktree-mode-checkout-to ()
  "Checkout blob or tree at point.

Ask user for path to checkout."
  (declare (interactive-only (git-walktree-checkout-blob
                              git-walktree-checkout-tree)))
  (interactive)

  (let (path type object)
    (let ((info (git-walktree-mode--get t)))
      (if info
          (setq path (git-walktree--join-path (plist-get info :file)
                                              git-walktree-current-path)
                type (plist-get info :type)
                object (plist-get info :object))
        (setq path git-walktree-current-path
              type "tree"
              object git-walktree-object-full-sha1)))

    (pcase type

      ("blob"
       (let* ((default (expand-file-name path
                                         git-walktree-repository-root))
              (dest (read-file-name "Checkout to blob to: "
                                    default
                                    default))
              (dest (expand-file-name dest)))
         ;; When DEST is a directory append the name to DEST
         (when (file-directory-p dest)
           (let ((name (file-name-nondirectory path)))
             (setq dest (expand-file-name name dest))))
         (when (and (file-exists-p dest)
                    (not (yes-or-no-p (format "Overwrite `%s'? " dest))))
           (message "Canceled by user")
           (cl-return-from git-walktree-mode-checkout-to))
         (git-walktree-checkout-blob object dest)
         (message "%s (%s) checked out to %s"
                  path
                  object
                  dest)))

      ("tree"
       (let* ((default (expand-file-name path
                                         git-walktree-repository-root))
              (dest (read-file-name "Checkout to tree to: "
                                    default
                                    default))
              (dest (expand-file-name dest)))
         (when (file-regular-p dest)
           (error "Cannot checkout tree object to a file"))
         (when (and (file-directory-p dest)
                    (not (yes-or-no-p (format "Overwrite content of `%s'? " dest))))
           (message "Canceled by user")
           (cl-return-from git-walktree-mode-checkout-to))
         (git-walktree-checkout-tree object dest)
         (message "%s (%s) checked out to %s"
                  path
                  object
                  dest)))

      (_
       (error "Cannot checkout this object")))))

(defgroup git-walktree-faces nil
  "Faces used by git-walktree."
  :group 'git-walktree
  :group 'faces)

(defface git-walktree-tree-face
  ;; Same as dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for tree objects."
  :group 'git-walktree-faces)
(defface git-walktree-commit-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for commit objects."
  :group 'git-walktree-faces)
(defface git-walktree-symlink-face
  ;; Same as dired-symlink face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symlink objects."
  :group 'git-walktree-faces)


(defvar git-walktree-mode-font-lock-keywords
  `(
    (,git-walktree-ls-tree-line-regexp
     . (
        (1 'shadow)
        (3 'shadow)
        ))
    (,git-walktree-ls-tree-line-tree-regexp
     . (
        (2 'git-walktree-tree-face)
        (4 'git-walktree-tree-face)
        ))
    (,git-walktree-ls-tree-line-commit-regexp
     . (
        (2 'git-walktree-commit-face)
        (4 'git-walktree-commit-face)
        ))
    (,git-walktree-ls-tree-line-symlink-regexp
     . (
        (2 'git-walktree-symlink-face)
        (4 'git-walktree-symlink-face)
        ))
    )
  "Syntax highlighting for `git-walktree' mode.")

(defvar git-walktree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'git-walktree-mode-next-line)
    (define-key map "p" 'git-walktree-mode-previous-line)
    (define-key map (kbd "C-n") 'git-walktree-mode-next-line)
    (define-key map (kbd "C-p") 'git-walktree-mode-previous-line)
    (define-key map "P" 'git-walktree-parent-revision)
    (define-key map "N" 'git-walktree-known-child-revision)
    (define-key map "^" 'git-walktree-up)
    (define-key map "G" 'git-walktree-mode-goto-revision)
    (define-key map (kbd "C-m") 'git-walktree-mode-open-this)
    (define-key map "C" 'git-walktree-mode-checkout-to)
    map))

(define-derived-mode git-walktree-mode special-mode "GitWalktree"
  "Major-mode for `git-walktree-open'."
  (setq-local revert-buffer-function
              'git-walktree-mode--revert-buffer)
  (setq-local font-lock-defaults
              '(git-walktree-mode-font-lock-keywords
                nil nil nil nil
                ))
  )

;; git-walktree-blob-mode (minor-mode)

(cl-defun git-walktree-blob-mode-checkout-to ()
  "Checkout current blob into the working directory.

Ask user for path to checkout."
  (declare (interactive-only git-walktree-checkout-blob))
  (interactive)
  (let ((dest nil))
    (setq dest
          (let ((current-path (expand-file-name git-walktree-current-path
                                                git-walktree-repository-root)))
            (read-file-name "Checkout to: "
                            current-path
                            current-path)))
    (setq dest
          (expand-file-name dest))

    ;; When DEST is a directory append the name to DEST
    (when (file-directory-p dest)
      (let ((name (file-name-nondirectory git-walktree-current-path)))
        (setq dest (expand-file-name name dest))))

    (let ((obj git-walktree-object-full-sha1))
      (cl-assert obj)
      (when (and (file-exists-p dest)
                 (not (yes-or-no-p (format "Overwrite `%s'? " dest))))
        (message "Canceled by user")
        (cl-return-from git-walktree-blob-mode-checkout-to))
      (git-walktree-checkout-blob obj dest)
      (message "%s checked out to %s"
               obj
               dest))))

(defvar git-walktree-blob-mode-map
  (let ((map (make-sparse-keymap)))
    ;; C overwrite view-mode keybind `view-kill-and-leave'
    (define-key map "C" 'git-walktree-blob-mode-checkout-to)
    (define-key map "P" 'git-walktree-parent-revision)
    (define-key map "N" 'git-walktree-known-child-revision)
    (define-key map "^" 'git-walktree-up)
    (define-key map "G" 'git-walktree-mode-goto-revision)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for `git-walktree-blob-mode'.")

(defvar git-walktree-blob-mode-overriding-map-alist
  (list
   (cons 'view-mode
         (let ((map (make-sparse-keymap)))
           (define-key map "C" 'git-walktree-blob-mode-checkout-to)
           map)))
  "Set `minor-mode-overriding-map-alist'.")

(define-minor-mode git-walktree-blob-mode
  "Minor-mode for git-walktree blob buffer."
  :lighter " GitWalktree"
  (setq-local revert-buffer-function
              'git-walktree-mode--revert-buffer)
  )


(provide 'git-walktree-mode)

;;; git-walktree-mode.el ends here
