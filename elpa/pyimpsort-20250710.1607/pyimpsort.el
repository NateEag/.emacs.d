;;; pyimpsort.el --- Sort Python imports  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mario Rodas
;; Copyright (C) 2025 The pyimpsort contributors

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Maintainer: Alain Delplanque <alaindelplanque@mailoo.org>
;; URL: https://github.com/emacsorphanage/pyimpsort
;; Package-Version: 20250710.1607
;; Package-Revision: 70d739c134d8
;; Package-Requires: ((emacs "24.3") (python))
;; Keywords: tools, python, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; The companion script `pyimpsort.py` is distributed under the MIT License.
;; See the header of that file for details.

;;; Commentary:
;;
;; `pyimpsort.el' sort the python imports of a file.
;; Currently uses [pyimpsort.py](pyimpsort.py) to process the way to sort python
;; imports.
;;
;;; Setup:
;;
;; Add the following snippet to your `init.el':
;;
;;     (require 'pyimpsort)
;;
;; To run `pyimpsort-buffer' manually, you can bind it to a key in `python-mode':
;;
;;     (eval-after-load 'python
;;       '(define-key python-mode-map (kbd "C-c C-u") #'pyimpsort-buffer))
;;
;; To run `pyimpsort-buffer' automatically before saving a Python file:
;;
;;     (add-hook 'python-mode-hook
;;               (lambda ()
;;                 (add-hook 'before-save-hook #'pyimpsort-buffer nil t)))
;;
;;; Configuration:
;;
;; By default, `pyimpsort.el' looks for the `pyimpsort.py` script in the same
;; directory as this file and constructs a shell command using the `python3`
;; executable found in the system or in the configured virtual environment
;; (see `python-shell-virtualenv-root').
;;
;; Alternatively, you can install `pyimpsort` via PyPI and configure the command
;; as explained above.
;;
;; These variables can be customized:
;;
;; - `pyimpsort-script'
;;   Absolute path to the `pyimpsort.py` script.  By default, it is resolved
;;   relative to the location of this file.  You can override it like so:
;;
;;       (setq pyimpsort-script "/my/custom/path/to/pyimpsort.py")
;;
;; - `pyimpsort-command'
;;   Full shell command used to call the script.  By default, it launches the
;;   `pyimpsort-script' using the Python interpreter configured in Emacs
;;   (`python-shell-interpreter').  Useful when calling the script inside a
;;   container or custom environment.  Example:
;;
;;       (setq pyimpsort-command
;;             "docker exec -i my-container python3 -m pyimpsort")
;;
;; - `pyimpsort-group-module-import'
;;   If non-nil, group multiple imports from the same module into a single
;;   statement. This adds the `--group` option to the command line.
;;
;;       (setq pyimpsort-group-module-import t)
;;
;; - `pyimpsort-group-platform-site'
;;   If non-nil, group platform-level site-packages (e.g. system-wide
;;   installations) separately from other third-party imports. These are
;;   placed before third-party group. This adds the `--site` option to the
;;   command line.
;;
;;       (setq pyimpsort-group-platform-site t)
;;
;; - `pyimpsort-local-import'
;;   A list of module names to arbitrarily place in the "local" import group.
;;   If left nil, the local module will be inferred by walking up directories
;;   containing `__init__.py` and returning the top-level package name.
;;
;;       (setq pyimpsort-local-import '("myproject"))
;;
;; You can also configure this per project using `.dir-locals.el`:
;;
;;    ((python-mode
;;      . ((pyimpsort-command . "docker exec -i my-container python3 -m pyimpsort")
;;         (pyimpsort-group-module-import . t)
;;         (pyimpsort-group-platform-site . t)
;;         (setq pyimpsort-local-import . ("myproject")))))
;;
;;; Troubleshooting:
;;
;; + **Doesn't sort correctly third party libraries**
;;
;;   `pyimpsort.el' tries to identify the third party libraries if are installed
;;   in in the PYTHONPATH, if a package is not installed it is assumed that
;;   belongs to the application.
;;   `pyimpsort.el' also tries to identify if a python virtualenv
;;   is activated.
;;
;;; Related projects:
;;
;; + [isort][] ([emacs integration](https://github.com/paetzke/py-isort.el))
;;
;; [isort]: https://github.com/timothycrosley/isort
;;
;;; ChangeLog:
;;
;; Version 0.1.1 (2025-07-10)
;;
;; - Minor fixes
;;
;; Version 0.1.0 (2025-07-10)
;;
;; - Added: `pyimpsort-local-import` — force specific modules to be treated as
;;   local
;; - Added: `pyimpsort-group-platform-site` — separate platform site-packages
;;   from other third-party imports
;; - Added: `pyimpsort-group-module-import` — group multiple imports from the
;;   same module into a single statement
;; - Fixed: improved detection of multi-line import blocks
;; - Fixed: preserve comments adjacent to import statements when sorting

;;; Code:

(require 'python)

(defgroup pyimpsort nil
  "Sort python imports."
  :prefix "pyimpsort-"
  :group 'applications)

(defcustom pyimpsort-display-error-buffer t
  "Display error buffer on error."
  :type 'boolean
  :group 'pyimpsort)

(defcustom pyimpsort-error-buffer-name "*pyimpsort-error*"
  "Buffer name of pyimpsort error."
  :type 'string
  :group 'pyimpsort)

(defcustom pyimpsort-script "pyimpsort.py"
  "Path to the `pyimpsort.py' script.

This script is used by default when `pyimpsort-command' is not customized.
If relative, it is resolved with respect to the location of the Emacs Lisp file."
  :type 'string
  :group 'pyimpsort)

(defcustom pyimpsort-command nil
  "Shell command used to launch `pyimpsort`.

If you override this value, `pyimpsort-script' will be ignored.
Use this if you run Python in a non-standard environment, such as a container:

    docker exec -i my-dev-container sudo -u my-dev-user python3 -m pyimpsort"
  :type 'string
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-command)

(defcustom pyimpsort-group-module-import nil
  "Group multiple imports from the same module into a single statement.

If non-nil, consecutive imports from the same module will be merged.
For example, instead of:
    from os import path
    from os import remove

You get:
    from os import path, remove"
  :type 'boolean
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-group-module-import)

(defcustom pyimpsort-group-platform-site nil
  "Group platform site-packages separately from other third-party imports.

When non-nil, modules found in the platform's site-packages directory
\(e.g., system-level installations) are grouped separately from other
third-party modules such as those installed in the user base.

This group is placed before other third-party imports."
  :type 'boolean
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-group-platform-site)

(defcustom pyimpsort-local-import nil
  "List of module names to treat as local imports.

Modules listed here are considered local to the project and will be grouped
accordingly, rather than as third-party or system imports.

If set to nil, the local module name is inferred by traversing the directory
tree upwards from the current file, stopping at the highest-level directory
that still contains an '__init__.py' file.  The name of that top-level directory
is used as the local module."
  :type 'list
  :group 'pyimpsort)

(make-variable-buffer-local 'pyimpsort-local-import)

(defconst pyimpsort-script-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Répertoire d'installation de `pyimpsort.el`.")

(defun pyimpsort--search-import-bounds ()
  "Return the bounds (BEGIN . END) of the top-level contiguous import block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\(from\\|import\\) " nil t)
      (beginning-of-line)
      (let ((start (point))
            (end (point)))
        (while (progn
                 (while (when (looking-at-p "^[[:blank:]]*\\(#[^\n]*\\)?$")
                          (let ((cur (point)))
                            (forward-line)
                            (/= cur (point)))))
                 (when (looking-at-p "^\\(from\\|import\\) ")
                   (python-nav-end-of-statement)
                   (forward-line)
                   (setq end (point)))))
        (cons start end)))))

(defun pyimpsort--get-local ()
  "Return the top-level package name by walking up directories with __init__.py."
  (let ((filename (buffer-file-name)))
    (when filename
      (let ((dir (file-name-directory filename)))
        (when (file-exists-p (concat dir "__init__.py"))
          (while (let ((parent (file-name-directory (directory-file-name dir))))
                   (when (and (not (equal parent "/"))
                            (file-exists-p (concat parent "__init__.py")))
                     (setq dir parent))))
          (file-name-base (directory-file-name dir)))))))

(defun pyimpsort--get-command ()
  "Return the shell command to run pyimpsort."
  (or pyimpsort-command
      (let* ((exec-path (python-shell-calculate-exec-path))
             (python-exec (executable-find "python3"))
             (script (when pyimpsort-script
                       (let ((fullpath (if (file-name-absolute-p pyimpsort-script)
                                           pyimpsort-script
                                         (expand-file-name pyimpsort-script
                                                           pyimpsort-script-path))))
                         (when (file-exists-p fullpath)
                           fullpath)))))
        (if (and python-exec script)
            (mapconcat #'shell-quote-argument (list python-exec script) " ")
          (user-error "Cannot find Python interpreter or the pyimpsort.py script")))))

;;;###autoload
(defun pyimpsort-region (begin end)
  "Sort python imports from region BEGIN to END points."
  (interactive "r")
  (let ((command (pyimpsort--get-command))
        (err-buf (get-buffer pyimpsort-error-buffer-name)))
    (unless err-buf
      (setq err-buf (get-buffer-create pyimpsort-error-buffer-name))
      (with-current-buffer err-buf
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "q") #'quit-window)
          (use-local-map map))))
    (with-current-buffer err-buf
      (setq buffer-read-only nil))
    (when pyimpsort-group-module-import
      (setq command (concat command " --group")))
    (when pyimpsort-group-platform-site
      (setq command (concat command " --site")))
    (if pyimpsort-local-import
        (setq command (concat command " "
                              (mapconcat (lambda (x) (format "--local %s" x))
                                         pyimpsort-local-import " ")))
      (let ((local (pyimpsort--get-local)))
        (when local
          (setq command (concat command " --local " (shell-quote-argument local))))))
    (atomic-change-group
      (let ((exit-code (shell-command-on-region begin end command nil 'replace err-buf)))
        (with-current-buffer err-buf
          (goto-char (point-max))
          (setq buffer-read-only t))
        (when (not (zerop exit-code))
          (when pyimpsort-display-error-buffer
            (pop-to-buffer err-buf))
          (user-error "Command '%s' failed.  See buffer '%s' for details"
                      command pyimpsort-error-buffer-name))))))

;;;###autoload
(defun pyimpsort-buffer ()
  "Sort python imports from current buffer."
  (interactive)
  (let ((bounds (pyimpsort--search-import-bounds)))
    (when bounds
      (pyimpsort-region (car bounds) (cdr bounds)))))

(provide 'pyimpsort)

;;; pyimpsort.el ends here
