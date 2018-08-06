;;; pyimpsort.el --- Sort python imports. -*- lexical-binding: t -*-

;; Copyright © 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/pyimpsort.el
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

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

;;; Commentary:
;;
;; `pyimpsort.el' sort the python imports of a file.
;; Currently uses [pyimpsort.py](pyimpsort.py) to process the way to sort python
;; imports.
;;
;;; Setup
;; Add the following snippet to your `init.el':
;;
;;     (require 'pyimpsort)
;;     (eval-after-load 'python
;;       '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer))
;;
;;; Troubleshooting:
;; + **Doesn't sort correctly third party libraries**
;;
;;   `pyimpsort.el' tries to identify the third party libraries if are installed
;;   in in the PYTHONPATH, if a package is not installed it is assumed that
;;   belongs to the application.
;;   `pyimpsort.el' also tries to identify if a python virtualenv
;;   is activated.
;;
;;; Related projects:
;; + [isort][] ([emacs integration](https://github.com/paetzke/py-isort.el))
;;
;; [isort]: https://github.com/timothycrosley/isort

;;; Code:

(require 'python)

(defgroup pyimpsort nil
  "Sort python imports."
  :prefix "pyimpsort-"
  :group 'applications)

(defcustom pyimpsort-display-error-buffer nil
  "Display error buffer on error."
  :type 'boolean
  :group 'pyimpsort)

(defcustom pyimpsort-error-buffer-name "*pyimpsort-error*"
  "Buffer name of pyimpsort error."
  :type 'string
  :group 'pyimpsort)

(defconst pyimpsort-script
  (expand-file-name "pyimpsort.py"
                    (file-name-directory (if load-in-progress
                                             load-file-name
                                           (buffer-file-name))))
  "Absolute path of python pyimpsort.py script.")

(defconst pyimpsort-imports-start-regexp
  (rx (group (and bol (or "import" "from")))))

(defconst pyimpsort-imports-end-regexp
  (rx (or (and bol (or "import" "from")) (and bol (* space) eol))))

(defun pyimpsort--search-beg-point (&optional end)
  "Search the first import line until reach the END point."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward pyimpsort-imports-start-regexp end t)
         (match-beginning 1))))

(defun pyimpsort--search-end-point (begin)
  "Search the last import line starting from BEGIN point."
  (let (end)
    (save-excursion
      (goto-char begin)
      (goto-char (point-at-bol))
      (catch 'eof
        (while (re-search-forward pyimpsort-imports-end-regexp (point-at-eol) t)
          (when (eobp)
            (throw 'eof "End of file."))
          (setq end (point-at-eol))
          (forward-line 1))))
    end))

;;;###autoload
(defun pyimpsort-region (begin end)
  "Sort python imports from region BEGIN to END points."
  (interactive "r")
  (let* ((exec-path (python-shell-calculate-exec-path))
         (python-executable (executable-find python-shell-interpreter))
         (command (format "%s %s" python-executable pyimpsort-script)))
    (atomic-change-group
      (or (zerop (shell-command-on-region begin end command nil 'replace pyimpsort-error-buffer-name pyimpsort-display-error-buffer))
          (error "Command exited abnormally.  See %s for details" pyimpsort-error-buffer-name)))))

;;;###autoload
(defun pyimpsort-buffer ()
  "Sort python imports from current buffer."
  (interactive)
  (let* ((begin (pyimpsort--search-beg-point))
         (end (and begin (pyimpsort--search-end-point begin))))
    (when (and begin end)
      (pyimpsort-region begin end))))

(provide 'pyimpsort)

;;; pyimpsort.el ends here
