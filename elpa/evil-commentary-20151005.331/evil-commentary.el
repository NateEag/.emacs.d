;;; evil-commentary.el --- Comment stuff out. A port of vim-commentary.

;; Copyright (C) 2014 Quang Linh LE

;; Author: Quang Linh LE <linktohack@gmail.com>
;; URL: http://github.com/linktohack/evil-commentary
;; Package-Version: 20151005.331
;; Version: 1.0.2
;; Keywords: evil comment commentary evil-commentary
;; Package-Requires: ((evil "1.0.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-commentary
;;
;; evil-commentary is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; evil-commentary is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This program emulates evil-commentary initially developed by Tim Pope
;; (tpope) It help you comment line with `counts` and `motions`.

;;; Example:
;;
;; `gcc` to comment/uncomment a line
;; `5gcc` to comment/uncomment 5 lines
;; `gc$` to comment/uncomment from current position
;; `gcap` to comment/uncomment current paragraph


;;; Code:

(require 'evil)

(defgroup evil-commentary nil
  "Comment stuff out"
  :group 'evil
  :prefix "evil-commentary-")

(defcustom evil-commentary-comment-function-for-mode-alist
  '((f90-mode . f90-comment-region)
    (web-mode . web-mode-comment-or-uncomment-region))
  "Mode-specific comment function.

By default, `evil-commentary' use `comment-or-uncomment'
function. By specify an alist of modes here, the comment function
provided by major mode will be use instead.

A comment functions has to accept BEG, END as its required
parameter."
  :group 'evil-commentary)

(evil-define-operator evil-commentary (beg end type)
  "Comment or uncomment region that {motion} moves over."
  :move-point nil
  (interactive "<R>")
  ;; Special treatment for org-mode
  (cond ((and (fboundp 'org-in-src-block-p)
              (org-in-src-block-p))
         (let* ((current-line (line-number-at-pos))
                (top-line (save-excursion
                            (move-to-window-line 0)
                            (line-number-at-pos)))
                (offset (- current-line top-line)))
           (push-mark beg)
           (goto-char end)
           (setq mark-active t)
           (org-babel-do-in-edit-buffer
            (call-interactively 'evil-commentary))
           (evil-scroll-line-to-top (1+ current-line))
           (evil-scroll-line-up (1+ offset))))
        (t
         (let ((comment-function
                (cdr (assoc major-mode
                            evil-commentary-comment-function-for-mode-alist))))
           (if comment-function (funcall comment-function beg end)
             (comment-or-uncomment-region beg end))))))

(evil-define-operator evil-commentary-line (beg end type)
  "Comment or uncomment [count] lines."
  :motion evil-line
  :move-point nil
  (interactive "<R>")
  (when (evil-visual-state-p)
    (unless (memq type '(line block))
      (let ((range (evil-expand beg end 'line)))
        (setq beg (evil-range-beginning range)
              end (evil-range-end range)
              type (evil-type range))))
    (evil-exit-visual-state))
  (evil-commentary beg end type))

(evil-define-operator evil-commentary-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (evil-yank beg end type register yank-handler)
  (evil-commentary beg end))

(evil-define-operator evil-commentary-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (evil-yank-line beg end type register)
  (evil-commentary-line beg end))

;;;###autoload
(define-minor-mode evil-commentary-mode
  "Commentary mode."
  :lighter " s-/"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'normal map "gc" 'evil-commentary)
            (evil-define-key 'normal map "gy" 'evil-commentary-yank)
            (define-key map (kbd "s-/") 'evil-commentary-line)
            map))

;;;###autoload
(define-obsolete-variable-alias 'evil-commentary-default-setup
  'evil-commentary-mode "0.0.3")
;;;###autoload
(define-obsolete-function-alias 'evil-commentary-default-setup
  'evil-commentary-mode "0.0.3")

(provide 'evil-commentary)

;;; evil-commentary.el ends here
