;;; jedi.el --- a Python auto-completion for Emacs

;; Copyright (C) 2015 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Package-Requires: ((epc "0.1.0") (auto-complete "1.4") (python-environment "0.0.2"))
;; Version: 0.2.0alpha2

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'auto-complete)
(require 'jedi-common)

;;; AC source
(defun jedi:ac-direct-matches ()
  (mapcar
   (lambda (x)
     (destructuring-bind (&key word doc description symbol)
         x
       (popup-make-item word
                        :symbol symbol
                        :document (unless (equal doc "") doc)
                        :summary description)))
   jedi:complete-reply))

;;;###autoload
(defun jedi:ac-setup ()
  "Add Jedi AC sources to `ac-sources'.

If auto-completion is all you need, you can call this function instead
of `jedi:setup', like this::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

Note that this function calls `auto-complete-mode' if it is not
already enabled, for people who don't call `global-auto-complete-mode'
in their Emacs configuration."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  (unless auto-complete-mode
    (auto-complete-mode)))

(defun jedi:ac-direct-prefix ()
  (or (ac-prefix-default)
      (when (= jedi:complete-request-point (point))
        jedi:complete-request-point)))

(defun jedi:after-change-handler (&rest _)
  (unless (or (ac-menu-live-p) (ac-inline-live-p))
    (jedi:defined-names--singleton-deferred)))

;; (makunbound 'ac-source-jedi-direct)
(ac-define-source jedi-direct
  '((candidates . jedi:ac-direct-matches)
    (prefix . jedi:ac-direct-prefix)
    (init . jedi:complete-request)
    (requires . -1)))

;;;###autoload
(defun* jedi:complete (&key (expand ac-expand-on-auto-complete))
  "Complete code at point."
  (interactive)
  (lexical-let ((expand expand))
    (deferred:nextc (jedi:complete-request)
      (lambda ()
        (let ((ac-expand-on-auto-complete expand))
          (ac-start :triggered 'command))))))
;; Calling `auto-complete' or `ac-update-greedy' instead of `ac-start'
;; here did not work.

(defun jedi:dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (when overwrite-mode
    (delete-char 1))
  (insert ".")
  (unless (or (ac-cursor-on-diable-face-p)
              ;; don't complete if the dot is immediately after int literal
              (looking-back "\\(\\`\\|[^._[:alnum:]]\\)[0-9]+\\."))
    (jedi:complete :expand nil)))

(defun jedi:auto-complete-mode ()
  (let ((map jedi-mode-map))
    (if jedi:complete-on-dot
        (define-key map "." 'jedi:dot-complete)
      (define-key map "." nil)))
  (if jedi-mode
      (add-hook 'after-change-functions 'jedi:after-change-handler nil t)
    (remove-hook 'after-change-functions 'jedi:after-change-handler t)))

(setq jedi:setup-function #'jedi:ac-setup
      jedi:mode-function #'jedi:auto-complete-mode)

(provide 'jedi)

;;; jedi-auto-complete.el ends here
