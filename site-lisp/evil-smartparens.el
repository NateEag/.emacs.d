;;; evil-smartparens.el --- Evil bindings to Smartparens

;; Copyright (C) 2014 Joe Schafer

;; Author: Joe Schafer <joe@jschaf.com>
;; Version: 0.1
;; Keywords:  convenience

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

;; Common sense bindings to the smartparens library.
;;
;; See documentation on https://github.com/jschaf/evil-smartparens

(require 'smartparens)
(require 'evil)

;;; Code:

(define-minor-mode evil-smartparens-mode
  "Minor mode for combining Evil and SmartParens."
  :keymap '()
  :lighter "E-SP")

(defun evil-sp-evilize-name (name)
  "Return an interned symbol NAME prefixed with 'evil-'."
   (intern (format "evil-%s" name)))

(defmacro evil-sp-make-evil-smartparens-movement (name)
  "Create Evil movement command from NAME function."
  `(let ((evil-name (evil-sp-evilize-name ,name))
         (doc (documentation ,name)))
     (evil-define-motion evil-name (count)
       doc
       :type inclusive
       :jump t
       (,name count))))

(defun evil-sp-beginning-of-previous-sexp (&optional arg)
  "Goto beginning of previous sexp.
If ARG is non-nil, go back ARG sexps."
  (interactive "P")
  (setq arg (or arg 1))
  (sp-previous-sexp arg)
  (sp-backward-sexp))

(defun evil-sp-smartparens-config ()
  "Bind smarparens commands."
  (interactive)
  (let ((sexp-motions '(("zh" . sp-backward-sexp)
                        ("zj" . sp-down-sexp)
                        ("zk" . sp-backward-up-sexp)
                        ("zl" . sp-forward-sexp)

                        ("zp" . evil-sp-beginning-of-previous-sexp)
                        ("zJ" . sp-end-of-sexp)
                        ("zK" . sp-beginning-of-sexp)
                        ("zn" . sp-next-sexp)

                        ("zH" . sp-beginning-of-previous-sexp)
                        ("zL" . sp-beginning-of-next-sexp)

                        ("z\C-k" . sp-backward-down-sexp)
                        ("z\C-j" . sp-up-sexp)))


        (sexp-modifications '(("zst" . sp-transpose-sexp)

                              ("zsu" . sp-unwrap-sexp)
                              ("zsb" . sp-backward-unwrap-sexp)

                              ("zfh" . sp-backward-slurp-sexp)
                              ("zfH" . sp-backward-barf-sexp)

                              ("zfl" . sp-forward-slurp-sexp)
                              ("zfL" . sp-forward-barf-sexp)

                              ("zgL" . sp-add-to-previous-sexp)
                              ("zgH" . sp-add-to-next-sexp)

                              ("zsw" . sp-swap-enclosing-sexp)
                              ("zss" . sp-splice-sexp)
                              ("zsdl" . sp-splice-sexp-killing-forward)
                              ("zsdh" . sp-splice-sexp-killing-backward)
                              ("zsda" . sp-splice-sexp-killing-around)

                              ("zsc" . sp-convolute-sexp)

                              ("zsh" . sp-absorb-sexp)
                              ("zsl" . sp-emit-sexp)

                              ("zsH" . sp-extract-before-sexp)
                              ("zsL" . sp-extract-after-sexp)

                              ("zsy" . sp-split-sexp)
                              ("zsY" . sp-join-sexp))))

    (loop for (key . func) in sexp-motions
          do
          ;; Define the motion command
          (evil-sp-make-evil-smartparens-movement func)

          ;; Create key-bindings
          (define-key evil-normal-state-map key func)
          (define-key evil-visual-state-map key func)
          (define-key evil-motion-state-map key func))

    (loop for (key . func) in sexp-modifications
          do
          ;; Create key-bindings
          (define-key evil-normal-state-map key func))))

(evil-sp-smartparens-config)

(provide 'evil-smartparens)
;;; evil-smartparens.el ends here
