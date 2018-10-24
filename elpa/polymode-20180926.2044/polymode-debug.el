;;; polymode-debug.el --- Interactive debugging utilities for polymode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2016-2018 Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/polymode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:
;;

;;; Code:

(require 'polymode-core)
(require 'poly-lock)
(require 'trace)


;;; MINOR MODE

(defvar pm--underline-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:underline (:color "tomato" :style wave)))
    overlay)
  "Overlay used in function `pm-debug-mode'.")

(defvar pm--highlight-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:inverse-video t))
    overlay)
  "Overlay used by `pm-debug-map-over-spans-and-highlight'.")

(defvar pm-debug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n M-i")     #'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n i")       #'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n M-p")     #'pm-debug-print-relevant-variables)
    (define-key map (kbd "M-n p")       #'pm-debug-print-relevant-variables)
    (define-key map (kbd "M-n M-h")     #'pm-debug-map-over-spans-and-highlight)
    (define-key map (kbd "M-n h")       #'pm-debug-map-over-spans-and-highlight)
    (define-key map (kbd "M-n M-t t")   #'pm-toggle-tracing)
    (define-key map (kbd "M-n M-t i")   #'pm-debug-toogle-info-message)
    (define-key map (kbd "M-n M-t f")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t p")   #'pm-debug-toggle-post-command)
    (define-key map (kbd "M-n M-t c")   #'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t a")   #'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-t M-t")   #'pm-toggle-tracing)
    (define-key map (kbd "M-n M-t M-i")   #'pm-debug-toogle-info-message)
    (define-key map (kbd "M-n M-t M-f")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t M-p")   #'pm-debug-toggle-post-command)
    (define-key map (kbd "M-n M-t M-c")   #'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t M-a")   #'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-f s")   #'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f b")   #'pm-debug-fontify-current-buffer)
    (define-key map (kbd "M-n M-f M-t")   #'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-f M-s")   #'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f M-b")   #'pm-debug-fontify-current-buffer)
    map))

;;;###autoload
(define-minor-mode pm-debug-minor-mode
  "Turns on/off useful facilities for debugging polymode.

Key bindings:
\\{pm-debug-minor-mode-map}"
  nil
  " PMDBG"
  :group 'polymode
  (if pm-debug-minor-mode
      (progn
        ;; this is global hook. No need to complicate with local hooks
        (add-hook 'post-command-hook 'pm-debug-highlight-current-span))
    (delete-overlay pm--underline-overlay)
    (delete-overlay pm--highlight-overlay)
    (remove-hook 'post-command-hook 'pm-debug-highlight-current-span)))

;;;###autoload
(defun pm-debug-minor-mode-on ()
  ;; activating everywhere (in case font-lock infloops in a polymode buffer )
  ;; this doesn't activate in fundamental mode
  (unless (eq major-mode 'minibuffer-inactive-mode)
    (pm-debug-minor-mode t)))

;;;###autoload
(define-globalized-minor-mode pm-debug-mode pm-debug-minor-mode pm-debug-minor-mode-on)


;;; INFO

(cl-defgeneric pm-debug-info (chunkmode))
(cl-defmethod pm-debug-info (chunkmode)
  (eieio-object-name chunkmode))
(cl-defmethod pm-debug-info ((chunkmode pm-inner-chunkmode))
  (format "%s head-matcher:\"%s\" tail-matcher:\"%s\""
          (cl-call-next-method)
          (eieio-oref chunkmode 'head-matcher)
          (eieio-oref chunkmode 'tail-matcher)))
(cl-defmethod pm-debug-info ((_chunkmode pm-inner-auto-chunkmode))
  (cl-call-next-method))

(defvar syntax-ppss-wide)
(defvar syntax-ppss-last)
(defun pm--debug-info (&optional span as-list)
  (let* ((span (or span (and polymode-mode (pm-innermost-span))))
         (message-log-max nil)
         (beg (nth 1 span))
         (end (nth 2 span))
         (obj (nth 3 span))
         (type (and span (or (car span) 'host))))
    (let ((out (list (current-buffer)
                     (point-min) (point) (point-max)
                     major-mode
                     type beg end
                     (and obj (pm-debug-info obj))
                     (format "lppss:%s"
                             (if pm--emacs>26
                                 (car syntax-ppss-wide)
                               syntax-ppss-last)))))
      (if as-list
          out
        (apply #'format
               "(%s) min:%d pos:%d max:%d || (%s) type:%s span:%s-%s %s %s"
               out)))))

(defun pm-debug-info-on-current-span (no-cache)
  "Show info on current span.
With NO-CACHE prefix, don't use cached values of the span."
  (interactive "P")
  (if (not polymode-mode)
      (message "not in a polymode buffer")
    (let ((span (pm-innermost-span nil no-cache)))
      (message (pm--debug-info span))
      ;; (move-overlay pm--highlight-overlay (nth 1 span) (nth 2 span) (current-buffer))
      (pm-debug-flick-region (nth 1 span) (nth 2 span)))))


;;; TOGGLING

(defvar pm-debug-display-info-message nil)
(defun pm-debug-toogle-info-message ()
  "Toggle permanent info display."
  (interactive)
  (setq pm-debug-display-info-message (not pm-debug-display-info-message)))

(defvar poly-lock-allow-fontification)
(defun pm-debug-toggle-fontification ()
  "Enable or disable fontification in polymode buffers."
  (interactive)
  (if poly-lock-allow-fontification
      (progn
        (message "fontificaiton disabled")
        (setq poly-lock-allow-fontification nil
              font-lock-mode nil))
    (message "fontificaiton enabled")
    (setq poly-lock-allow-fontification t
          font-lock-mode t)))

(defun pm-debug-toggle-after-change ()
  "Allow or disallow polymode actions in `after-change-functions'."
  (interactive)
  (if pm-allow-after-change-hook
      (progn
        (message "after-change disabled")
        (setq pm-allow-after-change-hook nil))
    (message "after-change enabled")
    (setq pm-allow-after-change-hook t)))

(defun pm-debug-toggle-post-command ()
  "Allow or disallow polymode actions in `post-command-hook'."
  (interactive)
  (if pm-allow-post-command-hook
      (progn
        (message "post-command disabled")
        (setq pm-allow-post-command-hook nil))
    (message "post-command enabled")
    (setq pm-allow-post-command-hook t)))

(defun pm-debug-toggle-all ()
  "Toggle all polymode guards back and forth."
  (interactive)
  (if poly-lock-allow-fontification
      (progn
        (message "fontificaiton, after-chnage and command-hook disabled")
        (setq poly-lock-allow-fontification nil
              pm-allow-after-change-hook nil
              pm-allow-post-command-hook nil))
    (message "fontificaiton, after-change and command-hook enabled")
    (setq poly-lock-allow-fontification t
          pm-allow-after-change-hook t
          pm-allow-post-command-hook t)))


;;; FONT-LOCK

(defun pm-debug-fontify-current-span ()
  "Fontify current span."
  (interactive)
  (let ((span (pm-innermost-span))
        (poly-lock-allow-fontification t))
    (poly-lock-flush (nth 1 span) (nth 2 span))
    (poly-lock-fontify-now (nth 1 span) (nth 2 span))))

(defun pm-debug-fontify-current-buffer ()
  "Fontify current buffer."
  (interactive)
  (let ((poly-lock-allow-fontification t))
    (poly-lock-flush (point-min) (point-max))
    (poly-lock-fontify-now (point-min) (point-max))))


;;; TRACING

(defvar pm-traced-functions
  '(
    ;; core initialization
    (0 (pm-initialize
        pm--common-setup
        pm--mode-setup))
    ;; core hooks
    (1 (polymode-post-command-select-buffer
        polymode-before-change-setup
        polymode-after-kill-fixes))
    ;; advises
    (2 (pm-override-output-cons
        pm-around-advice
        polymode-with-current-base-buffer))
    ;; font-lock
    (3 (poly-lock-function
        poly-lock-fontify-now
        poly-lock-flush
        jit-lock-fontify-now
        poly-lock-after-change
        poly-lock--extend-region-span
        poly-lock--extend-region
        poly-lock-adjust-span-face))
    ;; syntax
    (4 (pm--call-syntax-propertize-original
        polymode-syntax-propertize
        polymode-restrict-syntax-propertize-extension
        pm--reset-ppss-last))))

(defvar pm--do-trace nil)
;;;###autoload
(defun pm-toggle-tracing (level)
  "Toggle polymode tracing.
With numeric prefix toggle tracing for that LEVEL. Currently
universal argument toggles maximum level of tracing (4). Default
level is 3."
  (interactive "P")
  (setq level (prefix-numeric-value (or level 3)))
  (setq pm--do-trace (not pm--do-trace))
  (if pm--do-trace
      (progn (dolist (kv pm-traced-functions)
               (when (<= (car kv) level)
                 (dolist (fn (cadr kv))
                   (pm-trace fn))))
             (message "Polymode tracing activated"))
    (untrace-all)
    (message "Polymode tracing deactivated")))

;;;###autoload
(defun pm-trace (fn)
  "Trace function FN.
Use `untrace-function' to untrace or `untrace-all' to untrace all
currently traced functions."
  (interactive (trace--read-args "Trace: "))
  (let ((buff (get-buffer "*Messages*")))
    (advice-add
     fn :around
     (let ((advice (trace-make-advice
                    fn buff 'background #'pm-trace--tracing-context)))
       (lambda (body &rest args)
         (when (eq fn 'polymode-before-change-setup)
           (with-current-buffer buff
             (save-excursion
               (goto-char (point-max))
               (insert "\n"))))
         (if polymode-mode
             (apply advice body args)
           (apply body args))))
     `((name . ,trace-advice-name) (depth . -100)))))

(defun pm-trace-functions-by-regexp (regexp)
  "Trace all functions whose name matched REGEXP."
  (interactive "sRegex: ")
  (cl-loop for sym being the symbols
           when (and (fboundp sym)
                     (not (eq sym 'pm-trace)))
           when (string-match regexp (symbol-name sym))
           do (pm-trace sym)))

(defun pm-trace--tracing-context ()
  (let ((span (or *span*
                  (get-text-property (point) :pm-span))))
    (format " [%s pos:%d(%d-%d) %s%s (%f)]"
            (current-buffer) (point) (point-min) (point-max)
            (or (when span
                  (when (not (and (= (point-min) (nth 1 span))
                                  (= (point-max) (nth 2 span))))
                    "UNPR "))
                "")
            (when span
              (pm-format-span span))
            (float-time))))

;; fix object printing
(defun pm-trace--fix-1-arg-for-tracing (arg)
  (cond
   ((eieio-object-p arg) (eieio-object-name arg))
   ((and (listp arg) (eieio-object-p (nth 3 arg)))
    (list (nth 0 arg) (nth 1 arg) (nth 2 arg) (eieio-object-name (nth 3 arg))))
   (arg)))

(defun pm-trace--fix-args-for-tracing (orig-fn fn level args context)
  (let ((args (or (and (listp args)
                       (listp (cdr args))
                       (ignore-errors (mapcar #'pm-trace--fix-1-arg-for-tracing args)))
                  args)))
    (funcall orig-fn fn level args context)))

(advice-add #'trace-entry-message :around #'pm-trace--fix-args-for-tracing)
(advice-add #'trace-exit-message :around #'pm-trace--fix-args-for-tracing)
;; (advice-remove #'trace-entry-message #'pm-trace--fix-args-for-tracing)
;; (advice-remove #'trace-exit-message #'pm-trace--fix-args-for-tracing)


;;; RELEVANT VARIABLES

(defvar pm-debug-relevant-variables
  '(
    :change (before-change-functions
             after-change-functions)
    :command (pre-command-hook
              post-command-hook)
    :font-lock (fontification-functions
                font-lock-function
                font-lock-flush-function
                font-lock-ensure-function
                font-lock-fontify-region-function
                font-lock-fontify-buffer-function
                font-lock-unfontify-region-function
                font-lock-unfontify-buffer-function
                jit-lock-after-change-extend-region-functions
                jit-lock-functions)
    :indent (indent-line-function
             indent-region-function
             pm--indent-line-function-original)
    :revert (revert-buffer-function
             before-revert-hook
             after-revert-hook)
    :save (after-save-hook
           before-save-hook)
    :syntax (syntax-propertize-function
             syntax-propertize-extend-region-functions
             pm--syntax-propertize-function-original)
    ))

(defun pm-debug-print-relevant-variables ()
  "Print values of relevant hooks and other variables."
  (interactive)
  (let* ((buff (get-buffer-create "*polymode-vars*"))
         (cbuff (current-buffer))
         (vars (cl-loop for v on pm-debug-relevant-variables by #'cddr
                        collect (cons (car v)
                                      (mapcar (lambda (v)
                                                (cons v (buffer-local-value v cbuff)))
                                              (cadr v)))))
         (cbuff (current-buffer)))
    (require 'pp)
    (with-current-buffer buff
      (erase-buffer)
      (goto-char (point-max))
      (insert (format "\n================== %s ===================\n" cbuff))
      (insert (pp-to-string vars))
      (toggle-truncate-lines -1)
      (goto-char (point-max)))
    (display-buffer buff)))


;;; HIGHLIGHT

(defun pm-debug-highlight-current-span ()
  (when polymode-mode
    (with-silent-modifications
      (unless (memq this-command '(pm-debug-info-on-current-span
                                   pm-debug-highlight-last-font-lock-error-region))
        (delete-overlay pm--highlight-overlay))
      (condition-case-unless-debug err
          (let ((span (pm-innermost-span)))
            (when pm-debug-display-info-message
              (message (pm--debug-info span)))
            (move-overlay pm--underline-overlay (nth 1 span) (nth 2 span) (current-buffer)))
        (error (message "%s" (error-message-string err)))))))

(defun pm-debug-flick-region (start end &optional delay)
  (move-overlay pm--highlight-overlay start end (current-buffer))
  (run-with-timer (or delay 0.4) nil (lambda () (delete-overlay pm--highlight-overlay))))

(defun pm-debug-map-over-spans-and-highlight ()
  "Map over all spans in the buffer and highlight briefly."
  (interactive)
  (pm-map-over-spans (lambda (span)
                       (let ((start (nth 1 span))
                             (end (nth 2 span)))
                         (pm-debug-flick-region start end)
                         (sit-for 1)))
                     (point-min) (point-max) nil nil t))

(defun pm-debug-run-over-check (no-cache)
  "Map over all spans and report the time taken.
Switch to buffer is performed on every position in the buffer.
On prefix NO-CACHE don't use cached spans."
  (interactive)
  (goto-char (point-min))
  (let ((start (current-time))
        (count 1)
        (pm-initialization-in-progress no-cache))
    (pm-switch-to-buffer)
    (while (< (point) (point-max))
      (setq count (1+ count))
      (forward-char)
      (pm-switch-to-buffer))
    (let ((elapsed  (float-time (time-subtract (current-time) start))))
      (message "Elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))

(defun pm-dbg (msg &rest args)
  (let ((cbuf (current-buffer))
        (cpos (point)))
    (with-current-buffer (get-buffer-create "*pm-dbg*")
      (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (insert (apply 'format (concat "%f [%s at %d]: " msg)
                       (float-time) cbuf cpos args))))))

(provide 'polymode-debug)
;;; polymode-debug.el ends here
