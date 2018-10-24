;;; poly-lock.el --- Font lock sub-system for polymode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2018, Vitalie Spinu
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
;; Commentary
;;
;;
;; FONT-LOCK COMPONENTS:
;;
;; All * functions are lazy in poly-lock and jit-lock because they just mark
;; 'fontified nil.
;;
;;  fontification-functions ->                                           jit-lock-function  / poly-lock-function
;;  font-lock-ensure ->           font-lock-ensure-function ->           jit-lock-fontify-now/poly-lock-fontify-now
;; *font-lock-flush  ->           font-lock-flush-function  ->           jit-lock-refontify / poly-lock-flush
;; *font-lock-fontify-buffer ->   font-lock-fontify-buffer-function ->   jit-lock-refontify / poly-lock-flush
;;  font-lock-fontify-region ->   font-lock-fontify-region-function ->   font-lock-default-fontify-region
;;  font-lock-unfontify-region -> font-lock-unfontify-region-function -> font-lock-default-unfontify-region
;;  font-lock-unfontify-buffer -> font-lock-unfontify-buffer-function -> font-lock-default-unfontify-buffer
;;
;; Jit-lock components:
;; fontification-functions (called by display engine)
;;   --> jit-lock-function
;;     --> jit-lock-fontify-now (or deferred through timer/text-properties)
;;       --> jit-lock--run-functions
;;         --> jit-lock-functions (font-lock-fontify-region bug-reference-fontify etc.)
;;
;;
;; Poly-lock components:
;; fontification-functions
;;   --> poly-lock-function
;;    --> poly-lock-fontify-now
;;      --> jit-lock-fontify-now
;;      ...
;;
;; `font-lock-mode' call graph:
;; -> font-lock-function <---- replaced by `poly-lock-mode'
;;   -> font-lock-default-function
;;     -> font-lock-mode-internal
;;        -> font-lock-turn-on-thing-lock
;;           -> font-lock-turn-on-thing-lock
;;             -> (setq font-lock-flush-function jit-lock-refontify)
;;             -> (setq font-lock-ensure-function jit-lock-fontify-now)
;;             -> (setq font-lock-fontify-buffer-function jit-lock-refontify)
;;             -> (jit-lock-register #'font-lock-fontify-region)
;;               -> (add-hook 'jit-lock-functions #'font-lock-fontify-region nil t)
;;               -> jit-lock-mode
;;
;;; Commentary:
;;
;;; Code:

(require 'jit-lock)
(require 'polymode-core)

(defvar poly-lock-allow-fontification t)
(defvar poly-lock-allow-background-adjustment t)
(defvar poly-lock-fontification-in-progress nil)
(defvar-local poly-lock-mode nil)

(eval-when-compile
  (defmacro with-buffer-prepared-for-poly-lock (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    (declare (debug t))
    `(let ((inhibit-point-motion-hooks t))
       (with-silent-modifications
         ,@body))))

(defun poly-lock-no-jit-lock-in-polymode-buffers (fun arg)
  "Don't activate FUN in `polymode' buffers.
When not in polymode buffers apply FUN to ARG."
  (unless (or polymode-mode pm/polymode)
    (funcall fun arg)))
(pm-around-advice 'jit-lock-mode #'poly-lock-no-jit-lock-in-polymode-buffers)

(defun poly-lock-mode (arg)
  "This is the value of `font-lock-function' in all polymode buffers.
Mode activated when ARG is positive; happens when font-lock is
switched on."
  (unless polymode-mode
    (error "Calling `poly-lock-mode' in a non-polymode buffer (%s)" (current-buffer)))

  (setq poly-lock-mode arg)

  (if arg
      (progn
        ;; a lot of the following is inspired by what jit-lock does in
        ;; `font-lock-turn-on-thing-lock'

        (setq-local font-lock-support-mode 'poly-lock-mode)
        (setq-local font-lock-dont-widen t)

        ;; Re-use jit-lock registration. Some minor modes (adaptive-wrap)
        ;; register extra functionality. [Unfortunately `jit-lock-register'
        ;; calls `jit-lock-mode' which we don't want. Hence the advice. TOTHINK:
        ;; Simply add-hook to `jit-lock-functions'?]
        (jit-lock-register 'font-lock-fontify-region)

        ;; don't allow other functions
        (setq-local fontification-functions '(poly-lock-function))

        (setq-local font-lock-flush-function 'poly-lock-flush)
        (setq-local font-lock-fontify-buffer-function 'poly-lock-flush)
        (setq-local font-lock-ensure-function 'poly-lock-fontify-now)

        ;; There are some more, jit-lock doesn't change those, neither do we:
        ;; font-lock-unfontify-region-function (defaults to font-lock-default-unfontify-region)
        ;; font-lock-unfontify-buffer-function (defualts to font-lock-default-unfontify-buffer)

        ;; Don't fontify eagerly (and don't abort if the buffer is large). NB:
        ;; `font-lock-flush' is not triggered if this is nil.
        (setq-local font-lock-fontified t)

        ;; Now we can finally call `font-lock-default-function' because
        ;; `font-lock-support-mode' is set to "unrecognizible" value, only core
        ;; font-lock setup happens.
        (font-lock-default-function arg)

        ;; Must happen after call to `font-lock-default-function'
        (remove-hook 'after-change-functions 'font-lock-after-change-function t)
        (remove-hook 'after-change-functions 'jit-lock-after-change t)
        (add-hook 'after-change-functions 'poly-lock-after-change nil t)

        ;; Reusing jit-lock var becuase modes populate it directly. We are using
        ;; this in `poly-lock-after-change' below. Taken from `jit-lock
        ;; initialization.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))

    (remove-hook 'after-change-functions 'poly-lock-after-change t)
    (remove-hook 'fontification-functions 'poly-lock-function t))
  (current-buffer))

(defun poly-lock-function (start)
  "The only function in `fontification-functions' in polymode buffers.
This is the entry point called by the display engine. START is
defined in `fontification-functions'. This function has the same
scope as `jit-lock-function'."
  (unless pm-initialization-in-progress
    (if (and poly-lock-mode
             (not memory-full))
        (unless (input-pending-p)
          (let ((end (or (text-property-any start (point-max) 'fontified t)
                         (point-max))))
            (when (< start end)
              (poly-lock-fontify-now start end))))
      (with-buffer-prepared-for-poly-lock
       (put-text-property start (point-max) 'fontified t)))))

(defun poly-lock-fontify-now (beg end &optional _verbose)
  "Polymode main fontification function.
Fontifies chunk-by chunk within the region BEG END."
  (unless (or poly-lock-fontification-in-progress
              pm-initialization-in-progress)
    (let* ((font-lock-dont-widen t)
           (font-lock-extend-region-functions nil)
           ;; Fontification in one buffer can trigger fontification in another
           ;; buffer. Particularly, this happens when new indirect buffers are
           ;; created and `normal-mode' triggers font-lock in those buffers. We
           ;; avoid this by dynamically binding
           ;; `poly-lock-fontification-in-progress' and un-setting
           ;; `fontification-functions' in case re-display suddenly decides to
           ;; fontify something else in other buffer. There are also font-lock
           ;; guards in pm--mode-setup.
           (poly-lock-fontification-in-progress t)
           (fontification-functions nil)
           (protect-host (with-current-buffer (pm-base-buffer)
                           (eieio-oref pm/chunkmode 'protect-font-lock))))
      (save-restriction
        (widen)
        (save-excursion
          ;; fontify the whole region in host first. It's ok for modes like
          ;; markdown, org and slim which understand inner modes in a limited way.
          (unless protect-host
            (let ((span (pm-innermost-span beg)))
              (when (or (null (pm-true-span-type span))
                        ;; in inner spans fontify only if region is bigger than the span
                        (< (nth 2 span) end))
                (with-current-buffer (pm-base-buffer)
                  (with-buffer-prepared-for-poly-lock
                   (when poly-lock-allow-fontification
                     (jit-lock-fontify-now beg end))
                   (put-text-property beg end 'fontified t))))))
          (pm-map-over-spans
           (lambda (span)
             (when (or (pm-true-span-type span)
                       protect-host)
               (with-buffer-prepared-for-poly-lock
                (let ((sbeg (nth 1 span))
                      (send (nth 2 span)))
                  ;; skip empty spans
                  (when (> send sbeg)
                    (if (not (and poly-lock-allow-fontification
                                  poly-lock-mode))
                        (put-text-property sbeg send 'fontified t)
                      (let ((new-beg (max sbeg beg))
                            (new-end (min send end)))
                        (put-text-property new-beg new-end 'fontified nil)
                        (condition-case-unless-debug err
                            (if (eieio-oref pm/chunkmode 'protect-font-lock)
                                (pm-with-narrowed-to-span span
                                  (jit-lock-fontify-now new-beg new-end))
                              (jit-lock-fontify-now new-beg new-end))
                          (error
                           (message "(poly-lock-fontify-now %s %s [span %d %d %s]) -> (%s %s %s): %s"
                                    beg end sbeg send (current-buffer)
                                    font-lock-fontify-region-function new-beg new-end
                                    (error-message-string err))))
                        ;; even if failed set to t
                        (put-text-property new-beg new-end 'fontified t)))
                    (when poly-lock-allow-background-adjustment
                      (poly-lock-adjust-span-face span)))))))
           beg end))))
    (current-buffer)))

(defun poly-lock-flush (&optional beg end)
  "Force refontification of the region BEG..END.
END is extended to the next chunk separator. This function is
placed in `font-lock-flush-function''"
  (unless poly-lock-fontification-in-progress
    (let ((beg (or beg (point-min)))
          (end (or end (point-max))))
      (with-buffer-prepared-for-poly-lock
       (save-restriction
         (widen)
         (pm-flush-span-cache beg end)
         (put-text-property beg end 'fontified nil))))))

(defvar jit-lock-start)
(defvar jit-lock-end)
(defun poly-lock--extend-region (beg end)
  "Our own extension function which runs first on BEG END change.
Assumes widen buffer. Sets `jit-lock-start' and `jit-lock-end'."
  ;; old span can disappear, shrunk, extend etc
  (let* ((old-beg (or (previous-single-property-change end :pm-span)
                      (point-min)))
         (old-end (or (next-single-property-change end :pm-span)
                      (point-max)))
         ;; need this here before pm-innermost-span call
         (old-beg-obj (nth 3 (get-text-property old-beg :pm-span)))
         ;; (old-end-obj (nth 3 (get-text-property old-end :pm-span)))
         (beg-span (pm-innermost-span beg 'no-cache))
         (end-span (if (= beg end)
                       beg-span
                     (pm-innermost-span end 'no-cache)))
         (sbeg (nth 1 beg-span))
         (send (nth 2 end-span)))
    (if (< old-beg sbeg)
        (let ((new-beg-span (pm-innermost-span old-beg)))
          (if (eq old-beg-obj (nth 3 new-beg-span))
              ;; new span appeared within an old span, don't refontify the old part (common case)
              (setq jit-lock-start (min sbeg (nth 2 new-beg-span)))
            ;; wrong span shrunk to its correct size (rare or never)
            (setq jit-lock-start old-beg)))
      ;; refontify the entire new span
      (setq jit-lock-start sbeg))
    ;; I think it's not possible to do better than this. When region is shrunk,
    ;; previous region could be incorrectly fontified even if the mode is
    ;; preserved due to wrong ppss
    (setq jit-lock-end (max send old-end))
    ;; (if (> old-end send)
    ;;     (let ((new-end-span (pm-innermost-span (max (1- old-end) end))))
    ;;       (if (eq old-end-obj (nth 3 new-end-span))
    ;;           ;; new span appeared within an old span, don't refontify the old part (common case)
    ;;           (setq jit-lock-end (max end (nth 1 new-end-span)))
    ;;         ;; wrong span shrunk to its correct size
    ;;         (setq jit-lock-end old-end)))
    ;;   ;; refontify the entire new span
    ;;   (setq jit-lock-end send))

    ;; Check if the type of following span changed (for example when
    ;; modification is in head of an auto-chunk). Do this repeatedly till no
    ;; change. [TOTHINK: Do we need similar extension backwards?]
    (let ((go-on t))
      (while (and (< jit-lock-end (point-max))
                  go-on)
        (let ((ospan (get-text-property jit-lock-end :pm-span))
              (nspan (pm-innermost-span jit-lock-end 'no-cache)))
          (if (eq (nth 3 nspan) (nth 3 ospan))
              (setq go-on nil)
            (setq jit-lock-end (nth 2 nspan))))))
    (cons jit-lock-start jit-lock-end)))

(defun poly-lock--extend-region-span (span old-len)
  "Call `jit-lock-after-change-extend-region-functions' protected to SPAN.
Extend `jit-lock-start' and `jit-lock-end' by side effect.
OLD-LEN is passed to the extension function."
  (let ((beg jit-lock-start)
        (end jit-lock-end))
    (let ((sbeg (nth 1 span))
          (send (nth 2 span)))
      ;; expand only in top & bottom spans
      (when (or (> beg sbeg) (< end send))
        (pm-with-narrowed-to-span span
          (setq jit-lock-start (max beg sbeg)
                jit-lock-end   (min end send))
          (condition-case err
              (progn
                ;; set jit-lock-start and jit-lock-end by side effect
                (run-hook-with-args 'jit-lock-after-change-extend-region-functions
                                    jit-lock-start jit-lock-end old-len))
            (error (message "(after-change-extend-region-functions %s %s %s) -> %s"
                            jit-lock-start jit-lock-end old-len
                            (error-message-string err))))
          (setq jit-lock-start (min beg (max jit-lock-start sbeg))
                jit-lock-end (max end (min jit-lock-end send))))
        (cons jit-lock-start jit-lock-end)))))

(defun poly-lock-after-change (beg end old-len)
  "Mark changed region with 'fontified nil.
Installed in `after-change-functions' and behaves similarly to
`jit-lock-after-change' in what it calls
`jit-lock-after-change-extend-region-functions' in turn but with
the buffer narrowed to the relevant spans. BEG, END and OLD-LEN
are as in `after-change-functions'."
  (when (and poly-lock-mode
             pm-allow-after-change-hook
             (not memory-full))
    (with-buffer-prepared-for-poly-lock
     (save-excursion
       (save-match-data
         (save-restriction
           (widen)
           (poly-lock--extend-region beg end)
           (pm-flush-span-cache beg end)
           (pm-map-over-spans
            (lambda (span) (poly-lock--extend-region-span span old-len))
            ;; fixme: no-cache is no longer necessary, we flush the region
            beg end nil nil nil 'no-cache)
           (put-text-property jit-lock-start jit-lock-end 'fontified nil)
           (cons jit-lock-start jit-lock-end)))))))

(defun poly-lock--adjusted-background (prop)
  ;; if > lighten on dark backgroun. Oposite on light.
  (color-lighten-name (face-background 'default)
                      (if (eq (frame-parameter nil 'background-mode) 'light)
                          (- prop) ;; darken
                        prop)))

(declare-function pm-get-adjust-face "polymode-methods")
(defun poly-lock-adjust-span-face (span)
  "Adjust 'face property of SPAN..
How adjustment is made is defined in :adjust-face slot of the
SPAN's chunkmode."
  (interactive "r")
  (let ((face (pm-get-adjust-face (nth 3 span) (car span))))
    (when face
      (with-current-buffer (current-buffer)
        (let ((face (or (and (numberp face)
                             (list (cons 'background-color
                                         (poly-lock--adjusted-background face))))
                        face)))
          (font-lock-prepend-text-property
           (nth 1 span) (nth 2 span) 'face face))))))

(provide 'poly-lock)
;;; poly-lock.el ends here
