;;; mathjax.el --- Render formulas using MathJax     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/mathjax.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;; Keywords: tex, text, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to MathJax, a mathematical
;; display engine.

;;; Code:

(require 'dom)
(require 'svg)
(eval-when-compile (require 'subr-x))

(defvar mathjax--state nil
  "Place to store the MathJax process state.")

(defvar mathjax--ttl 60
  "Time to let the MathJax process live without producing output.")

(defvar mathjax--installation-directory
  (if-let ((file (if (fboundp 'macroexp-file-name) ;Emacs≥28.
                     (macroexp-file-name)
                   load-file-name)))
      (file-name-directory file)
    default-directory)
  "Directory where the `mathjax' package is installed.")

(defun mathjax-available-p ()
  "Return non-nil if MathJax can be used.

Currently, the only requirement to use this (besides Emacs with SVG
image support) is the Node JavaScript interpreter."
  (not (null (let ((default-directory mathjax--installation-directory))
               (executable-find "node")))))

(defun mathjax--get-state ()
  "Return a cons cell consisting of a MathJax process and a list of callbacks."
  (unless (process-live-p (car mathjax--state))
    (setq mathjax--state nil))
  (with-memoization mathjax--state
    (let* ((default-directory mathjax--installation-directory)
           (buffer (generate-new-buffer " *mathjax*"))
           (sentinel
            (lambda (proc _)
              (cond
               ((process-live-p proc))
               ((zerop (process-exit-status proc)) (kill-buffer buffer))
               (t (lwarn 'devdocs :error
                         (format "\
MathJax process exited with status %s.  See buffer %s for more information."
                                 (process-exit-status proc)
                                 (buttonize (buffer-name buffer)
                                            #'pop-to-buffer
                                            buffer)))))))
           (proc
            (make-process
             :name "mathjax"
             :buffer buffer
             :connection-type 'pipe
             :noquery t
             :command
             `("node" "math2svg.js")
             :sentinel sentinel))
           (timer (run-at-time mathjax--ttl nil
                               (lambda ()
                                 (setq mathjax--state nil)
                                 (process-send-eof proc)))))
      (add-function
       :after (process-filter proc)
       (lambda (&rest _)
         (goto-char (point-min))
         (while-let ((data (ignore-errors
                             (json-parse-buffer :object-type 'alist)))
                     (callback (pop (cdr mathjax--state))))
           (funcall callback data))
         (delete-region (point-min) (point))
         (timer-set-time timer (time-add nil mathjax--ttl))))
      (list proc))))

(cl-defun mathjax-render (callback math &key format options)
  "Asynchronously render MATH as an SVG image.

MATH can be a string in TeX, MathML or AsciiMath notation or a MathML
DOM node.  When ready, call CALLBACK with an alist argument.  It
contains an SVG image string under key `svg' if rendering was
successful, or a key `error' otherwise.

The following keyword arguments are accepted:

FORMAT can be `tex', `mathjax' or `asciimath'.  The default is `tex',
but this option is ignored if MATH is a DOM node.

OPTIONS is a plist containing any of the entries described in
https://docs.mathjax.org/en/latest/web/typeset.html#conversion-options."
  (when (eq 'math (car-safe math))
    (setq format 'mathml)
    (setq math (with-temp-buffer
                 (dom-print math)
                 (buffer-string))))
  (unless format (setq format 'tex))
  (let* ((mathjax (mathjax--get-state))
         (proc (car mathjax))
         (arg `(:math ,math :format ,(symbol-name format) ,@options)))
    (process-send-string proc (json-serialize arg))
    (process-send-string proc "\n")
    (nconc mathjax (list callback))))

(cl-defun mathjax-display (start end math &key format options after)
  "Render MATH and display it as an overlay between START and END.
The following keyword arguments are accepted:

AFTER, if given, is a function to be called with the new overlay as
argument.

FORMAT and OPTIONS are passed directly to `mathjax-render', which see."
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (buffer (current-buffer))
         (callback
          (lambda (data)
            (when (and (buffer-live-p buffer)
                       ;; Normally we allow buffer modifications
                       ;; between the time `mathjax-display' is called
                       ;; and the callback is called, but not if the
                       ;; region was deleted entirely.  Hence the test
                       ;; below.
                       (< start end))
              (with-current-buffer buffer
                (dolist (ov (overlays-at start))
                  (when (eq (overlay-get ov 'category) 'mathjax)
                    (delete-overlay ov)))
                (let* ((inhibit-read-only t)
                       (ov (make-overlay start end buffer t)))
                  (overlay-put ov 'category 'mathjax)
                  (overlay-put ov 'evaporate t)
                  (when-let ((svg (alist-get 'svg data)))
                    (let* ((h (and (string-match "height=\"\\([-.0-9]+\\)" svg)
                                   (string-to-number (match-string 1 svg))))
                           (va (and (string-match "vertical-align: \\([-.0-9]+\\)" svg)
                                    (string-to-number (match-string 1 svg))))
                           (ascent (if (and h va) (round (* 100 (+ va h) (/ h))) 100))
                           (image (svg-image svg :ascent ascent)))
                      (overlay-put ov 'display image)))
                  (when-let ((error (alist-get 'error data)))
                    (overlay-put ov 'face 'error)
                    (overlay-put ov 'mathjax-error error))
                  (when after
                    (funcall after ov))))))))
    (set-marker-insertion-type start t)
    (mathjax-render callback math :format format :options options)))

(defvar mathjax-delimiters
  `(("\\\\[\\$]" ,regexp-unmatchable)
    ("\\$\\$" "\\$\\$" :multiline t)
    ("\\$" "\\$")
    ("\\\\(" "\\\\)")
    ("\\\\\\[" "\\\\\\]" :multiline t))
  "List of math markup delimiters for `mathjax-typeset-region'.
Entries should be of the form

  (START-PATT END-PATT . PROPERTIES)

where START-PATT and END-PATT are regular expressions and PROPERTIES is
a plist containing keyword arguments to `mathjax-display', plus any of
following:

MULTILINE indicates whether to search for END-PATT beyond the end of the
line where START-PATT is found.  Search never advances beyond a blank
line.")

(defun mathjax--math-searchfn (items)
  "Return a function to search for math regions.

ITEMS should be a list like `mathjax-delimiters'.  The returned function
takes one argument, the search bound, and returns a list of the form

  (OUTER-START INNER-START INNER-END OUTER-END . OPTIONS)

The outer bounds include the delimiter and the inner bounds don't.  The
options plist is simply copied from the matching ITEMS entry."
  (let ((begpatt (concat "\\("
                         (string-join (mapcar #'car items) "\\)\\|\\(")
                         "\\)")))
    (lambda (limit)
      (catch 'mathjax--done
        (while (re-search-forward begpatt limit t)
          (pcase-let* ((ostart (match-beginning 0))
                       (istart (match-end 0))
                       (`(_ ,endpatt . ,options) ;Find which of the items matched
                        (let* ((md (cddr (match-data t)))
                               (it items))
                          (while (not (pop md)) (pop md) (pop it))
                          (car it)))
                       (limit (thread-first
                                (when (plist-get options :multiline)
                                  (save-excursion
                                    (when (re-search-forward "^[[:space:]]*$" limit t)
                                      (match-beginning 0))))
                                (or (pos-eol))
                                (min limit))))
            (when (re-search-forward endpatt limit t)
              (throw 'mathjax--done
                     `(,ostart ,istart ,(match-beginning 0) ,(match-end 0) ,@options)))))))))

(defun mathjax-typeset-region (start end)
  "Render and display all formulas between START and END."
  (let ((searchfn (if (functionp mathjax-delimiters)
                      mathjax-delimiters
                    (mathjax--math-searchfn mathjax-delimiters))))
    (save-excursion
      (goto-char start)
      (while-let ((m (funcall searchfn end)))
        (pcase-let ((`(,ostart ,istart ,iend ,oend . ,options) m))
          (mathjax-display ostart oend
                           (buffer-substring-no-properties istart iend)
                           :format (plist-get options :format)
                           :options (plist-get options :options)
                           :after (plist-get options :after)))))))

;;; shr integration

(defvar shr-external-rendering-functions)
(declare-function shr-generic "shr.el")

(defun mathjax-shr-tag-math (dom)
  "Function to render a math DOM node.
To be used in `shr-external-rendering-functions'."
  (let ((start (point)))
    (shr-generic (thread-first
                   dom
                   (dom-child-by-tag 'semantics)
                   (dom-child-by-tag 'annotation)
                   (or dom)))
    (mathjax-display start (point) dom
                     :after (when (bound-and-true-p shr-fill-text)
                              #'mathjax-refill-hack))))

(defun mathjax-refill-hack (ov)
  "Move a line break covered by OV to the right.

This can be used as an :after argument to `mathjax-display' and is
useful in combination with shr, since MathJax overlays may hide a line
break inserted when shr fills the paragraph."
  (save-excursion
    (goto-char (overlay-start ov))
    (when-let ((p0 (when (re-search-forward "\n[[:space:]]*" (overlay-end ov) t)
                     (match-beginning 0)))
               (p1 (point)))
      (goto-char (overlay-end ov))
       (when (re-search-forward "[[:space:]]+" (pos-eol) t)
         (delete-region (match-beginning 0) (point))
         (insert (buffer-substring p0 p1))))))


;;;###autoload
(defun mathjax-shr-setup ()
  "Arrange for `shr' to use MathJax in this buffer."
  (make-local-variable 'shr-external-rendering-functions)
  (push '(math . mathjax-shr-tag-math)
        shr-external-rendering-functions))

(provide 'mathjax)
;;; mathjax.el ends here
