;;; symex-misc.el --- An evil way to edit Lisp symbolic expressions as trees -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/symex.el

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:
;;
;; Miscellaneous Lisp editing-related features
;;

;;; Code:


(require 'lispy)
(require 'evil)
(require 'symex-primitives)
(require 'symex-evaluator)
(require 'symex-traversals)
(require 'symex-interface-elisp)
(require 'symex-interface-racket)
(require 'symex-interface-scheme)
(require 'symex-interface-clojure)
(require 'symex-interface-common-lisp)
(require 'symex-interface-arc)

;; These are customization or config variables defined elsewhere;
;; explicitly indicating them here to avoid byte compile warnings
(defvar symex-refocus-p)
(defvar symex-highlight-p)
(defvar symex-racket-modes)
(defvar symex-elisp-modes)

;; buffer-local branch memory stack
(defvar-local symex--branch-memory nil)

;;;;;;;;;;;;;;;;;;;;;
;;; MISCELLANEOUS ;;;
;;;;;;;;;;;;;;;;;;;;;

(evil-define-state emacslike
  "An Emacs-like state."
  :tag " <E> "
  :message "-- EMACHS --"
  :enable (emacs))

(evil-define-state normallike
  "A Normal-like state."
  :tag " <N> "
  :message "-- NORMALE --"
  :enable (normal))

(defun symex-evaluate ()
  "Evaluate Symex."
  (interactive)
  (let ((original-evil-state evil-state))
    (unwind-protect
        (save-excursion
          ;; enter an "emacs-like" state so that which symex is meant
          ;; has a standard interpretation. We don't go into emacs state
          ;; itself since, as a known, "registered" evil state in
          ;; rigpa, it would trigger state transition logic
          ;; that we don't want to trigger since this is to be treated
          ;; merely as an implementation detail of this operation
          (evil-emacslike-state)
          (forward-sexp) ; selected symexes will have the cursor on the starting paren
          (cond ((member major-mode symex-racket-modes)
                 (symex-eval-racket))
                ((member major-mode symex-elisp-modes)
                 (symex-eval-elisp))
                ((equal major-mode 'scheme-mode)
                 (symex-eval-scheme))
                ((equal major-mode 'clojure-mode)
                 (symex-eval-clojure))
                ((equal major-mode 'lisp-mode)
                 (symex-eval-common-lisp))
                ((equal major-mode 'arc-mode)
                 (symex-eval-arc))
                (t (error "Symex mode: Lisp flavor not recognized!"))))
      ;; enter a "normal-like" state here momentarily, to prevent entry
      ;; into symex mode from being treated as if it was in an "emacs" context
      ;; since the entry into emacs state is done here as an implementation
      ;; detail and is not user-directed
      ;; we don't enter normal state itself but rather a clone, to go
      ;; "under the radar" of any registered hooks
      (evil-normallike-state)
      ;; ideally we shouldn't do this since it would still trigger entry
      ;; hooks, but for now that's OK
      ;; the right way to handle all this would be to avoid any state
      ;; transitions
      (funcall (intern (concat "evil-" (symbol-name original-evil-state) "-state"))))))

(defun symex-evaluate-definition ()
  "Evaluate entire containing symex definition."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-eval-definition-racket))
        ((member major-mode symex-elisp-modes)
         (symex-eval-definition-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-eval-definition-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-eval-definition-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-eval-definition-common-lisp))
        ((equal major-mode 'arc-mode)
         (symex-eval-definition-arc))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-evaluate-pretty ()
  "Evaluate Symex and transform output into a useful string representation."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-pretty-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-pretty-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-pretty-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-pretty-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-pretty-common-lisp))
          ((equal major-mode 'arc-mode)
           (symex-eval-pretty-arc))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-eval-print ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-print-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-print-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-print-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-print-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-print-common-lisp))
          ((equal major-mode 'arc-mode)
           (symex-eval-print-arc))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-evaluate-thunk ()
  "Evaluate Symex as a thunk.

This treats the symex as a thunk -- i.e. a function that takes no
arguments -- by (transparently) wrapping it in parens and then
executing it."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-eval-thunk-racket))
          ((member major-mode symex-elisp-modes)
           (symex-eval-thunk-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-eval-thunk-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-eval-thunk-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-eval-thunk-common-lisp))
          ((equal major-mode 'arc-mode)
           (symex-eval-thunk-arc))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-describe ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ; selected symexes will have the cursor on the starting paren
    (cond ((member major-mode symex-racket-modes)
           (symex-describe-symbol-racket))
          ((member major-mode symex-elisp-modes)
           (symex-describe-symbol-elisp))
          ((equal major-mode 'scheme-mode)
           (symex-describe-symbol-scheme))
          ((equal major-mode 'clojure-mode)
           (symex-describe-symbol-clojure))
          ((equal major-mode 'lisp-mode)
           (symex-describe-symbol-common-lisp))
          ((equal major-mode 'arc-mode)
           (symex-describe-symbol-arc))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-repl-racket))
        ((member major-mode symex-elisp-modes)
         (symex-repl-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-repl-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-repl-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-repl-common-lisp))
        ((equal major-mode 'arc-mode)
         (symex-repl-arc))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun symex-run ()
  "Send to REPL."
  (interactive)
  (cond ((member major-mode symex-racket-modes)
         (symex-run-racket))
        ((member major-mode symex-elisp-modes)
         (symex-run-elisp))
        ((equal major-mode 'scheme-mode)
         (symex-run-scheme))
        ((equal major-mode 'clojure-mode)
         (symex-run-clojure))
        ((equal major-mode 'lisp-mode)
         (symex-run-common-lisp))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(cl-defun symex--new-scratch-buffer (buffer-name)
  "Create a new empty buffer.

The buffer will be named BUFFER-NAME and will be created in the
currently active (at the time of command execution) major mode.
As a \"scratch\" buffer, its contents will be treated as
disposable, and it will not prompt to save if it is closed or
if Emacs is exited.

Modified from:
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer buffer-name))
        (major-mode-to-use major-mode))
    (with-current-buffer $buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save nil))
    $buf))

(defun symex-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (let* ((buffer-name (cond ((member major-mode symex-racket-modes)
                             "*scratch - Racket*")
                            ((member major-mode symex-elisp-modes)
                             "*scratch*")
                            ((equal major-mode 'scheme-mode)
                             "*scratch - Scheme*")
                            ((equal major-mode 'clojure-mode)
                             "*scratch - Clojure*")
                            ((equal major-mode 'lisp-mode)
                             "*scratch - Common Lisp*")
                            (t (error "Symex mode: Lisp flavor not recognized!"))))
         (buf (get-buffer buffer-name)))
    (let ((buf (or buf (symex--new-scratch-buffer buffer-name))))
      (switch-to-buffer-other-window buf))))

(defun symex-switch-to-messages-buffer ()
  "Switch to messages buffer while retaining focus in original window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  (goto-char (point-max))
  (recenter)
  (evil-window-mru))

(defun symex-select-nearest ()
  "Select symex nearest to point."
  (interactive)
  (cond ((and (not (eobp))
              (save-excursion (forward-char) (lispy-right-p)))  ; |)
         (forward-char)
         (lispy-different))
        ((symex-comment-line-p)
         (symex-if-stuck (symex--go-backward)
                         (symex--go-forward)))
        ((looking-at-p "[[:space:]\n]")  ; <>| <> or <> |$
         (condition-case nil
             (progn (re-search-forward "[^[:space:]\n]")
                    (backward-char))
           (error (symex-if-stuck (symex--go-backward)
                                  (symex--go-forward)))))
        ((thing-at-point 'sexp)  ; som|ething
         (beginning-of-thing 'sexp))
        (t (symex-if-stuck (symex--go-backward)
                           (symex--go-forward))))
  (point))

(defun symex-index ()  ; TODO: may be better framed as a computation
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((original-location (point)))
      (let ((current-location (symex-goto-first))
            (result 0))
        (while (< current-location original-location)
          (symex--execute-tree-move (symex-make-move 1 0))
          (setq current-location (point))
          (setq result (1+ result)))
        result))))

(defun symex-height ()  ; TODO: may be better framed as a computation
  "Get height (above root) of current symex."
  (interactive)
  (save-excursion
    (symex-select-nearest)
    (let ((moves (symex-execute-traversal symex--traversal-goto-lowest)))
      (length moves))))

(defun symex-depth ()
  "DEPRECATED.  Renamed to `symex-height`.

This interface will be removed in a future version."
  (symex-height))

(defun symex-leap-backward (&optional soar)
  "Leap backward to a neighboring branch, preserving height and position.

If SOAR is true, leap between trees too, otherwise, stay in the
current tree.

Note: This isn't the most efficient at the moment since it determines
the height at every step of the traversal which itself is logarithmic
in the size of the tree, making the cost O(nlog(n)).

There are at least two possible ways in which we could implement this
'leap' feature: first, as a \"local\" traversal from the starting
position, keeping track of changes to the height while traversing and
stopping when a suitable destination point is reached.  This would be
efficient since we would only need to determine the height once, at the
start, making it O(n).  However, this approach would require some
notion of 'memory' to be built into the DSL semantics, which at
present it lacks (representing a theoretical limitation on the types
of traversals expressible in the DSL in its present form).

A second way to do it is in \"global\" terms -- rather than keeping
track of changing height in the course of the traversal, instead,
determine always from a common reference point (the root) the current
height. This allows us to circumvent the need for 'memory' since this
information could be computed afresh at each step.  This latter
approach is the one employed here."
  (interactive)
  (let ((traverse (if soar
                      symex--traversal-postorder
                    symex--traversal-postorder-in-tree))
        (height (symex-height))
        (index (symex-index)))
    (let* ((find-neighboring-branch
            (symex-traversal
             (circuit (precaution traverse
                                  (afterwards (not (lambda ()
                                                     (= (symex-height)
                                                        height))))))))
           (run-along-neighboring-branch
            (symex-traversal
             (maneuver (decision (at first)
                                 find-neighboring-branch
                                 (maneuver symex--traversal-goto-first
                                           find-neighboring-branch))
                       traverse
                       symex--traversal-goto-first
                       (circuit (precaution (move forward)
                                            (beforehand (lambda ()
                                                          (< (symex-index)
                                                             index)))))))))
      (symex-execute-traversal
       (symex-traversal
        (precaution (maneuver run-along-neighboring-branch
                              (circuit
                               (precaution (decision (lambda ()
                                                       (< (symex-index)
                                                          index))
                                                     run-along-neighboring-branch
                                                     symex--move-zero)
                                           (beforehand (lambda ()
                                                         (< (symex-index)
                                                            index))))))
                    (beforehand (not (at root)))
                    (afterwards (lambda ()
                                  (and (= (symex-index)
                                          index)
                                       (= (symex-height)
                                          height))))))))))

(defun symex-leap-forward (&optional soar)
  "Leap forward to a neighboring branch, preserving height and position.

If SOAR is true, leap between trees too, otherwise, stay in the
current tree.

See the documentation on `symex-leap-backward` for details regarding
the implementation."
  (interactive)
  (let ((traverse (if soar
                      symex--traversal-preorder
                    symex--traversal-preorder-in-tree))
        (height (symex-height))
        (index (symex-index)))
    (let* ((find-neighboring-branch
            (symex-traversal
             (circuit (precaution traverse
                                  (afterwards (not (lambda ()
                                                     (= (symex-height)
                                                        height))))))))
           (run-along-neighboring-branch
            (symex-traversal
             (maneuver (decision (at last)
                                 find-neighboring-branch
                                 (maneuver symex--traversal-goto-last
                                           find-neighboring-branch))
                       traverse
                       (circuit (precaution (move forward)
                                            (beforehand (lambda ()
                                                          (< (symex-index)
                                                             index)))))))))
      (symex-execute-traversal
       (symex-traversal
        (precaution (maneuver run-along-neighboring-branch
                              (circuit
                               (precaution (decision (lambda ()
                                                       (< (symex-index)
                                                          index))
                                                     run-along-neighboring-branch
                                                     symex--move-zero)
                                           (beforehand (lambda ()
                                                         (< (symex-index)
                                                            index))))))
                    (beforehand (not (at root)))
                    (afterwards (lambda ()
                                  (and (= (symex-index)
                                          index)
                                       (= (symex-height)
                                          height))))))))))

(defun symex--selection-side-effects ()
  "Things to do as part of symex selection, e.g. after navigations."
  (interactive)
  (when symex-highlight-p
    (mark-sexp)))

(defun symex-selection-advice (orig-fn &rest args)
  "Attach symex selection side effects to a given function.

ORIG-FN could be any function that results in a symex being selected.
ARGS are the arguments that were passed to ORIG-FN (as any advice function
is expected to handle in Emacs)."
  (interactive)
  (let ((result (apply orig-fn args)))
    (symex--selection-side-effects)
    result))

(advice-add #'symex-go-forward :around #'symex-selection-advice)
(advice-add #'symex-go-backward :around #'symex-selection-advice)
(advice-add #'symex-go-up :around #'symex-selection-advice)
(advice-add #'symex-go-down :around #'symex-selection-advice)
(advice-add #'symex-goto-first :around #'symex-selection-advice)
(advice-add #'symex-goto-last :around #'symex-selection-advice)
(advice-add #'symex-goto-lowest :around #'symex-selection-advice)
(advice-add #'symex-goto-highest :around #'symex-selection-advice)
(advice-add #'symex-traverse-forward :around #'symex-selection-advice)
(advice-add #'symex-traverse-backward :around #'symex-selection-advice)
(advice-add #'symex-select-nearest :around #'symex-selection-advice)

(defun symex--remember-branch-position (orig-fn &rest args)
  "Remember branch position when descending the tree.

This pushes the current position onto a stack, which is popped
while ascending.

ORIG-FN applied to ARGS is the invocation being advised."
  (let ((position (symex-index)))
    (let ((result (apply orig-fn args)))
      (when result
        (push position symex--branch-memory))
      result)))

(defun symex--return-to-branch-position (orig-fn &rest args)
  "Return to recalled position on the branch.

ORIG-FN applied to ARGS is the invocation being advised."
  (let ((result (apply orig-fn args)))
    (when result
      (let ((position (pop symex--branch-memory)))
        (when position
          (symex--execute-tree-move (symex-make-move position 0)))))
    result))

(defun symex--clear-branch-memory ()
  "Clear the branch memory stack.

Technically, branch memory is tree-specific, and stored branch
positions are no longer relevant on a different tree than the one on
which they were recorded.  To be conservative and err on the side of
determinism here, we clear branch memory upon entering symex mode,
since may enter at arbitrary points in the code, i.e. on arbitrary
trees.

TODO: Yet, hypothetically if there were two identical trees next to
one another, then the positions from one would naturally carry over to
the other and in some sense this would be the most intuitive.  Thus,
an alternative could be to retain branch memory across trees so that
we attempt to climb each tree as if it were the last tree
climbed, which may in practice end up being more intuitive than
assuming no knowledge of the tree at all.

This may be worth exploring as a defcustom."
  (setq symex--branch-memory nil))

(defun symex--forget-branch-positions (orig-fn &rest args)
  "Forget any stored branch positions when moving to a different tree.

ORIG-FN applied to ARGS is the invocation being advised."
  (let ((result (apply orig-fn args)))
    (when result
      (setq symex--branch-memory nil))
    result))

(provide 'symex-misc)
;;; symex-misc.el ends here