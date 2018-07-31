;;; -*- lexical-binding: t; -*-
;;; aggressive-fill-paragraph.el --- A mode to automatically keep paragraphs filled

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20160301.1414
;; Package-Requires: ((dash "2.10.0"))
;; URL: https://github.com/davidshepherd7/aggressive-fill-paragraph-mode
;; Keywords: fill-paragraph, automatic, comments

;;; Commentary:

;; An emacs minor-mode for keeping paragraphs filled in both comments and prose.

;;; Code:

(require 'dash)


;; Helpers

(defun afp-inside-comment? ()
  (nth 4 (syntax-ppss)))

(defun afp-outside-comment? ()
  (not (afp-inside-comment?)))

(defun afp-get-comment-bounds ()
  "Return a list containing the current comment's start and end points.

Return `nil' if point is not currently inside a comment.

This returns nil in single-line comment blocks if point is on
a comment-start sequence, because syntax-ppss doesn't think those are
comment characters (and I have to admit it has a point). At first I
thought that was an issue but I'm no longer convinced of that.

TODO Factor this out to a standalone package, because it isn't specific to
aggressive-fill-paragraph."

  (when (afp-inside-comment?)
    (save-excursion
      (let ((comment-start-pos)
            (comment-end-pos))
        ;; Move to beginning of current comment. In a multiline comment block,
        ;; this puts point directly after the comment starter - in a single-line
        ;; comment block, it does the same, but since each line of the block is
        ;; considered an independent comment, you may be in the middle of the
        ;; comment.
        ;;
        ;; However, since we're at *a* comment start either way...
        (goto-char (comment-beginning))

        ;; ...comment-forward with a negative buffer-size get us all the way
        ;; back to the first non-comment-or-whitespace character before the
        ;; comment block we were in to start with.
        (forward-comment (* -1 (buffer-size)))

        ;; At this point, if we search to the first non-whitespace character
        ;; (including newlines) then go back one character, we should be at the
        ;; actual start of the current comment.
        (re-search-forward "[^[:space:]\n\r]")
        (backward-char)

        (setq comment-start-pos (point))

        ;; Finally, we can now use forward-comment to move to the first
        ;; non-whitespace-or-comment character *after the current comment-block.
        (forward-comment (buffer-size))

        ;; ...which means searching backward for the first non-whitespace
        ;; character takes us to the comment-closer (and we have to move
        ;; forward a char afterwards so we're actually after it).
        (re-search-backward "[^[:space:]\n\r]")
        (forward-char)
        (setq comment-end-pos (point))

        ;; Return the comment's start and end positions.
        (list comment-start-pos comment-end-pos)))))

(defun afp-comment-only-mode? ()
  (apply #'derived-mode-p afp-fill-comments-only-mode-list))

(defun afp-current-line ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


;; Functions for testing conditions to suppress fill-paragraph

(defun afp-point-in-blank-lines? ()
  "Return true if point is on a blank line.

This is a workaround to avoid filling of the next paragraph after
deleting the current one, because in its current state the super
aggressive fill winds up filling the next paragraph right after a
delete of the current one, which is not what we want.

TODO Look for a cleaner solution. This is only needed after
deletion, so it definitely feels like a workaround."

  ;; TODO Rename this as afp-point-on-blank-line? if this logic works out.
  (looking-at "^$"))

(defun afp-outside-comment-and-comment-only-mode? ()

  (and (afp-comment-only-mode?) (afp-outside-comment?)))

(defun afp-markdown-inside-code-block? ()
  """Basic test for indented code blocks in markdown."""
  (and (string-equal major-mode "markdown-mode")
       (string-match-p "^    " (afp-current-line))))

(defun afp-start-of-paragraph? ()
  "Return non-nil if we are starting a new paragraph in a comment.

In programming modes that don't include a trailing space as part
of the fill prefix (e.g. when it's just '#', not '# '), this
makes it possible to start new paragraphs while
`afp-fill-on-self-insert' is non-nil.

Without this function in afp-suppress-fill-pfunction-list, it's
annoying to do that, because pressing the spacebar to add the
leading space manually results in a fill, deleting the new lines.

TODO Figure out if this is just a hack. The problem lies in how
we treat space characters while writing comments, and I'm not
convinced this is a sane solution."

  (string-match-p (concat "^\\s-*" comment-start "\\s-*$") (afp-current-line)))

(defun afp-in-bulleted-list? ()
  "Guess whether we are editing a bulleted list.

Tries to handle comments and regular text, which may be a poor decision.

TODO: handle modes with multi-line comment syntaxes correctly.
This does not work in C-style comments."
  (if (afp-inside-comment?)
       ;; TODO: extend to match any line in paragraph
       (string-match-p (concat "^\\s-*" comment-start "\\s-*[-\\*\\+]")
                       (afp-current-line))
    (string-match-p (concat "^\\s-*[-\\*\\+]") (afp-current-line))))

(defun afp-in-formatted-paragraph? ()
  "Return non-nil if we are in a paragraph that looks hand-formatted.

The current heuristic is just 'Does it have some lines that are
significantly narrower than the fill-column?'

There may be a better one awaiting discovery."

  (save-excursion
    (forward-paragraph)

    (let* ((end (point))
           (start (progn (backward-paragraph)
                         (point)))
           (lengths))
      (narrow-to-region start end)
      ;; Line length calculation code yanked from
      ;; https://emacs.stackexchange.com/a/17848/351
      ;;
      ;; It would be better to ignore lines that are just a fill-prefix, as
      ;; empty comment lines don't tell you much about whether text is being
      ;; formatted.
      (while (not (eobp))
        (push (- (line-end-position)
                 (line-beginning-position))
              lengths)
        (forward-line))
      (widen)

      ;; FIXME Remove dependency on dash.el? Just trying to get this working
      ;; for now.
      (and (> (length lengths) 1)
           ;; Are all lines other than the last one more than 10 characters shy
           ;; of fill-column? The last line of a paragraph tends to be a widow,
           ;; and thus very short, which is why we ignore it.
           ;;
           ;; This logic does not work in source code comments, at least not in
           ;; sh-mode or emacs-lisp-mode. Looks like I'd need a
           ;; forward/backward paragraph that accounted for fill-prefixes when
           ;; computing paragraph boundaries.
           ;;
           ;; ...it's peculiar that fill-paragraph works in comments but
           ;; forward-paragraph and backward-paragraph don't.
           (-all? (lambda (x)
                    (< x (- fill-column 10)))
                  ;; Ignore the last line, since it's usually a widow.
                  (-slice lengths 0 -1))))))

;; Org mode tables have their own filling behaviour which results in the
;; cursor being moved to the start of the table element, which is no good
;; for us! See issue #6.
(require 'org)
(declare-function org-element-type "org-element" element)
(defun afp-in-org-table? ()
  (interactive)
  (and (derived-mode-p 'org-mode)
       (or (eql (org-element-type (org-element-at-point)) 'table)
           (eql (org-element-type (org-element-at-point)) 'table-row))))

(defcustom afp-fill-on-self-insert
  nil
  "If non-nil, `afp-fill-paragraph' fills after a character is inserted by
typing it directly.

This is in contrast to the default behavior, which is to fill only after
characters in `afp-fill-keys' are typed."
  :group 'aggressive-fill-paragraph)

(defcustom afp-fill-after-functions
  nil
  "A list of functions that should fill the paragraph after running.

Note that `delete-region' will have no effect if entered here - see
`afp-advise-filled-functions' for an explanation of why."
  :group 'aggressive-fill-paragraph
  :type '(repeat function))

(defcustom afp-suppress-fill-pfunction-list
  (list
   #'afp-markdown-inside-code-block?
   #'afp-point-in-blank-lines?
   #'afp-start-of-paragraph?
   #'afp-in-bulleted-list?
   #'afp-in-formatted-paragraph?
   #'afp-in-org-table?
   #'afp-outside-comment-and-comment-only-mode?
   )
  "List of predicate functions of no arguments, if any of these
  functions returns false then paragraphs will not be
  automatically filled."
  :group 'aggressive-fill-paragraph)

(defcustom afp-fill-comments-only-mode-list
  (list 'emacs-lisp-mode 'sh-mode 'python-mode 'js-mode)
  "List of major modes in which only comments should be filled."
  :group 'aggressive-fill-paragraph)

(defcustom afp-fill-keys
  (list ?\ ?.)
  "List of keys after which to fill paragraph."
  :group 'agressive-fill-paragraph)



;; The main functions


(defun afp-only-fill-comments (&optional justify)
  "Replacement fill-paragraph function which only fills comments
and leaves everything else alone."
  (if (afp-inside-comment?)
      (fill-comment-paragraph justify))

  ;; returning true says we are done with filling, don't fill anymore
  t)


(defun afp-suppress-fill? ()
  "Check all functions in afp-suppress-fill-pfunction-list"
  (-any? #'funcall afp-suppress-fill-pfunction-list))


;; Tell the byte compiler that these functions exist
(declare-function ess-roxy-entry-p "ess-roxy" nil)
(declare-function ess-roxy-fill-field "ess-roxy" nil)

(defun afp-ess-fill-comments ()
  "Fill comments in ess-mode (for R and related languages),
taking care with special cases for documentation comments."
  ;; Make sure we have the required libraries (this function is only run
  ;; when (derived-mode-p 'ess-mode) so we should!)
  (require 'ess-mode)
  (require 'ess-roxy)

  (if (ess-roxy-entry-p)
      (ess-roxy-fill-field)
    (afp-only-fill-comments)))


(defun afp-choose-fill-function ()
  "Select which fill paragraph function to use"
  (cond

   ;; In certain modes it is better to use afp-only-fill-comments to avoid
   ;; strange behaviour in code.
   ;;
   ;; I have commented this out and hacked up an implementation of the
   ;; only-fill-comments behavior that works in js2-mode based on a suppression
   ;; function, because this logic overrides the use of fill-paragraph-function
   ;; in js2-mode, which means that /*-style comments are not filled correctly
   ;; (unless you run fill-paragraph by hand).
   ;;
   ;; In some sense doing this via a suppression function is more elegant.
   ;;
   ;; However, I'm not confident that it has the desired behavior in all the
   ;; modes that rely on afp-fill-comments-only-mode-list. Will have to figure
   ;; that out. Only triggering a fill inside a comment may not be the same
   ;; thing as only filling the comment - I can imagine a (poorly-written?)
   ;; fill function that fills both the comment and the code immediately
   ;; following it.
   ;;
   ;; ((apply #'derived-mode-p afp-fill-comments-only-mode-list)
   ;; #'afp-only-fill-comments)

   ;; For python we could also do something with let-binding
   ;; python-fill-paren-function so that code is left alone. This would
   ;; allow docstrings to be filled, but unfortunately filling of strings
   ;; and docstrings are both handled by the same chunk of code, so even
   ;; normal strings would be filled.

   ((derived-mode-p 'ess-mode) #'afp-ess-fill-comments)

   ;; Use the buffer local fill function if it's set
   ((not (null fill-paragraph-function)) fill-paragraph-function)

   ;; Otherwise just use the default one
   (t #'fill-paragraph)))

(defun afp-fill-paragraph (&rest args)
  "If this mode is active, fill a paragraph with the appropriate fill function.

Primarily intended for use as advice to commonly-used functions like
`kill-region' and `yank', to keep things properly filled all the time.

Note, however, that `delete-region' cannot be consistently advised.
See `afp-advise-filled-functions' for a discussion of why."

  (when (and aggressive-fill-paragraph-mode (not (afp-suppress-fill?)))
    (funcall (afp-choose-fill-function))))

(defun aggressive-fill-paragraph-post-self-insert-function ()
  "Fill paragraph when space is inserted and fill is not disabled
for any reason."
  (when (and (or (and afp-fill-on-self-insert
                      ;; do not fill after whitespace, so that making new
                      ;; paragraphs always works. Just a dumb hack to make my
                      ;; life a little better - still need to think more about
                      ;; how best to do this.
                      (not (-contains? '(?\n ?\s ?\t) last-command-event)))
                 (-contains? afp-fill-keys last-command-event))
             (not (afp-suppress-fill?)))

    ;; If the new character is whitespace, delete it before filling and
    ;; reinserting the characters. This works around cases where filling
    ;; removes whitespace.
    ;;
    ;; TODO Find more robust way to check "is it whitespace". There must be one
    ;; built into Emacs.
    (when (memq last-command-event '(?\s ?\t))
      (backward-delete-char 1))

    (afp-fill-paragraph)

    (when (memq last-command-event '(?\s ?\t))
      (insert last-command-event))))



;; Minor mode set up
;;;###autoload
(define-minor-mode aggressive-fill-paragraph-mode
  "Toggle automatic paragraph fill when spaces are inserted in comments."
  :global nil
  :group 'electricity

  (if aggressive-fill-paragraph-mode
      (add-hook 'post-self-insert-hook
                #'aggressive-fill-paragraph-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'aggressive-fill-paragraph-post-self-insert-function t)))

(defun afp-advise-filled-functions ()
  "Advise each function in `afp-fill-after-functions' to fill after running.

This makes it possible to work like you're in a word processor, by
having deletes and pastes trigger filling.

Note, however, that advising `delete-region' does not work reliably,
because advice cannot be applied to native functions with their
own bytecode operation, at least not in the face of byte-compiled
elisp:

http://nullprogram.com/blog/2013/01/22

The only workaround I can think of for this is re-implementing
`delete-region' in Emacs Lisp, as it would then be an advisable
function even in byte-compiled code.

However, that sounds like crazy talk."

  (dolist (target-function afp-fill-after-functions)
    (advice-add target-function :after #'afp-fill-paragraph)))

(defun afp-set-default-fill-after-functions ()
  "Set `afp-fill-after-functions' to the standard values for
  word-processor-style filling.

TODO Actually install the hooks after this function is called?
It's mainly intended as an easy setup aid, so that might be the
smart thing to do."

  (setq afp-fill-after-functions '(backward-delete-char
                                   backward-delete-char-untabify
                                   kill-region
                                   yank
                                   yank-pop)))

;;;###autoload
(defun afp-setup-recommended-hooks ()
  "Install hooks to enable aggressive-fill-paragraph-mode in recommended major modes."
  (interactive)

  (add-hook 'text-mode-hook #'aggressive-fill-paragraph-mode)
  (add-hook 'prog-mode-hook #'aggressive-fill-paragraph-mode))


(provide 'aggressive-fill-paragraph)

;;; aggressive-fill-paragraph.el ends here
