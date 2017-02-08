;; My smartparens config.

;; Yoinked from a github issue related to smartparens:
;; https://github.com/bbatsov/prelude/issues/374
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-smartparens-keybindings ()
  "Set up smartparens keybindings the way I want."
  ;; This is mostly smartparen's defaults, but my spinal cord really wants
  ;; M-backspace to kill a word backwards.
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

  (define-key sp-keymap (kbd "C-S-d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "C-S-a") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-M-d") 'sp-end-of-sexp)

  (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
  (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

  (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)

  (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

  (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

  (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
  (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp))

(defun ne-sp-point-after-colon (id action context)
  "Return true if point is after a colon character. Else, return nil.

Used to suppress pairing parens if I'm typing a frowny-face."
  (when (eq action 'insert)
    (sp--looking-back-p (concat ":" (regexp-quote id)))))

(defun smartparens-init ()
  "My general smartparens settings, many snagged from the default config."

  (interactive)

  (diminish 'smartparens-mode)

  ;; Highlighting is cute, but I don't find it actually makes my life any
  ;; simpler.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Turn off quote auto-escaping, because it does the wrong thing in too many
  ;; circumstances.
  ;;
  ;; For instance, it shouldn't auto-escape double quotes in a single-quoted
  ;; string.
  ;;
  ;; I also find it annoying when I'm at EOL and manually insert a closing and
  ;; it's automatically turned into a pair of escaped quotes. That one's not so
  ;; soluble, but it bothers me more than it helps me.
  (setq sp-autoescape-string-quote nil)

  ;; Do not autopair parens when typing the following: :(
  (sp-pair "(" ")" :unless '(ne-sp-point-after-colon))

  ;; do not autoinsert ' pair if the point is preceded by word.  This
  ;; will handle the situation when ' is used as a contraction symbol in
  ;; natural language.  Nil for second argument means to keep the
  ;; original definition of closing pair.
  (sp-pair "'" nil :unless '(sp-point-after-word-p))

  ;; emacs is lisp hacking enviroment, so we set up some most common
  ;; lisp modes too
  (sp-with-modes sp--lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it serve as
    ;; hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p)))

  ;; '{' is almost always a block delimiter, so let's try turning this on
  ;; pseudo-globally.
  (sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  ;; Set up sp-keybindings.
  (my-smartparens-keybindings))
