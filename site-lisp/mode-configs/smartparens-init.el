;; My smartparens config.

;; Yoinked from a github issue related to smartparens:
;; https://github.com/bbatsov/prelude/issues/374
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun smartparens-init ()
  "My general smartparens settings, many snagged from the default config."

  (interactive)

  (require 'smartparens)

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
  (sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))
