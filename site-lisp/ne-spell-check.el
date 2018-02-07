;;; ne-spell-check.el --- Configure Emacs' spell-checking the way I want.

;;; Author: Chen Bin

;;; Version: 0.0.1

;;; Commentary:

;; Ripped and tweaked from
;;
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;;
;; because there appears to be no better way to acquire the code the author
;; proposes.
;;
;; Package is namespaced with my initials just so it's unlikely to collide with
;; a future package.
;;
;; It configures flyspell/ispell to allow run-together words in source code
;; buffers, to deal with camelCaseNames semi-sanely.
;;
;; Alas, it's only semi-sanely - when you ask for suggestions, you get a ton of
;; run-together suggestions, which is not what I really want.
;;
;; Also, when I use a high --run-together-limit and/or a low
;; --run-together-min, it can freeze Emacs. That seems to be due to causing a
;; segfault in the core aspell binary, because that's the result I see when I
;; screw with it more.
;;
;; Even as I have it now, it can still make things pretty slow, but it doesn't
;; seem to hang anymore.
;;
;; TODO File a bug with aspell for segfaulting.
;;
;; TODO File a bug on flyspell for freezing when aspell crashes. That should be
;; detectable.
;;
;; TODO Split camel-case words and send each subword to flyspell.

;;; Code:

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun ne-flyspell-detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words."

  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=3")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "-d en_US")))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-local-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (ne-flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (ne-flyspell-detect-ispell-args))
(defadvice ispell-word (around ne-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (ne-flyspell-detect-ispell-args t))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around ne-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (ne-flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (ne-flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

(provide 'ne-spell-check)
;;; ne-spell-check.el ends here
