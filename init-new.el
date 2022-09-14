;;; init-new.el --- a step towards a more robust Emacs config.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; My current Emacs configuration has become unwieldy, bloated, slow, and is
;; generally aging poorly.
;;
;; I know a lot more about how to configure Emacs than I did back in 2009, and
;; the state of the art has improved a lot since I first got serious about it.
;;
;; I am thus starting a project of trying a new, more streamlined init file, in
;; hopes that I'll be able to get to something that runs more quickly and with
;; fewer quirks (like the occasional screen blanking mentioned in todo.txt).
;;
;; I spawn test instances of it with `emacs -q --load ~/.emacs.d/init-new.el'.
;; Note the explicit invocations of (package-initialize) and (run-hooks
;; after-init-hook) to support running it that way, per
;; https://stackoverflow.com/a/17149070/1128957.

;;; Code:

;; Time Emacs startup, per https://blog.d46.us/advanced-emacs-startup/.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Temporarily make the GC threshold large, to speed up startup.
(setq ne/old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 2 1000 1000))

;; Set up my load path and a few other core things.
(load-file (concat user-emacs-directory "site-lisp/bootstrap.el"))

;; Initialize packages as if this is a normal init file.
;;
;; FIXME Remove when I promote this file to being my primary init.el
(package-initialize)

 ;; TODO Ignore site lisp files, as much as we can. Mac OS X Mojave included
 ;; some that made make-process take a minimum of five seconds, at least with
 ;; my setup. :/
 ;;
 ;; This change didn't actually fix that, but in principle I want my config to
 ;; be as isolated from the host system as possible. I wound up customizing an
 ;; Emacs build to do it
(setq inhibit-default-init t)

;; Enable useful functions that are disabled by default.
(put 'narrow-to-region 'disabled nil)

(use-package evil
  :commands evil-local-mode
  :config

  ;; Use regular emacs keybindings for insert-mode (except for ESC-ESC-ESC,
  ;; because vim keybindings are still vim).
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Trying out evil-smartparens. We'll see if it works out.
  (evil-sp-smartparens-config)

  ;; Always use a leader key, because the leader is awesome. See
  ;; my-keybindings.el for my actual leader keybindings.
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  ;; Turn on surround everywhere.
  (global-evil-surround-mode)

  ;; Use 'gx' for swapping vim textobjects/motions.
  (evil-exchange-install)

  ;; Add vim operator for commenting things.
  (evil-commentary-mode)

  (diminish 'evil-commentary-mode)

  ;; Set up my custom textobjects.
  (ne/install-textobjects))

(use-package evil-exchange
  :commands evil-exchange-install)

(use-package evil-commentary
  :commands evil-commentary-mode)

(use-package ne-evil-textobjects
  :commands ne/install-textobjects)

(use-package evil-smartparens
  :commands evil-sp-smartparens-config)


;; Text-editing modes of various stripes.
(defun text-mode-init ()
  "Configuration that is shared across my various text modes."

  ;; "Emacs is a great OS, but a terrible text editor.
  ;; Fortunately, it's possible to write a great text editor for a great OS..."
  ;; -- some wag discussing evil-mode
  (evil-local-mode)

  ;; Not all software uses git, but git-gutter does the right thing if it can't
  ;; find a parent git repo.
  (git-gutter-mode)

  (turn-on-flyspell)
  (ac-ispell-setup)
  (ac-ispell-ac-setup)
  (diminish 'flyspell-mode)

  (auto-fill-mode t)
  (aggressive-fill-paragraph-mode t)

  (smartparens-mode)

  ;; I occasionally want to use yasnippet in text mode.
  (yas-minor-mode))

(use-package text-mode
  :hook (text-mode . text-mode-init))

;; TODO It'd be nice if this hook didn't run when I'm just opening a buffer to
;; compile the file, like when installing packages. Advising the appropriate
;; functions would probably do it, but I'm not sure which ones need the advice.
(defun my-prog-mode-init ()
  "General setup for programming modes."

  ;; Set up basic text-mode niceties.
  (text-mode-init)

  (unicode-troll-stopper-mode)
  (diminish 'unicode-troll-stopper-mode)

  ;; Auto-fill comments, but not code. Note that this is impacted by
  ;; text-mode-init activating aggressive-fill-paragraph-mode, so that editing
  ;; comments flows like I'm in a word processor (at least in languages where
  ;; they work well).
  (comment-auto-fill)

  ;; Smartparens beat auto-pair after a long, hard struggle.
  (smartparens-mode t)
  (diminish 'smartparens-mode)

  ;; I want to have a personal spelling wordlist for technical jargon so that I
  ;; can include it only in contexts where it's reasonable to use it. Then,
  ;; presto - I get warned when I use jargon in non-technical writing.
  ;;
  ;; The below *almost* works, but I would need to kill the ispell process
  ;; whenever I'm changing buffers (or at least changing to a buffer that needs
  ;; a different set of dictionaries).
  ;;
  ;; Looks like ispell-kill-ispell should do that job fine, so long as starting
  ;; a new ispell process isn't a heavy drag on buffer swaps... :/
  ;;
  ;; There is no obvious on-buffer-change hook.
  ;; https://stackoverflow.com/a/47551055 has some notes on what could work for
  ;; detecting that.
  ;;
  ;; Paired with the buffer-local-value function, it should be possible to
  ;; restart the ispell process only when you want to change the arguments it's
  ;; using (i.e., when ispell-extra-args is different in the buffer you just
  ;; swapped to).

  ;; (setq-local ispell-extra-args (list "--lset-extra-dicts" (concat
  ;;                                                           (getenv "HOME")
  ;;                                                           "/"
  ;;                                                        ".aspell.software-jargon.pws")))

  ;; Everyone loves autocompletion.
  (auto-complete-mode t)
  (setq ac-sources (list ac-source-yasnippet
                         ac-source-words-in-same-mode-buffers))
  (diminish 'auto-complete-mode)

  ;; Get clickable code-folding indicators in the fringe.
  ;; DEBUG This is *deathly* slow when you open files over about 7000 lines.
  ;; Not sure if it's so much linecount as just lots of folding points?
  ;; Regardless, opening something like web-mode.el effectively freezes emacs
  ;; for minutes. If I could optimize it or give it some kind of cutoff, that
  ;; might make it usable again.
  ;;
  ;; (hideshowvis-minor-mode t)

  ;; Pressing RET should do newline-then-indent, and continue any comment we
  ;; happen to be in.
  (local-set-key (kbd "RET") 'indent-new-comment-line)

  ;; Flycheck does on-the-fly syntax checking, if the appropriate tools are
  ;; installed.
  (flycheck-mode t)

  ;; For programming, it's convenient if your editor guesses indentation styles
  ;; automatically.
  (guess-style-guess-all)

  ;; Make camelCaseNames more readable. See custom.el for settings.
  (glasses-mode)

  ;; Similarly, make camelCaseNames more navigable (for vanilla Emacs - looks
  ;; like I need evil-little-word to let evil behave this way).
  (subword-mode t)

  ;; It doesn't work everywhere, but which-function-mode is nice when it does.
  (which-function-mode t)

  ;; Highlight TODO and friends as warnings. Yanked from this blog post:
  ;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|DEBUG\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\\>"
          1 font-lock-string-face t)))

  ;; Turn on smart-dash-mode if it's not a bad idea in our current mode.
  (if (not (member major-mode '(emacs-lisp-mode
                                lisp-mode
                                lisp-interaction-mode
                                css-mode
                                scss-mode)))
      (progn
        (smart-dash-mode)
        (diminish 'smart-dash-mode))))

(use-package prog-mode
  :hook my-prog-mode-init)

;; Put results from sexp evaluation in the *scratch* buffer inside a comment.
;; This makes the whole buffer syntactically-valid and is just cleaner.
(use-package scratch-comment
  :bind (:map lisp-interaction-mode-map
              ("C-j" . scratch-comment-eval-sexp)))

(use-package eldoc
  :diminish)

(defun emacs-lisp-init ()
  "Hook function for `emacs-lisp-mode'."

  (my-prog-mode-init)

  (setq mode-name "elisp")

  (elisp-slime-nav-mode)
  (diminish 'elisp-slime-nav-mode)
  (ac-emacs-lisp-mode-setup)
  (setq-local ne-yas-auto-insert-snippet-name "package")

  (push '(?\` . ("`" . "'")) evil-surround-pairs-alist)

  (eldoc-mode t)
  (diminish 'eldoc-mode))

(use-package lisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("/Cask\\'" . emacs-lisp-mode))
  :config
  ;; Wait to set up elisp customizations until initialization is done.
  ;;
  ;; Otherwise, the various elisp buffers like *scratch* that appear before my
  ;; config is finished loading throw errors for all the dependencies that
  ;; aren't loaded yet.
  ;;
  ;; This still results in *scratch* having the extras, though, since the hook
  ;; runs first time it's set. For a workaround, this is a pretty seamless one.
  (add-hook 'after-init-hook
            #'(lambda ()
                (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init))))

;; Restore the previous gc-cons-threshold, for day-to-day operations.
(setq gc-cons-threshold ne/old-gc-cons-threshold)

(provide 'init-new)
;;; init-new.el ends here

;; Pretend this file is a normal init file.
;;
;; FIXME Delete this before promoting this file to be my default init.el.
(run-hooks after-init-hook)
