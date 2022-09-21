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

;; On macOS, explicitly use a dark menubar. Added because my Nix-built Emacs
;; 28.1 no longer got this right by default, and boy is the white menubar
;; eye-burning.
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defun add-subdirs-to-front-of-load-path (path)
  "Add directories beneath PATH to the beginning of load-path."
  (let ((default-directory path))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
                (normal-top-level-add-subdirs-to-load-path))
                 load-path))
    (setq load-path (append (list path) load-path))))

(add-subdirs-to-front-of-load-path (concat user-emacs-directory "site-lisp"))

(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.org/packages/"))

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


;;;
;;; Configure miscellaneous bits of Emacs built-in behaviors.
;;;


;; When in doubt, choose the newer of a compiled vs. a source elisp file.
(setq load-prefer-newer t)

(defvar my-autosaves-dir (concat user-emacs-directory "autosaves/")
  "Path to my autosaves directory.

It's defined in here because I need it to precede usage in config-packages.el.")

;;;
;;; Install a bunch of manually-maintained autoloads. Yeah, this is dumb.
;;;

;; Set up manually-maintained autoloads.
(defun nateeag-autoloads-init ()
  "Define Nate Eagleson's manually-maintained autoloads.

These are mostly for lazy-loading registration of mode hooks, but a few of them
are for modes that didn't come with autoloading."

  (autoload 'auto-complete-init "auto-complete-init.el")
  (autoload 'web-mode-init "web-mode-init.el")
  (autoload 'comment-auto-fill "comment-auto-fill.el")
  (autoload 'emacs-lisp-init "emacs-lisp-init.el")
  (autoload 'js-mode-init "js-mode-init.el")
  (autoload 'hs-minor-mode-init "hs-minor-mode-init.el")
  (autoload 'yasnippet-init "yasnippet-init.el")
  (autoload 'php-mode-init "php-mode-init.el")
  (autoload 'smartparens-init "smartparens-init.el")
  (autoload 'emmet-mode-init "emmet-mode-init.el")
  (autoload 'css-mode-init "css-mode-init.el")
  (autoload 'smart-dash-mode "smart-dash.el" "Smart Dash mode")

  ;; Autoloads for eclim preferences and eclimd.
  ;; eclimd should have come with autoloads, in principle, but it didn't.
  (autoload 'start-eclimd "eclimd.el" nil t)

  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (autoload 'rst-mode "rst-mode.el")
  (autoload 'markdown-mode "markdown-mode.el")
  (autoload 'tern-mode "tern.el" nil t)

  ;; Autoloads for guess-style.
  (autoload 'guess-style-set-variable "guess-style" nil t)
  (autoload 'guess-style-guess-variable "guess-style")
  (autoload 'guess-style-guess-all "guess-style" nil t)

  ;; Autoloads for svn-commit-msg-mode, which I just grabbed from some guy's
  ;; .emacs.d.
  (autoload 'svn-msg-mode "svn-msg" nil t)

  ;; I use python-mode.el, with the TQS-coloration patch applied.
  ;; I should probably try installing the latest version and seeing how it
  ;; holds up.
  (autoload 'python-mode "python-mode" "Python editing mode." t)

  ;; tea-time's autoloads, despite being installed from MELPA, don't seem to
  ;; work. Therefore...
  (autoload 'tea-timer "tea-time.el")

  ;; Manual autoloads for sdcv-mode, a dictionary lookup tool I use for access
  ;; to Webster's 1913 dictionary. Also an alias because I keep forgetting the
  ;; command I need to actually do this lookup.
  (autoload 'sdcv-search "sdcv-mode.el" "Look up words in dictionary." t)
  (defalias 'ne-dictionary-lookup 'sdcv-search)

  ;; I can never remember synosaurus' name.
  (defalias 'ne/synonym-lookup 'synosaurus-lookup)
  (defalias 'ne/thesaurus-lookup 'synosaurus-lookup)

  (autoload 'update-packages-update-installed-packages "update-packages" nil t))

(nateeag-autoloads-init)


;;;
;;; Autosave configuration
;;;

;; Autosave's defaults are not very nice. Here, we fix them.
;; Create autosave dir if it doesn't exist.
;;
;; TODO Pull this out into a package proper. Maybe someone's already built it?
;; This config is many years old now.
;;
;; TODO Put autosaves outside my .emacs.d. I don't back .emacs.d up, since I
;; have it on GitHub, but I should really back up my backups...
;;
;; Figure out when to suppress autosaves if editing in a file that's sometimes
;; updated by an external process while you're in it - this autosave setup can
;; wind up racing with that, and sometimes it wins. Basically I need to add a
;; second suppression function to the one I already have for when files
;; disappear due to changing branches, but I'm not sure what the logic in this
;; suppression function would be. Something involving auto-revert-mode?

(defvar my-autosaves-dir (concat user-emacs-directory "autosaves/")
  "Path to my autosaves directory.

It's defined in here because I need it to precede usage in config-packages.el.")

(make-directory my-autosaves-dir t)

(setq
   ; Don't clobber symlinks.
   backup-by-copying t

   ; Don't break multiple hardlinks.
   backup-by-copying-when-linked t

   ; Don't litter the filesystem with backups *or* autosaves.
   backup-directory-alist
    `(("." . ,my-autosaves-dir))

   auto-save-file-name-transforms
   `((".*" ,(concat my-autosaves-dir "\\1") t))

   ;; Never auto-delete backups, so the backup-walker package is as useful as
   ;; possible.
   delete-old-versions -1

   ; use versioned backups
   version-control t)

;; Back up files even when using version control.
(setq vc-make-backup-files t)

(defun force-buffer-backup ()
  "Force buffer to back up on next save."

  (setq buffer-backed-up nil))

;; Back up buffers on every save.

(add-hook 'before-save-hook 'force-buffer-backup)

(defun maybe-reset-major-mode ()
  "Reset the buffer's major-mode if a different mode seems like a better fit.

Mostly useful as a before-save-hook, to guess mode when saving a
new file for the first time."

  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))

(add-hook 'before-save-hook 'maybe-reset-major-mode)

;; If a file looks scripty and it isn't executable at save time, make it so.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Save when Emacs loses focus, when I change buffers, when I change windows,
;; and when it's been idle for a while.
;;
;; TODO Turn this into a standalone package? Not sure if anyone else would care
;; to use it...

(defun ne/should-autosave-buffer (buffer)
  "Return `nil' if the passed buffer should not be autosaved.

Primarily used to avoid autosaving buffers whose buffer-file-name
is not an existing file, as such orphaned buffers tend to crop up
when changing branches in git."

  (when (buffer-file-name buffer)
    (file-exists-p (buffer-file-name buffer))))

(defun ne/save-when-file (&rest args)
  "Save current buffer if it points to an existing file.

Accepts unused `args' so it can be used as advice for arbitrary functions."

  (when (ne/should-autosave-buffer (current-buffer))
    (save-buffer)))

(defun ne/advise-focus-autosave-should-save-p (old-function &rest arguments)
  "Prevent `focus-autosave-should-save-p' from running when the passed
buffer's file does not exist."

  (when (ne/should-autosave-buffer (nth 0 arguments))
                  (apply old-function arguments)))

(advice-add 'other-window :before #'ne/save-when-file)
(advice-add 'switch-to-buffer :before #'ne/save-when-file)
(advice-add 'focus-autosave-should-save-p
            :around
            #'ne/advise-focus-autosave-should-save-p)

(focus-autosave-mode t)
(diminish 'focus-autosave-mode)
;; Save this so I can turn off the save-on-idle feature if need be.
(setq ne/save-on-idle-timer (run-with-idle-timer 5 t 'focus-autosave-save-all))


;;;
;;; Configure third-party packages I depend on.
;;;

;; Help me keep track of what I spend my time doing.
(use-package activity-watch-mode
  :diminish
  :init (global-activity-watch-mode))

;; Helpful is a massive improvement over the baseline Emacs help functions
;; (which are themselves a massive improvement over what most programs offer).
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; A hack to work around never properly declaring my-functions as a package or
;; setting up autoloads for it.
(use-package s
  :commands s-replace s-trim)

(use-package my-functions
  :commands
  hit-servlet comment-or-uncomment-region-or-line wrap-args
  move-current-buffer insert-date insert-time unfill-paragraph
  add-auto-mode ne/set-theme-to-match-system-theme)

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  ;; Configuration to make moody's modeline decor look right.
  ;;
  ;; FIXME I need this to run every time I change themes. I don't do it often
  ;; but I do occasionally use solarized-light.
  ;;
  ;; If this gets annoying, try setting this up via add-advice:
  ;;
  ;; https://emacs.stackexchange.com/questions/29679/faces-not-set-immediately-after-load-theme
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline line)
    (set-face-attribute 'mode-line nil :underline line)
    (set-face-attribute 'mode-line-inactive nil :overline line)
    (set-face-attribute 'mode-line-inactive nil :underline line)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Make it easy to find the cursor after scrolling.
;;
;; FIXME I think beacon-mode has caused some graphical glitches in my config.
;; Keep an eye on this.
(use-package beacon
  :init (beacon-mode 1)
  :diminish beacon-mode)

;; evil-collection complains if this variable is not bound before loading it.
;;
;; I'm not sure what setting I actually want from it.
(setq evil-want-keybinding nil)
(use-package evil
  :commands evil-local-mode
  :after evil-smartparens
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

(use-package evil-collection)

(use-package evil-smartparens
  :commands evil-sp-smartparens-config)

(use-package my-keybindings
  ;; Because I define bindings based on evil-mode, this should only happen after
  ;; evil-mode loads.
  :after evil)

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


;;;
;;; Version control tooling.
;;;

(use-package svn-msg
  :mode ("svn-commit\(.[[:digit]]+\)*.tmp" . svn-msg-mode))

(use-package git-gutter
  :diminish
  :config
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (add-to-list 'git-gutter:update-hooks 'magit-post-refresh-hook))

(use-package magit-delta
  :diminish
  :config
  (setq magit-delta-default-dark-theme "Solarized (dark)"))

(use-package magit
  :hook ((magit-mode . magit-svn-mode)
         (magit-status-mode . evil-local-mode)
         (magit-rebase-mode . evil-local-mode))
  :config
  (evil-collection-magit-setup)
  (magit-delta-mode)
  ;; I never use magit's gitignore editing and because evil-collection doesn't
  ;; have support for everything I want to do from evil-normal-state, I change
  ;; to evil-insert-state sometimes.
  ;;
  ;; Specifically I can't jump to end-of-line/start-of-line in
  ;; normal-state because magit binds '$', '^' and '0'. I also can't
  ;; trigger magit-svn with its default binding of 'N', because evil
  ;; rightfully binds 'N' to evil-search-previous.
  (define-key magit-status-mode-map (kbd "i") 'evil-insert-state))

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


;;; Communications packages.

;; How can I trigger evil-local-mode in elfeed-search-mode? It doesn't seem to
;; have a hook...

(use-package elfeed
  :after (evil-collection)
  :hook ((elfeed-search-mode . evil-local-mode)
         (elfeed-show-mode . evil-local-mode))
  :config (evil-collection-elfeed-setup))

(use-package notmuch
  ;; I don't exactly love emoji, but people use them in emails, so I guess I'd
  ;; rather see what they're communicating than not.
  :hook ((notmuch-search-mode notmuch-show-mode message-mode) . emojify-mode)
  :config
  (progn
    ;; TODO Stop marking deleted and spam messages as read.
    ;;
    ;; I'm marking them as read just to keep the gmail inbox semi-usable, and
    ;; to make sure the next time I change my sync program and therefore have
    ;; to re-index from scratch, it's not as *much* of a disaster, in that I'll
    ;; at least have a clear record of what I've processed and what I haven't.
    ;;
    ;; ...granted, the right answer here is to figure out how to sync notmuch
    ;; tags to IMAP folders. Someday.
    (define-key notmuch-search-mode-map "d"
      (lambda (&optional beg end)
        "mark message as deleted"
        (interactive)
        (notmuch-search-tag (list "+deleted" "-unread") beg end)))

    (define-key notmuch-search-mode-map "r"
      (lambda (&optional beg end)
        "mark message as an archived receipt"
        (interactive)
        (notmuch-search-tag (list "+receipts" "-unread" "-inbox") beg end)))

    (define-key notmuch-search-mode-map "s"
      (lambda (&optional beg end)
        "mark message as spam"
        (interactive)
        (notmuch-search-tag (list "+spam" "-inbox" "-unread") beg end)))

    ;; Be evil-ish, because I want that.
    (define-key notmuch-show-mode-map "j" 'next-line)
    (define-key notmuch-show-mode-map "k" 'previous-line)
    (define-key notmuch-search-mode-map "j" 'next-line)
    (define-key notmuch-search-mode-map "k" 'previous-line)

    ;; Make it easier to open attachments in the corresponding tool.
    (define-key notmuch-show-mode-map (kbd "o")
      'notmuch-show-interactively-view-part)

    (define-key notmuch-show-mode-map (kbd "D")
      (lambda ()
        "Delete current message and move to the next message or thread.

TODO Make this delete all messages in buffer, a la notmuch-show-archive-thread-then-next?"
        (interactive)
        (notmuch-show-tag (list "+deleted" "-inbox" "-unread"))
        (unless (notmuch-show-next-open-message)
          (notmuch-show-next-thread t))))

    (define-key notmuch-show-mode-map "w"
      (lambda ()
        "Mark message as watched and reply to the sender."
        (interactive)
        (notmuch-show-tag (list "+watched"))
        (notmuch-show-reply-sender)))

    (define-key notmuch-show-mode-map "W"
      (lambda ()
        "Mark message as watched and reply to all."
        (interactive)
        (notmuch-show-tag (list "+watched"))
        (notmuch-show-reply)))

    ;; I already have a keybinding for 'archive thread', so rebind Spacebar to
    ;; just let me advance the thread and move to the next one when I'm done
    ;; with it, rather than moving to the next one and archiving.
    (define-key notmuch-show-mode-map (kbd "SPC")
      (lambda ()
        "Move to the next message or thread.

I feel like this should be built-in somewhere to notmuch-mode, but I haven't
found it."
        (interactive)

        (if (notmuch-show-advance)
            (notmuch-show-next-thread t))))

    ;; Download an attachment locally.
    ;;
    ;; A rebinding of the standard 'w' keybinding, since I use 'w' for
    ;; something else."
    (define-key notmuch-show-mode-map "d" 'notmuch-show-save-attachments)

    ;; This keeps my Shift-Tab keybinding for completing addresses from
    ;; auto-selecting the wrong address, while still leaving a default
    ;; selected. Judging from completing-read's docs, this is the way it ought
    ;; to be done.
    ;;
    ;; TODO Submit this trivial patch upstream?
    (defun notmuch-address-selection-function (prompt collection initial-input)
      "Call (`completing-read'
      PROMPT COLLECTION nil nil INITIAL-INPUT 'notmuch-address-history)"
      (completing-read
       prompt collection nil nil nil 'notmuch-address-history initial-input))
    ))

(use-package notmuch-mua
  :config
  (defun ne/notmuch-tag-sent-emails (&optional arg)
    "Use shell command to tag sent emails as sent and read.

The shell command lives in my dotfiles repo."

    (start-process "tag-sent-emails" nil "tag-sent-emails"))

  (advice-add 'notmuch-mua-send-and-exit :after #'ne/notmuch-tag-sent-emails)

  (require 'notmuch-address)
  (notmuch-address-setup)

  ;; TODO Get address completion to work correctly. I can trigger it now, but
  ;; it assumes I want the first result, which is not often true and thus
  ;; forces me to type more than I want to.
  (define-key message-mode-map
    (kbd "<backtab>")
    '(lambda () (interactive) (notmuch-address-expand-name))))

(use-package message-attachment-reminder)


;;;
;;; Low-importance packages I like having available.
;;;

;; I don't know that I'll ever use this, but why not?
(use-package rfc-mode
  :commands rfc-mode-browse)


;; Restore the previous gc-cons-threshold, for day-to-day operations.
(setq gc-cons-threshold ne/old-gc-cons-threshold)

(provide 'init-new)
;;; init-new.el ends here

;; Pretend this file is a normal init file.
;;
;; FIXME Delete this before promoting this file to be my default init.el.
(run-hooks after-init-hook)
