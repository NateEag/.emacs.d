;; config-packages.el --- configure my Emacs packages.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just a big collection of use-package invocations.

;;; Code:

(require 'use-package)

(use-package s
  :commands s-replace s-trim)

(use-package my-functions
  :commands hit-servlet comment-or-uncomment-region-or-line wrap-args
            move-current-buffer insert-date insert-time unfill-paragraph
            add-auto-mode)

(use-package daily-log
  :commands daily-log-show-total-time
            daily-log-show-current-week-time
            daily-log-show-current-week-time-remaining)

(use-package my-keybindings)

(use-package config-windows
  :commands set-windows-env)

(use-package cygwin-mount
  :commands cygwin-mount-activate)

;; Make it easy to find the cursor after scrolling.
(use-package beacon
  :init (beacon-mode 1)
  :diminish beacon-mode)

(use-package notmuch
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
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "+deleted" "-unread") beg end)))

    (define-key notmuch-search-mode-map "r"
      (lambda (&optional beg end)
        "mark message as an archived receipt"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "+receipts" "-unread" "-inbox") beg end)))

    (define-key notmuch-search-mode-map "s"
      (lambda (&optional beg end)
        "mark message as spam"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "+spam" "-inbox" "-unread") beg end)))

    ;; Be evil-ish, because I want that.
    (define-key notmuch-show-mode-map "j" 'next-line)
    (define-key notmuch-show-mode-map "k" 'previous-line)
    (define-key notmuch-search-mode-map "j" 'next-line)
    (define-key notmuch-search-mode-map "k" 'previous-line)

    ;; Make it easier to open attachments in the corresponding tool.
    (define-key notmuch-show-mode-map (kbd "o")
      'notmuch-show-interactively-view-part)

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

    ;; Download an attachment locally.
    ;;
    ;; A rebinding of the standard 'w' keybinding, since I use 'w' for
    ;; something else."
    (define-key notmuch-show-mode-map "d" 'notmuch-show-save-attachments)

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
    (define-key notmuch-message-mode-map
                (kbd "<backtab>")
                '(lambda () (interactive) (notmuch-address-expand-name)))))

(use-package uniquify
             :init
             (setq
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":"
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode))

(use-package helm-projectile
  :commands helm-projectile-find-file)

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (helm-mode 1)

    ;; Yoinked from spacemacs:
    ;;
    ;; https://github.com/syl20bnr/spacemacs/blob/522366bbd179bc332a863efeb523daa09c603458/layers/+distribution/spacemacs-base/packages.el#L787-L795
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    (define-key helm-map (kbd "C-h") 'helm-next-source)
    (define-key helm-map (kbd "C-S-h") 'describe-key)
    (define-key helm-map (kbd "C-l") (kbd "RET"))
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key keymap (kbd "C-S-h") 'describe-key))

    (define-key helm-find-files-map (kbd "C-c d") 'insert-date)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)))

(use-package flycheck
  :diminish
  :defer t
  :config (progn
            (flycheck-define-checker proselint
              "A linter for prose."
              :command ("proselint" source)
              :error-patterns
              ((warning line-start (file-name) ":" line ":" column ": "
                        (id (one-or-more (not (any " "))))
                        (message) line-end))
              :modes (text-mode markdown-mode gfm-mode rst-mode))

            (add-to-list 'flycheck-checkers 'proselint)

            ;; Prefer locally-installed eslint, if available.
            ;;
            ;; Yanked from lunaryorn himself:
            ;;
            ;; http://emacs.stackexchange.com/a/21207/351
            (defun my/use-eslint-from-node-modules ()
              (let* ((root (locate-dominating-file
                            (or (buffer-file-name) default-directory)
                            "node_modules"))
                     (eslint (and root
                                  (expand-file-name "node_modules/eslint/bin/eslint.js"
                                                    root))))
                (when (and eslint (file-executable-p eslint))
                  (setq-local flycheck-javascript-eslint-executable eslint))))

            (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)))

(use-package ledger-mode
  :hook (ledger-mode . evil-ledger-mode))

;; Since emacs 24.4 made revert undoable, this option is perfectly safe and
;; pretty convenient.
(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode))

(use-package ne-yas-auto-insert
  :commands ne-yas-auto-insert-activate
            ne-yas-auto-insert-config)

(use-package autoinsert
  :init
  (progn
    (ne-yas-auto-insert-config)
    (ne-yas-auto-insert-activate)
    (auto-insert-mode)))

;; undo-tree improves on emacs' default infinite undo by teaching it to be
;; intelligent in how it handles branching, and offering a nice UI for
;; comparing things across branches.
;;
;; If it weren't for the bug that destroys undo history, it would be an
;; unmitigated spectacular win.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)

  ;; Store undo history in my autosaves directory (which is configured
  ;; elsewhere).
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,my-autosaves-dir)))

  ;; Evil has built-in logic to set up bindings in undo-tree mode.
  ;;
  ;; However, there doesn't seem to be a built-in way to say 'turn on
  ;; evil-local-mode whenever we run undo-tree-visualize'.
  ;;
  ;; Therefore, set that up here.
  (defadvice undo-tree-visualize (after evil-2 activate)
    (evil-local-mode)))

(use-package my-frame-setup
  :commands my-set-up-frame)

(use-package space-trail
  ;; TODO Remove commands once it's packaged and has autoloads.
  :commands space-trail-activate
  :init (space-trail-activate))

(use-package evil-exchange
  :commands evil-exchange-install)

(use-package evil-commentary
  :commands evil-commentary-mode)

(use-package ne-evil-textobjects
  :commands ne/install-textobjects)

;; Adds a text object to evil for selecting HTML attributes, bound by default
;; to 'x'.
;;
;; I do this often enough it seemed worth installing.
(use-package exato)

(use-package crontab-mode
  :mode "\\.cron\\(tab\\)?\\'"
  :config (add-hook 'crontab-mode-hook 'conf-mode-init))

(use-package evil
  :commands evil-local-mode
  :config
  (progn
    ;; I used to use evil-escape to let me use 'jk' to return to
    ;; evil-normal-state from evil-insert-state.
    ;;
    ;; I have abandoned that by making Control with no other keys send Escape
    ;; on my keyboards.
    ;;
    ;; This change was largely motivated by typing 'jk' way more often than I
    ;; meant to when working on a machine other than my own. By conforming to
    ;; standard Vim keybindings and just making it easier to trigger them, I
    ;; still have a comfortable personal environment but find it way easier to
    ;; work in foreign ones.
    ;;
    ;; ...at least that's the hope.

    ;; Use regular emacs keybindings for insert-mode (except for ESC-ESC-ESC,
    ;; because vim keybindings are still vim).
    (setq evil-insert-state-map (make-sparse-keymap))
    (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

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
    (ne/install-textobjects)

    ;; If a buffer is empty on evil-mode start, go directly to insert-mode,
    ;; because we'll almost certainly want to start typing.
    ;;
    ;; An empty buffer isn't the *only* case where this is the case, but it's a
    ;; starting point.
    (add-hook 'evil-local-mode-hook
              '(lambda ()
                 ;; HACK Magit buffers seem to start at size 0, but they
                 ;; populate very quickly, so waiting just a bit before
                 ;; checking whether we should be in insert-mode seems to work
                 ;; okay in practice. Doesn't work for *scratch*, though.
                 (run-at-time "0.1 sec"
                              nil
                              (lambda ()
                                (when (and evil-local-mode
                                           (= (buffer-size) 0)
                                           ;; HACK *scratch* buffer seems to
                                           ;; start out at 0 length, so I
                                           ;; explicitly ignore it.
                                           (not (string-equal (buffer-name)
                                                              "*scratch*")))
                                  (evil-insert-state))))))))

(use-package magit
  :defer t
  :hook (magit-mode . magit-svn-mode)
  :config (require 'evil-magit)
          (add-hook 'magit-status-mode-hook 'evil-local-mode)
          ;; (add-hook 'magit-mode-hook 'evil-normal-state)
          )

(use-package git-commit
  :init (global-git-commit-mode 1)
        (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package simple
  :diminish auto-fill-function)

(use-package aggressive-fill-paragraph
  :defer t
  :config (afp-advise-filled-functions))

;; Commented out because this is crashing my setup for some reason, and I'm not
;; currently using eclim.
;; (use-package eclimd
;;   :defer t
;;   :diminish
;;   :init
;;   (progn
;;     (autoload 'eclimd--running-p "eclimd" nil t)))

;; (use-package eclim
;;   :defer t
;;   :diminish)

(use-package sh-script
  :mode (("\\.envrc\\'" . sh-mode)
         ("\\.env" . sh-mode))
  :config (add-hook 'sh-mode-hook '(lambda ()
                                     (setq-local ne-yas-auto-insert-snippet-name
                                           "shell-script"))))

(use-package tern
  :diminish tern-mode
  :config (add-hook 'tern-mode-hook '(lambda ()
                                      (require 'tern-auto-complete)
                                      (tern-ac-setup)

                                      ;; Keybinding to force Tern's
                                      ;; autocompletion, for cases like
                                      ;; discussing data structures and APIs in
                                      ;; comments.
                                      (define-key
                                        tern-mode-keymap
                                        (kbd "C-<tab>")
                                        'tern-ac-complete)

                                      (setq tern-ac-sync t)
                                      (add-to-list 'ac-sources
                                                   'ac-source-tern-completion))))

(use-package tern-auto-complete
  :commands tern-ac-setup
  :config
  ;; Make tern's ac-source work with ac-trigger-key-command.
  ;;
  ;; Without this advice, completions are not fired when I press Tab with my
  ;; config.
  ;;
  ;; TODO Turn this into a PR to tern-auto-complete?
  ;;
  ;; It's almost an exact copy of the advice from there on auto-complete, and
  ;; should prevent other people who use an ac-trigger-key from having the same
  ;; issue.
  (defadvice ac-trigger-key-command (around add-tern-ac-candidates first activate)
    "Load tern-js candidates before ac-start."
    (if (and tern-ac-sync
             (memq major-mode tern-ac-js-major-modes)
             (not (or (ac-menu-live-p) (ac-inline-live-p)))
             ;; Extension - do not complete if there is no prefix, so TAB
             ;; fallback behavior can occur in that case.
             (ac-prefix-default))
        (tern-ac-complete-request
         '(lambda ()
            (auto-complete-1 :triggered 'trigger-key)))
      ad-do-it))
  ;; ac-source-yasnippet fails with the above advice if I don't do this.
  ;;
  ;; TODO Make this more robust. It should only add ac-trigger-key-command if
  ;; it isn't already in there.
  (if yas-expand-only-for-last-commands
      (add-to-list 'yas-expand-only-for-last-commands 'ac-trigger-key-command))
  )

(use-package json-mode
  ;; There's nothing that needs to be configured, but don't forget the
  ;; jq-interactively command. It can be very handy when trying to figure out
  ;; how to transform some arbitrary JSON with jq.
  :config (add-hook 'json-mode-hook 'js-mode-init))

(use-package jedi-force
  :commands jedi-force-set-up-hooks)

(use-package python-mode
  :init (jedi-force-set-up-hooks)
  :config
  (add-hook 'python-mode-hook '(lambda ()
                                 (my-prog-mode-init)

                                 (setq jedi:use-shortcuts t)
                                 (setq jedi:complete-on-dot t))))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

;; cc-mode defines several derived packages, so this setup will probably grow
;; to cover more than one mode.
(use-package cc-mode
  :defer t
  :config
  (progn
    ;; java-mode setup
    (add-hook 'java-mode-hook
              '(lambda ()
                 ;; If eclimd is running, use it.
                 (when (eclimd--running-p)
                   (eclim-mode)
                   (require 'ac-emacs-eclim-source)

                   ;; Set up eclim-mode-specific keybindings.
                   ;; TODO This jumps to def in a different window, and I don't
                   ;; see an equivalent command for returning that I can map to
                   ;; "M-,". Fix that if I start doing more Java stuff.
                   (define-key java-mode-map (kbd "M-.") 'eclim-java-find-declaration)
                   )))))

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            'my-prog-mode-init))

(use-package groovy-mode
  :defer t
  :mode "\\.groovy$"
  :mode "\\.gradle$"
  :config
  (add-hook 'groovy-mode-hook 'my-prog-mode-init))

(use-package nxml-mode
  :mode ("web.config$" . xml-mode)
  :defer t
  :config
  (progn
    (setq nxml-child-indent 4)
    (setq nxml-slash-auto-complete-flag t)
    (add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.blade.php\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'web-mode-init))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . scss-mode))
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              ;; Use SCSS-style comments, largely so that comment functions
              ;; don't go crazy the way they do in css-mode.
              (setq comment-start "//"
                    comment-end ""))))

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
            '(lambda ()
              (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init))))

(use-package slime
  :config
  (progn
    (defun mit-scheme-start-swank (file encoding)
     (format "%S\n\n" `(start-swank ,file)))

    (defun mit-scheme-find-buffer-package ()
      (save-excursion
        (let ((case-fold-search t))
          (goto-char (point-min))
          (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
               (match-string-no-properties 1)))))

    (defun mit-scheme-slime-mode-init ()
      (slime-mode t)
      (make-local-variable 'slime-find-buffer-package-function)
      (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

    (slime-setup)

    (if (not (memq 'mit-scheme slime-lisp-implementations))
        (setq slime-lisp-implementations
              (cons '(mit-scheme ("mit-scheme")
                                 :init mit-scheme-start-swank)
                    slime-lisp-implementations)))

    (setq slime-default-lisp 'mit-scheme)

    (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init)))

(use-package hideshow
  :defer t
  :config
  (progn
    (diminish 'hs-minor-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
      (progn
        ;; Set up all snippet dirs to lazy-load.
        (yas-reload-all)

        ;; Give keys_with_underscores priority over non-underscored-keys.
        ;; This lets things like require_once in php-mode not be overridden by
        ;; 'once' from cc-mode, php-mode's parent mode.
        (setq yas-key-syntaxes (list "w_" "w" "w_." "w_.()" "^ "))

        ;; GRIPE For reasons I don't understand, I need this invocation in
        ;; order to avoid a never-ending recursion of defining keybindings. I
        ;; think it's some interaction between yasnippet and auto-complete, but
        ;; I'm not really sure.
        (define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)
        (define-key yas-minor-mode-map (kbd "C-c y") yas-maybe-expand)
        (define-key yas-minor-mode-map [(tab)] nil)))

(use-package glasses
  :commands glasses-mode
  :diminish glasses-mode)

(use-package diff-mode
  :init (add-hook 'diff-mode-hook
                  '(lambda ()
                     ;; TODO Submit this as a patch to diff-mode.
                     ;;
                     ;; It already has these semantics, in that lines starting
                     ;; with a # do not cause the hunk headers to adjust, so
                     ;; that should probably be properly reflected.
                     (setq-local comment-start "#")
                     (text-mode-init)
                     ))
  :bind (:map diff-mode-map
              ("M-\d" . backward-kill-word)))

(use-package apache-mode
  :hook (apache-mode . conf-mode-init))

(use-package diffview
  :commands diffview-current)

;; eldoc-overlay mode is interesting, but has some quirks that make it kinda
;; painful.
;;
;; Hence, just commenting this out for now.
;;
;; (use-package eldoc
;;   :diminish
;;   :init (add-hook 'eldoc-mode-hook 'eldoc-overlay-mode))

(use-package flyspell
  :defer t
  :config
  (progn
    ;; The below configures flyspell/ispell to allow run-together words in
    ;; source code buffers, to deal with camelCaseNames sanely.
    ;;
    ;; Unfortunately, something about this setup causes Emacs to bog in some
    ;; cases and I have not yet figured out why. I've been able to make it
    ;; happen right after starting to edit a JS file and deleting a character
    ;; from a camelCasedIdentifier that becomes misspelled when I do that.
    ;;
    ;; Starting and stopping the mode a few times usually seems to make the
    ;; freeze stop. I don't know why.
    (require 'ne-spell-check)

    ;; Function to use popup.el menu for flyspell instead of the GUI menu.
    ;; From Emacswiki: http://www.emacswiki.org/emacs/FlySpell#toc11
    ;; It'd be nice to convert this to a package.
    (defun flyspell-emacs-popup-textual (event poss word)
      "A textual flyspell popup menu."
      (require 'popup)
      (let* ((corrects (if flyspell-sort-corrections
                           (sort (car (cdr (cdr poss))) 'string<)
                         (car (cdr (cdr poss)))))
             (cor-menu (if (consp corrects)
                           (mapcar (lambda (correct)
                                     (list correct correct))
                                   corrects)
                         '()))
             (affix (car (cdr (cdr (cdr poss)))))
             show-affix-info
             (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                         (list
                                          (list (concat "Save affix: " (car affix))
                                                'save)
                                          '("Accept (session)" session)
                                          '("Accept (buffer)" buffer))
                                       '(("Save word" save)
                                         ("Accept (session)" session)
                                         ("Accept (buffer)" buffer)))))
                           (if (consp cor-menu)
                               (append cor-menu (cons "" save))
                             save)))
             (menu (mapcar
                    (lambda (arg) (if (consp arg) (car arg) arg))
                    base-menu)))
        (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
    (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

(use-package atomic-chrome
  :demand
  :functions (atomic-chrome-start-server atomic-chrome-stop-server)
  :config (progn
          ;; Don't start Atomic Chrome if another Emacs instance is already
          ;; running it, and don't kill it if we didn't start the server.
          ;;
          ;; Since I do a fair bit of elisp hacking, I start a second Emacs
          ;; pretty often as part of my workflow, to verify that my init code
          ;; is doing what I expect, and vanilla Atomic Chrome causes startup
          ;; failures.
          ;;
          ;; TODO Get this fixed upstream. A real pidfile could be an option.

          (defvar ne/atomic-chrome-started-by-us nil
            "Whether this Emacs instance started an Atomic Chrome server.")

          (defun ne/atomic-chrome-mark-server-started ()
            (when (not (file-exists-p "~/.atomic-chrome-running"))
              (setq ne/atomic-chrome-started-by-us t)
              (write-region "" nil "~/.atomic-chrome-running")))

          (defun ne/atomic-chrome-mark-server-stopped ()
            (when ne/atomic-chrome-started-by-us
              (delete-file "~/.atomic-chrome-running")))

          (advice-add 'atomic-chrome-start-server :before
                      #'ne/atomic-chrome-mark-server-started)

          (advice-add 'atomic-chrome-stop-server :before
                      #'ne/atomic-chrome-mark-server-stopped)

          (add-hook 'kill-emacs-hook 'ne/atomic-chrome-mark-server-stopped)

          (when (not (file-exists-p "~/.atomic-chrome-running"))
                (atomic-chrome-start-server)))
  )

(use-package conf-mode
  ;; As a rule of thumb, if it's in dotfiles/src and it doesn't match a
  ;; more-specific regex, it should probably open in conf-mode.
  :mode "dotfiles/src/.+")

;; EmacsWiki-based packages that used to be on MELPA but are no more:
;;
;; https://github.com/melpa/melpa/pull/5008#issuecomment-360098939
(use-package ascii
  :commands ascii-on ascii-off ascii-display ascii-customize)

(use-package frame-cmds
  :commands maximize-frame-vertically)

;;; config-packages.el ends here
