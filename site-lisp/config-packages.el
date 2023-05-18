;; config-packages.el --- configure my Emacs packages.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just a big collection of use-package invocations.

;;; Code:

(require 'use-package)
(require 'auto-minor-mode)

(use-package envrc
  :diminish
  :init
  (envrc-global-mode))

(use-package activity-watch-mode
  :diminish
  :init (global-activity-watch-mode))

(use-package s
  :commands s-replace s-trim)

(use-package my-functions
  :commands
  hit-servlet comment-or-uncomment-region-or-line wrap-args
  move-current-buffer insert-date insert-date-iso-format insert-time unfill-paragraph
  add-auto-mode ne/set-theme-to-match-system-theme)

(use-package daily-log
  :commands
  daily-log-show-total-time
  daily-log-show-current-week-time
  daily-log-show-current-week-time-remaining)

(use-package tzc
  :init
  ;; TODO Factor this out to a once-at-start check that populates a var?
  (if (file-exists-p "/etc/NIXOS")
      (setq tzc-main-dir "/etc/zoneinfo/")))

(use-package my-keybindings)

(use-package config-windows
  :commands set-windows-env)

;; Helpful is a massive improvement over the baseline Emacs help functions
;; (which are themselves a massive improvement over what most programs offer).
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; I like having reference docs installed and browseable locally.
;;
;; Enter the dashman.
(use-package dash-docs
  :init (setq dash-docs-docsets-path (expand-file-name "~/Reference/.docsets"))
  :config (cl-map 'list #'dash-docs-ensure-docset-installed '("JavaScript"
                                                              "NodeJS"
                                                              "TypeScript"
                                                              "Bash"
                                                              "MongoDB"
                                                              "Go")))

(use-package cygwin-mount
  :commands cygwin-mount-activate)

;; Make it easy to find the cursor after scrolling.
(use-package beacon
  :init (beacon-mode 1)
  :diminish beacon-mode)

;; I really love Solarized. I generally use the dark variant, but I do
;; occasionally use the light version (usually when in bright lighting).
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline line)
    (set-face-attribute 'mode-line nil :underline line)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :overline line)
    (set-face-attribute 'mode-line-inactive nil :underline line)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; N.B.: I can get colors from Git by setting TERM=ansi after the session
;; starts. I suppose I should figure out the right solution to that problem
;; then turn it into code.

(use-package shell-pop
  ;; Make shell-pop's shell appear in the window it created for it. Workaround
  ;; from open GitHub issue:
  ;;
  ;; https://github.com/kyagi/shell-pop-el/issues/51#issuecomment-297470855
  :config
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  (setq shell-pop-full-span t
        shell-pop-universal-key "C-'"
        shell-pop-shell-type '("ansi-term" "*ansi-term*"
                               (lambda nil
                                 (ansi-term shell-pop-term-shell)))))

;; I don't know that I'll ever use this, but why not?
(use-package rfc-mode
  :commands rfc-mode-browse)

;; How can I trigger evil-local-mode in elfeed-search-mode? It doesn't seem to
;; have a hook...

(use-package elfeed
  :after (evil-collection)
  :hook ((elfeed-search-mode . evil-local-mode)
         (elfeed-show-mode . evil-local-mode))
  :config (evil-collection-elfeed-setup))

(use-package auth-source-pass
  :config (auth-source-pass-enable))

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
        (notmuch-search-tag (list "+deleted" "-unread" "-inbox") beg end)))

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

(use-package uniquify
  :init
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

;; Generally I prefer lazy-loading, but I think I need to do this so that the
;; helpful package will have counsel available any time I call it.
;;
;; TODO Figure out if there's a way to lazy-load optional packages.
(use-package counsel :demand t)

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode))

(use-package ivy
  :diminish (ivy-mode . "")
  :config (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
              ("<return>" . ivy-alt-done)
              ;; I'm pretty used to vim keybindings at this point.
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ;; Sometimes you want to use input text that happens to match an
              ;; existing candidate, but use it exactly as specified. This
              ;; keybinding allows that.
              ("C-<return>" . ivy-immediate-done))
  ;; Override the default binding, which kills buffers or text depending whether
  ;; cursor is at end-of-line.
  ;;
  ;; That is a clever idea, but I'd prefer my vim-ish keybinding hack.
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)))

(use-package flycheck
  :diminish
  :defer t
  :config (progn
            (flycheck-objc-clang-setup)

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
  :commands
  ne-yas-auto-insert-activate
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
  ;; elsewhere). That said, I have turned off undo-tree-auto-save-history to
  ;; try to work around the infamous bug of undo history lossage, so this
  ;; setting is largely irrelevant.
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
  :config (add-hook 'crontab-mode-hook '(lambda () (aggressive-fill-paragraph-mode nil)))
          (add-hook 'crontab-mode-hook 'conf-mode-init)
          )

(use-package evil-smartparens
  :commands evil-sp-smartparens-config)

(use-package evil
  :commands evil-local-mode
  :config
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

  ;; (setq evil-mode-line-format nil)

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

(use-package evil-collection
  :init (evil-collection-init))

(use-package evil-args
  :config (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
          (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package scratch-comment
  :bind (:map lisp-interaction-mode-map
              ("C-j" . scratch-comment-eval-sexp)))

(use-package svn-msg
  :mode ("svn-commit\(.[[:digit]]+\)*.tmp" . svn-msg-mode))

(use-package git-gutter
  :diminish
  :config
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (add-to-list 'git-gutter:update-hooks 'magit-post-refresh-hook))

;; Direnv lets you specify per-project shell environments. It's pretty magical.
;; Why don't we automatically apply direnv variables in buffers it applies to?
(use-package direnv
  :init
  ;; I don't need direnv to tell me what it's doing.
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package magit-delta
  :diminish
  :config
  (setq magit-delta-default-dark-theme "Solarized (dark)"))

(use-package magit-svn
  :commands magit-svn-mode)

(use-package magit
  :defer t
  :hook ((magit-mode . magit-svn-mode)
         (magit-status-mode . evil-local-mode)
         (magit-rebase-mode . evil-local-mode))
  :config
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

(use-package forge
  :after magit
  :hook ((forge-post-mode . (lambda ()
                                     (auto-fill-mode -1)
                                     (aggressive-fill-paragraph-mode -1)
                                     (visual-line-mode -1)))))

(use-package git-commit
  :init (global-git-commit-mode 1)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package simple
  :diminish auto-fill-function)

(use-package set-minor-mode-key
  :commands set-minor-mode-key)

(use-package aggressive-fill-paragraph
  :defer t
  :config (afp-advise-filled-functions))

(use-package string-edit
  :hook ((string-edit-mode . evil-local-mode)))

(use-package ne-smart-dash-hacks
  :commands ne-smart-dash-hacks-sh-mode-install)

(use-package smart-dash
  :hook ((smart-dash-mode . (lambda () (when (equal major-mode 'sh-mode)
                                         (ne-smart-dash-hacks-sh-mode-install))))))

(use-package sh-script
  :config (defun ne-sh-mode-maybe-insert-equals ()
            (interactive)
            (if (looking-back "^[[:space:]]*" nil)
                (progn (self-insert-command 1)
                       (backward-char))
              (self-insert-command 1)))

  :mode (("\\.envrc\\'" . sh-mode)
         ("\\.env" . sh-mode)
         ;; I sometimes write bashrc.local files that define a specific
         ;; interactive configuration for a given machine or project. If
         ;; there's a better name for these files I'd love to know.
         ("bashrc\\.local" . sh-mode)
         ;; Ditto with etc/bashrc (most common as /etc/bashrc)
         ("etc/bashrc" . sh-mode)
         ;; Bash completions are often named for the command that they modify
         ;; without any filename extension, sometimes with no hint that they're
         ;; a bash file other than living inside a completions/ directory.
         ;;
         ;; Hence this rule, so that when I edit such files they work
         ;; appropriately.
         ("/completions/" . sh-mode))
  :bind (:map sh-mode-map ("=" . 'ne-sh-mode-maybe-insert-equals))
  :hook ((sh-mode . (lambda () (setq-local ne-yas-auto-insert-snippet-name
                                           "shell-script")))
         (sh-mode . lsp)))

(use-package dtrt-indent
  :diminish)

;; I don't actually use unimpaired, but evil-collection apparently forces it to
;; load.
;;
;; TODO File a bug report that evil-collection not load unimpaired needlessly.
(use-package evil-collection-unimpaired
  :diminish)

(use-package gnuplot-mode
  :interpreter "gnuplot")

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

(use-package ac-ispell
  :commands ac-ispell-setup)

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
  :config (add-hook 'json-mode-hook 'js-mode-init)
  (add-hook 'json-mode-hook '(lambda ()
                               (setq-local js2-concat-multiline-strings
                                           nil))))

(use-package typescript
  ;; FIXME Get comment-auto-fill to Just Work in typescript-mode.
  ;;
  ;; The below binding gives me reasonable behavior in /* */ comment blocks but
  ;; doesn't continue //-style comments. I know I've had similar issues in
  ;; other modes, but I always forget the whys and wherefores.
  ;;
  ;; ...this really points back to working out the kinks in
  ;; aggressive-fill-mode and the bug(s?) in the underlying Emacs comment-fill
  ;; functions once and for all. :/
  :bind (:map typescript-mode-map
              ("RET" . default-indent-new-line))
  :hook ((typescript-mode . lsp)
         (typescript-mode . (lambda () (setq-local comment-style
                                                   'extra-line)))))

(use-package jedi-force
  :commands jedi-force-set-up-hooks)

(use-package python-mode
  :mode "Tiltfile\\'"
  :init (jedi-force-set-up-hooks)
  :config
  (add-hook 'python-mode-hook '(lambda ()
                                 (my-prog-mode-init)

                                 (setq jedi:use-shortcuts t)
                                 (setq jedi:complete-on-dot t))))

(use-package terraform-mode
  :hook ((terraform-mode . lsp)))

(use-package poly-argocd-template-mode
  :mode "/argo/.*\.tpl\\'")

(use-package hcl-mode
  :mode "\\.hcl.tpl")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :defer t
  :bind ("RET" . default-indent-new-line)
  :config
  (add-hook 'yaml-mode-hook
            'my-prog-mode-init)
  (add-hook 'yaml-mode-hook 'lsp)
  ;; yaml-mode constantly shifts indentation and breaks things with
  ;; aggressive-fill-paragraph-mode. Therefore, turning it off in there.
  (add-hook 'yaml-mode-hook '(lambda () (aggressive-fill-paragraph-mode -1)) 80))

(use-package groovy-mode
  :defer t
  :mode "\\.groovy\\'"
  :mode "\\.gradle\\'"
  :config
  (add-hook 'groovy-mode-hook 'my-prog-mode-init))

(use-package lua-mode
  :mode "\\.lua\\'"
  :config (setq lsp-clients-lua-language-server-bin
                "~/.nix-profile/bin/lua-language-server")
  :hook ((lua-mode . lsp)))

(use-package nxml-mode
  :mode ("web.config$" . xml-mode)
  :defer t
  :config
  (progn
    (setq nxml-child-indent 4)
    (setq nxml-slash-auto-complete-flag t)
    (add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))))

(use-package jinja2-mode
  :mode (("\\.j2\\'" . jinja2-mode)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.blade.php\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'web-mode-init))

(use-package web-mode-edit-element
  :diminish 'web-mode-edit-element-minor-mode)

(use-package auto-rename-tag
  :diminish)

;; TODO Extract html-scratchpad to a standalone package with autoloads. Then I
;; wouldn't have to declare the command.
(use-package html-scratchpad
  :commands html-scratchpad-open)

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

(use-package kbd-mode)

;; auto-compile dies on files that eventually lead it to compile compressed
;; elisp files. The way I caused this was a use-package invocation that touches
;; on files in core Emacs (specifically cc-mode, seen below), and therefore is
;; loaded from byte-compiled versions of files that are themselves compressed
;; and stored with the extension '.el.gz'.
;;
;; I tried adding a function to auto-compile-inhibit-compile-hook that would
;; abort on attempts to compile compressed files, but I was not able to
;; construct a function that did the job.
;;
;; I imagine it is unlikely I'll encounter this again outside the context of
;; the compressed files bundled with Emacs.app, where I know that the .elc
;; files are correct and the compressed sources have not changed. Thus, my
;; workaround is to just touch the .elc files inside the .app folder, thus
;; ensuring auto-compile mode does not try to compile them at load time.

(use-package cc-mode
  :defer t
  ;; FIXME This should just map cc-mode to lsp-cquery-enable. However, when I
  ;; do that lsp-cquery-enable runs in PHP files. This is arguably a bug in
  ;; php-mode, which I have filed an issue for:
  ;; https://github.com/ejmr/php-mode/issues/407
  :hook ((c-mode . (lambda ()
                     (when (equal major-mode 'c-mode)
                       (lsp-cquery-enable))))
         (java-mode . lsp)))

(use-package lsp-java
  :after lsp-mode
  :config (add-hook 'java-mode-hook 'lsp))

(use-package haskell-mode
  :after lsp-mode lsp-haskell
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp)))

(use-package go-mode
  :hook ((go-mode . lsp)
         (go-mode . (lambda () (aggressive-fill-paragraph-mode -1)))))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package apples-mode
  :mode "\\.scpt\\'"
  :hook (apples-mode . my-prog-mode-init)
  :interpreter "osascript")

;; For some reason cquery has caused my config to be broken after updating from
;; MELPA (on 2018-12-04). I've taken the expedient course of commenting it out
;; for now, since I have lots of time-bound work at the moment.
;;
;;(use-package cquery)

(use-package hideshow
  :defer t
  :config
  (progn
    (diminish 'hs-minor-mode)))

(use-package lsp-mode
  :defer t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode)

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

    (defalias 'ne/yas-edit-snippet
      'yas-visit-snippet-file
      "I always forget this function's name.

With this alias I hope to not need to remember it.")

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

;; I have a lot of old todo.txt files that do not follow the standard
;; format, but instead just start each line with "- ".
;;
;; I wish I had just followed the standard format, but since I didn't,
;; here is an easy way to just use text-mode for them.
(defun ne/reset-todo-file-type? ()
  "If current buffer uses my old ad-hoc todo.txt format, set it to text-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^- " 3 t)
        (progn (text-mode)
               (read-only-mode -1)))))

(use-package todotxt-mode
  :mode (("\\todo.txt\\'" . todotxt-mode))
  :hook ((todotxt-mode . (lambda ()
                           (aggressive-fill-paragraph-mode -1)
                           (auto-fill-mode -1)
                           (ne/reset-todo-file-type?)))))

(use-package apache-mode
  ;; A rule specific to a system I use at $DAYJOB.
  :mode "default-[[:digit:]]\\{8\\}\\'"
  :hook (apache-mode . conf-mode-init))

(use-package nginx-mode)

(use-package diffview
  :commands diffview-current)

(use-package csv-mode
  ;; One does not often want to automatically fill paragraphs in CSVs.
  :hook (csv-mode . (lambda () (aggressive-fill-paragraph-mode -1))))

;; eldoc-overlay mode is interesting, but has some quirks that make it kinda
;; painful.
;;
;; Hence, just commenting this out for now.
;;
;; (use-package eldoc
;;   :diminish
;;   :init (add-hook 'eldoc-mode-hook 'eldoc-overlay-mode))

;; Use camel-spell to spell-check camel-cased words, mostly to catch spelling
;; errors in source code.
;;
;; In principle I think the algorithm it applies should be supported at the
;; spellchecker level, because then you can apply the tool generally, not just
;; in Emacs - spell-check your code as part of your build process, for
;; instance.
;;
;; The algorithm can even be a bit smarter, which I intend to fix - with words
;; like SimpleHTTPServer, you can and should split between 'P' and 'S', as the
;; closing capital letter is part of the next word, by very strong convention.
;;
;; For now, though, this works, and is a big improvement on what I had before.
;;
;; TODO Get an open-source spellchecker to add support for camelCasing words.
(use-package camel-spell)

(use-package text-mode
  :mode "\\.txt.gpg")

(use-package rst-mode
  :mode "\\.rst")

(use-package sensitive-mode
  :minor ("\\.gpg$" . sensitive-mode))

(use-package flyspell
  :defer t
  :config
  (progn
    ;; Function to use popup.el menu for flyspell instead of the GUI menu. From
    ;; Emacswiki: http://www.emacswiki.org/emacs/FlySpell#toc11 It'd be nice to
    ;; convert this to a package.
    ;;
    ;; FIXME Get this working with camel-spell. For some reason it doesn't seem
    ;; to want to.
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

(use-package monkeytype
  :hook ((monkeytype-mode . (lambda ()
                              (centered-cursor-mode)
                              (evil-insert -1)))))

(use-package atomic-chrome
  :demand
  :functions (atomic-chrome-start-server atomic-chrome-stop-server)
  :hook ((atomic-chrome-edit-mode . (lambda
                                      ()
                                      (aggressive-fill-paragraph-mode -1)
                                      (auto-fill-mode -1))))
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
  :mode (("\\.service\\'" . conf-mode)
         ;; fail2ban suggests you put your own configuration into jail.local.
         ;; This is thus a useful rule.
         ("jail.local\\'" . conf-mode)
         ("CODEOWNERS\\'" . conf-mode))
  ;; As a rule of thumb, if it's in dotfiles/src and it doesn't match a
  ;; more-specific regex, it should probably open in conf-mode.
  ;;
  ;; Because we don't want this to override any other rules, we manually put it
  ;; on the end of the list.
  :config
  (add-to-list 'auto-mode-alist '("dotfiles/src/.+" . conf-mode) t))

;; EmacsWiki-based packages that used to be on MELPA but are no more:
;;
;; https://github.com/melpa/melpa/pull/5008#issuecomment-360098939
(use-package ascii
  :commands ascii-on ascii-off ascii-display ascii-customize)

;;; config-packages.el ends here
