;; config-packages.el --- configure my Emacs packages.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just a big collection of use-package invocations.

;;; Code:

(require 'use-package)

(use-package beacon
  :init (beacon-mode 1))

(use-package notmuch
  :config
  (progn
    (define-key notmuch-search-mode-map "d"
      (lambda (&optional beg end)
        "mark message as deleted"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "+deleted") beg end)))
    (define-key notmuch-show-mode-map (kbd "o")
      'notmuch-show-interactively-view-part)))

(use-package uniquify
             :init
             (setq
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":"
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (helm-mode)

    (require 'helm-git-files)))

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

;; It seems to me that undo-tree-mode and backup-walker might be good candidates
;; for merging somehow - they're like two sides of the same coin. I'll need a
;; lot more hands-on experience with both to have any idea how that would look
;; in practice.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package my-frame-setup
  :commands my-set-up-frame)

(use-package space-trail
  ;; TODO Remove commands once it's packaged and has autoloads.
  :commands space-trail-activate
  :init (space-trail-activate))

(use-package s
  :commands s-replace s-trim)

(use-package magit
  :defer t)

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package simple
  :diminish auto-fill-function)

(use-package eclimd
  :defer t
  :diminish
  :init
  (progn
    (autoload 'eclimd--running-p "eclimd" nil t)))

(use-package eclim
  :defer t
  :diminish)

(use-package conf-mode
  :mode "/dotfiles/")

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
  :init
  (progn
    (setq nxml-child-indent 4)
    (setq nxml-slash-auto-complete-flag t)
    (add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'web-mode-init))

(use-package scss-mode
  :mode "\\.scss\\'"
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
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init))

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
        (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)
        (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
        (define-key yas-minor-mode-map [(tab)] nil)))

(use-package evil-exchange
  :commands evil-exchange-install)

(use-package evil
  :commands evil-local-mode
  :config
  (progn

    ;; Activate key-chord-mode so I can bind actions to character pairs.
    ;; Since key-chord-mode is not a true minor mode, there's no need to diminish
    ;; it.
    (key-chord-mode t)

    ;; Use regular emacs keybindings for insert-mode.
    (setcdr evil-insert-state-map nil)

    ;; Use 'jk' to go from insert-state to normal-state. It's easier to type than
    ;; Escape.
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)

    ;; Use ',' as my leader key.
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")

    ;; See my-keybindings.el for my actual leader keybindings.

    ;; Turn on surround everywhere.
    (global-evil-surround-mode)

    ;; Some modes it's better to start in insert-state for.
    (require 'cl)
    (loop for (mode . state) in '((git-commit-mode . insert))
          do (evil-set-initial-state mode state))

    (evil-exchange-install)

    (evil-commentary-default-setup)

    (diminish 'evil-commentary-mode)))

(use-package glasses
  :commands glasses-mode
  :diminish glasses-mode)

(use-package diffview
  :commands diffview-current)

(use-package flyspell
  :defer t
  :config
  (progn
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

(use-package my-functions
  :commands hit-servlet comment-or-uncomment-region-or-line wrap-args
            move-current-buffer insert-date insert-time unfill-paragraph
            add-auto-mode)

(use-package my-keybindings)

(use-package config-windows
  :commands set-windows-env)

(use-package cygwin-mount
  :commands cygwin-mount-activate)

;;; config-packages.el ends here
