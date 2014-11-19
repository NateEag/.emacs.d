;;; config-packages.el --- configure my Emacs packages.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just a big collection of use-package invocations.

;;; Code:

(require 'use-package)

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

    ;; DEBUG The first time this runs in a repo, I don't see any git-ls-files
    ;; results. I think it's probably just taking a while to run git-ls-files?
    (require 'helm-git-files)
    (defun my-helm-for-files ()
      "Try to make it painless to open files/swap buffers."
      (interactive)
      (helm :sources '(helm-source-buffers-list
                       helm-git-files:modified-source
                       helm-git-files:untracked-source
                       helm-git-files:all-source
                       helm-source-recentf)))))

(use-package my-frame-setup
  :commands my-set-up-frame)

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode)

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package eclimd
  :defer t
  :diminish
  :init
  (progn
    (autoload 'eclimd--running-p "eclimd" nil t)))

(use-package eclim
  :defer t
  :diminish)

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

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              ;; Use SCSS-style comments, largely so that comment functions
              ;; don't go crazy the way they do in css-mode.
              (setq comment-start "//"
                    comment-end ""))))

(use-package hideshow
  :defer t
  :config
  (progn
    (diminish 'hs-minor-mode)))

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

    ;; Turn on surround everywhere.
    (global-evil-surround-mode)

    ;; See my-keybindings.el for my actual leader keybindings.

    ;; Set up evil-nerd-commenter.
    ;;
    ;; This is a lame workaround. Requiring evil-nerd-commenter if evil-mode is
    ;; not active causes a crash. Thus, we require it whenever evil-mode is
    ;; activated, since it'll be a no-op after the first time.
    ;;
    ;; DEBUG Commented out for now, because I still haven't solved the weird
    ;; crashes it's causing me.
    ;;
    ;; (add-hook 'evil-local-mode-hook '(lambda ()
    ;;                                    (require 'evil-nerd-commenter)
    ;;                                    (evilnc-default-hotkeys)))

    ))

(use-package glasses
  :commands glasses-mode
  :diminish glasses-mode)

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
