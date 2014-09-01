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

(use-package evil
  :defer t
  :config
  (progn
   ;; Use regular emacs keybindings for insert-mode.
   (setcdr evil-insert-state-map nil)

   ;; Use 'jk' to go from insert-state to normal-state. It's easier to type than
   ;; Escape.
   (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

   ;; Use ',' as my leader key.
   (global-evil-leader-mode)
   (evil-leader/set-leader ",")

   ;; Turn on surround everywhere.
   (global-evil-surround-mode)

   ;; Set up my leader shortcuts.
   ;;
   ;; I'm not really sure what I'll want here long-term. I'm starting with a few
   ;; of my regular shortcuts.
   (evil-leader/set-key
     "g" 'magit-status
     "b" 'my-helm-for-files
     "s" 'helm-swoop
     "r" 'er/expand-region
     "f" 'find-file
     "w" 'save-buffer
     "x" 'execute-extended-command)

   ;; Some modes it's better to start in insert-state for.
   (require 'cl)
   (loop for (mode . state) in '((git-commit-mode . insert))
         do (evil-set-initial-state mode state))

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

;;; config-packages.el ends here
