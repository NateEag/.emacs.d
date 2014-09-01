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


;;; config-packages.el ends here
