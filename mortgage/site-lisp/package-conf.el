;;; package-conf.el --- My package configuration.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:
;;;
;;; Declare the packages I use and how I like them configured.
;;;
;;; The heart of my Emacs mortgage setup.
;;;
;;; Assumes elpaca-init.el has been loaded.

;;; Code:

;; I'm not confident compile-angel is actually better than auto-compile.
;;
;; I'm giving it a try. We'll see how it feels.
(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-path-suffixes'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-path-suffixes)
  (push "/early-init.el" compile-angel-excluded-path-suffixes)
  ;; Files I load directly in my init.el via (require) seem to choke the startup
  ;; process.
  ;;
  ;; TODO: Figure out why and optimize this a bit.
  (push "/elpaca-init.el" compile-angel-excluded-path-suffixes)
  (push "/package-conf.el" compile-angel-excluded-path-suffixes)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files when they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

(use-package elec-pair
  :demand t
  :config
  (electric-pair-mode))

(use-package hydra
  :ensure t
  :demand t
  :diminish)

;; Emacs is a better operating system, hands down, and you can take it from me
;; when you pry it from my cold, dead hands.
;;
;; For me, vi really is a better editor, in that changing text is faster and
;; easier on my fragile wrists than it is with Emacs default keybindings or an
;; editor that requires using the mouse (i.e. anything other than the Ancient
;; Trio of ed, emacs, vi).
;;
;; Fortunately, if you have a good operating system, eventually people port or
;; rewrite just about anything to it. :P
(use-package evil
  :ensure t
  :demand t
  :after hydra evil-surround
  :config

  ;; Evil everywhere, by default.
  ;;
  ;; I didn't do this in my previous config, but at this point it
  ;; seems simpler to opt out in the rare case where I don't want it.
  (evil-mode 1)

  (global-evil-surround-mode)

  ;; TODO: Figure out how to load these via use-package.
  ;;
  ;; It would be cleaner, and I may eventually want to trigger these hydras
  ;; from keybindings other than my normal-mode leader key.
  (require 'nateeag-keybindings)

  ;; I used leader-key for ages, but my lesser-used bindings are easily-forgotten.
  ;;
  ;; So, I'm giving Hydra a try, which means I don't _need_ a leader key - just
  ;; to bind the top-level hydra to the appropriate evil keymap.
  ;;
  ;; FIXME: I _do_ need to figure out the correct way to load the hydra. That
  ;; means working out autoloads, I guess.
  (evil-define-key 'normal 'global (kbd "SPC") 'nateeag/keybindings/body))

(use-package evil-surround
  :ensure t)

;; Make emacs' built-in help functions more helpful.
;;
;; They're already way better than those of any other program I use to begin
;; with, but helpful does make them noticeably more useful.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; Many minor modes don't need to be documented in the precious space of the
;; modeline.
;;
;; ...and diminish.el is a lovely reflection on the nature of those things we
;; take for granted and thus no longer notice, if you take the time to read the
;; source.
(use-package diminish
  :ensure t)

;; I love project-specific shell environments.
;;
;; Direnv is the best tool I've encountered for them.
(use-package envrc
  :ensure t
  :diminish
  :init
  (envrc-global-mode))

;; Handy for quickly jumping to arbitrary parts of a file.
;;
;; Vimium's link following for text views, roughly.
(use-package avy
  :ensure t)

;; Magit remains the best version control interface I've used thus far.
;;
;; Maybe jj would unseat it if I gave it a fair trial, but I'm not sure I'll
;; ever get around to it, when I have my own very strong opinions about how
;; version control Ought To Work.
(use-package magit
  :ensure t)

(provide 'package-conf)
;;; package-conf.el ends here
