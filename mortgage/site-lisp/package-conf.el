;;; package-conf.el --- My package configuration. -*- lexical-binding: t; -*-

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

;; Configuration of core Emacs behaviors, using the dummy emacs package.
;;
;; I should probably move my prefs.el settings in here.
(use-package emacs
  :ensure nil
  :bind (;; I'd much rather have a sane way to goto-line than to
	 ;; interactively change my font settings.
	 ("M-g" . goto-line-preview)

	 ;; Reverting a buffer is much like refreshing.
	 ([f5] . (lambda () (interactive) (revert-buffer t t)))

	 ;; I overrode this default keybinding over a decade ago, so
	 ;; it's deep in my subconscious.
	 ;;
	 ;; I'm not going to change it now.
	 ("M-;" . (lambda ()
		    (interactive)
		    (let (beg end)
		      (if (region-active-p)
			  (setq beg (region-beginning) end (region-end))
			(setq beg (line-beginning-position) end (line-end-position)))
		      (comment-or-uncomment-region beg end))))
	 )

  :init
  ;; I'd rather fit another few rows of buffer text than see menus and
  ;; buttons.
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ;; Ditto for another column.
  (scroll-bar-mode -1)

  :custom
  ;; Use tab key for completions.
  (tab-always-indent 'complete)

  ;; I've been using Emacs since 2003.
  ;;
  ;; I don't need to see the splash screen or other displays of basic emacs
  ;; trivia.
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)

  ;; I want vanilla emacs plus my explicitly-chosen extensions. That's it.
  (inhibit-default-init t)
  )

;; I'm not confident compile-angel is actually better than auto-compile.
;;
;; I'm giving it a try. We'll see how it feels.
(use-package compile-angel
  :ensure t
  :demand t
  :diminish compile-angel-on-load-mode compile-angel-on-save-local-mode
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

(use-package eldoc
  :init
  (setq eldoc-minor-mode-string nil)
  :diminish)

(use-package goto-line-preview
  :ensure t)

;; Emacs is a better operating system, hands down, and you can take it from me
;; when you pry it from my cold, dead hands.
;;
;; For me, vi really is a better editor, in that changing text is faster and
;; easier on my fragile wrists than it is with Emacs default keybindings or an
;; editor that requires using the mouse (i.e. anything other than the Ancient
;; Trio of ed, emacs, and vi).
;;
;; Fortunately, if you have a good operating system, eventually people port or
;; rewrite just about anything to it. :P
(use-package evil
  :ensure t
  :demand t
  :after hydra evil-surround
  :init
  ;; Ensure evil-collection initialization works.
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)

  :config
  ;; Evil everywhere, by default.
  ;;
  ;; I didn't do this in my previous config, but at this point it
  ;; seems simpler to opt out in the rare case where I don't want it.
  (evil-mode 1)

  (global-evil-surround-mode)
  (evil-commentary-mode)

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

;; Let's see if I can adapt to the standard evil community keybindings.
(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t)

(use-package evil-commentary
  :ensure t
  :diminish)

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

(use-package elisp-slime-nav
  :ensure t
  :diminish)

(use-package lisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("/Cask\\'" . emacs-lisp-mode))
  :after corfu cape
  ;; Wait to set up elisp customizations until initialization is done.
  ;;
  ;; Otherwise, the various elisp buffers like *scratch* that appear before my
  ;; config is finished loading throw errors for all the dependencies that
  ;; aren't loaded yet.
  ;;
  ;; This still results in *scratch* having the extras, though, since the hook
  ;; runs first time it's set. For a workaround, this is a pretty seamless one.
  :hook
  (after-init
   .
   (lambda ()
     (add-hook
      'emacs-lisp-mode-hook
      #'(lambda ()
	  (turn-on-elisp-slime-nav-mode))))))

(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode))

;; Select items more easily in functions using Emacs' built-in
;; completing-read, by filtering with space-separated fragments.
(use-package consult
  :ensure t)

;; Support filtering completions with regex fragments in no particular order.
;;
;; This makes it easier to narrow progressively, because you can add
;; specificity based on remaining candidate strings without having to
;; worry about which parts of those strings you're targeting.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

;; A more-readable completion UI than the default Emacs one.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; It can be nice to have a standard inline completion popup.
;;
;; Corfu's approach of sticking as close to built-in Emacs featues as
;; possible seems like a good idea.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Annotates completion candidates with metadata.
;;
;; I'm not sure I'll keep all of them - the file metadata feels like noise to me.
;;
;; Function summaries in execute-extended-command are gold, though. <3
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Offers a way to search for buffers _or_ files within a project.
(use-package consult-project-extra
  :ensure t)

;; Many minor modes don't need to be documented in the precious space of the
;; modeline.
;;
;; ...and diminish.el is a lovely reflection on the nature of those things we
;; take for granted and thus no longer notice, if you take the time to read the
;; source.
(use-package diminish
  :ensure t)

;; When buffers would otherwise have matching names, put their parent
;; directory after them, with a colon separating.
(use-package uniquify
  :init
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

(use-package smart-dash
  :ensure t
  :diminish)

;; I've been using yasnippet to generate (small amounts of)
;; boilerplate for several years now.
;;
;; In general I disapprove of boilerplate, and think that if it isn't
;; getting folded into abstractions, the project is not healthy, but
;; some languages do require a relatively large amount of ceremony.
(use-package yasnippet
  :ensure t
  :diminish
  :init
  (yas-minor-mode)
  :config

  ;; Set up all snippet dirs to lazy-load.
  (yas-reload-all)

  ;; Give keys_with_underscores priority over non-underscored-keys.
  ;; This lets things like require_once in php-mode not be overridden by
  ;; 'once' from cc-mode, php-mode's parent mode.
  (setq yas-key-syntaxes (list "w_" "w" "w_." "w_.()" "^ "))

  (defalias 'nateeag/yas-edit-snippet
    'yas-visit-snippet-file
    "I always forget this function's name.

With this alias I hope to not need to remember it."))

(use-package prog-mode
  :after smart-dash
  :bind (:map prog-mode-map
	      ("RET" . indent-new-comment-line))

  :hook
  (prog-mode
   .
   (lambda ()
     ;; Get a language server going.
     ;;
     ;; The docs say not to do this unless you're sure it will always
     ;; work, but it looks like all it's fussing about is that you'll
     ;; get a warning displayed when it can't start one.
     (eglot-ensure)

     ;; Turn on smart-dash-mode if it's not a bad idea in our current mode.
     ;;
     ;; TODO: Port my bash-related smart-dash hacks. Or maybe upstream them.
     (if (not (member major-mode '(emacs-lisp-mode
                                   lisp-mode
                                   lisp-interaction-mode
                                   css-mode
                                   scss-mode)))
         (smart-dash-mode))))
  )

;; Code formatters make life a lot simpler, both when writing code and during review.
;;
;; apheleia runs code formatters automatically, and leaves your cursor as close to its
;; original location as it can.
(use-package apheleia
  :ensure t
  :diminish
  :config
  (apheleia-global-mode))

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

(use-package git-link
  :ensure t)

;; It's nice to be able to see what parts of a file you've changed.
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode))

;; I just ran into the scdoc documentation format for the first time.
;;
;; I didn't see a mode in MELPA or GNU ELPA, but did find this one.
(use-package scdoc-mode
  :ensure (:host codeberg :repo "armaan/scdoc-mode"))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline line)
    (set-face-attribute 'mode-line nil :underline line)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :overline line)
    (set-face-attribute 'mode-line-inactive nil :underline line)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

;; Makes a sleek modeline I've been using for years.
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; TODO: Move to glsl-ts-mode
(use-package glsl-mode
  :ensure t
  :after eglot
  :config
  (add-to-list 'eglot-server-programs '((glsl-mode) "glsl_analyzer")))

;; If you haven't tried nix or NixOS, you're missing out.
(use-package nix-mode
  :ensure t
  :after eglot
  :config
  (add-to-list 'eglot-server-programs '((nix-mode) "nil")))

(use-package ast-grep
  :ensure t)

(provide 'package-conf)
;;; package-conf.el ends here
