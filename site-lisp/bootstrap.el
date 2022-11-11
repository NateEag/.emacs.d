;; Handle a few things that I want to do almost all the time, so that I can
;; write scripts in batch mode correctly.

(defun make-emacs-dir-path (path)
  "Return path with `user-emacs-directory' prepended."
  (concat user-emacs-directory path))

;; Set up load-path.
(defun add-subdirs-to-front-of-load-path (path)
  "Add directories beneath path to the beginning of load-path."
  (let ((default-directory path))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
                (normal-top-level-add-subdirs-to-load-path))
                 load-path))
    (setq load-path (append (list path) load-path))))


;; Set up the package library per my desires.
(load-file (make-emacs-dir-path "site-lisp/ne-package-init.el"))

;; Add my local dirs to the load path.
(add-subdirs-to-front-of-load-path (make-emacs-dir-path "site-lisp"))

;; I'm starting to flirt with ledger for handling financial tracking. In
;; complete opposition to my usual approach to emacs, I'm going to let
;; installing the ledger package install the corresponding elisp, in hopes of
;; keeping the CLI and the Emacs mode in proper synchronization.
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list
 'load-path
 (expand-file-name "/usr/local/Cellar/ledger/3.1.1_3/share/emacs/site-lisp/ledger"))
(add-to-list 'auto-mode-alist '("\.ledger$" . ledger-mode))


;; Set up auto-compile, which should prevent me from ever again loading an
;; older byte-code file.
;;
;; ...at least, as long as I'm running 24.4 or newer...

(setq load-prefer-newer t)

(require 'auto-compile)

(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;; I use Flycheck, so I don't need to see the compilation errors buffer.
(setq auto-compile-display-buffer nil)

;;
(defvar my-autosaves-dir (make-emacs-dir-path "autosaves/")
  "Path to my autosaves directory.

It's defined in here because I need it to precede usage in config-packages.el.")


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
