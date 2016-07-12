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

(add-subdirs-to-front-of-load-path (make-emacs-dir-path "site-lisp"))


;; Set up the package library per my desires.
(require 'package)
(setq package-user-dir (make-emacs-dir-path "elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.milkbox.net/packages/"))
(package-initialize)


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

  ;; I use python-mode.el, with the TQS-coloration patch applied.
  ;; I should probably try installing the latest version and seeing how it
  ;; holds up.
  (autoload 'python-mode "python-mode" "Python editing mode." t)

  ;; tea-time's autoloads, despite being installed from MELPA, don't seem to
  ;; work. Therefore...
  (autoload 'tea-timer "tea-time.el")

  (autoload 'update-packages-update-installed-packages "update-packages" nil t))

(nateeag-autoloads-init)
