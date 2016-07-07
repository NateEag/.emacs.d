;;; init.el --- Nate Eagleson's Emacs config.

;;; Commentary:

;; I use Emacs as my primary text editor. This is my init file.
;;
;; See readme.rst for more info.

;;; Code:

;; Debug if there's an error during setup, since we'll have to fix it. We turn
;; this back off at the end of the file.
(setq debug-on-error t)

;; Set up my load path and a few other core things.
(load-file (concat user-emacs-directory "site-lisp/bootstrap.el"))

;; Set up package-specific autoloads and settings.
(load-file (make-emacs-dir-path "site-lisp/config-packages.el"))

;;; General preferences.

(setq custom-file (make-emacs-dir-path "custom.el"))
(load custom-file)

;; Let's try out the solarized theme. In theory, it should be easier on my
;; eyes...
(load-theme 'solarized-light t)

;; Most of the time, I want Unix-style line endings, and UTF-8 is generally a
;; good thing.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Work around visual-bell bug on OS X El Capitan:
;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
;; Remove this when Emacs 25.1 comes out.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; I don't like to type 'yes' when I could just type 'y'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable useful functions that are disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; If the matching line for a paren is offscreen, show the matching line in the
;; minibuffer.
(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (and
         (char-before (point))
         (char-equal (char-syntax (char-before (point))) ?\)))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

;; If a directory doesn't exist on the way to a new file, create it.
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; Autosave's defaults are not very nice. Here, we fix them.
;; Create autosave dir if it doesn't exist.
;; TODO Put autosaves outside my .emacs.d. I don't back .emacs.d up, since
;; I have it on GitHub, but I should really back up my backups...
(setq my-autosaves-dir (make-emacs-dir-path "autosaves/"))
(make-directory my-autosaves-dir t)
(setq
   ; Don't clobber symlinks.
   backup-by-copying t

   ; Don't break multiple hardlinks.
   backup-by-copying-when-linked t

   ; Don't litter the filesystem with backups *or* autosaves.
   backup-directory-alist
    `(("." . ,my-autosaves-dir))

   auto-save-file-name-transforms
   `((".*" ,(concat my-autosaves-dir "\\1") t))

   ;; Never auto-delete backups, so the backup-walker package is as useful as
   ;; possible.
   delete-old-versions -1

   ; use versioned backups
   version-control t)

;; Back up files even when using version control.
(setq vc-make-backup-files t)

;; Turn off default vc-mode, because I never use it.
(setq vc-handled-backends nil)

;; Tell magit to stop bothering me.
(setq magit-last-seen-setup-instructions "1.4.0")


;;; Save-related hooks.

(defun force-buffer-backup ()
  "Force a backup every time we save a file."

  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-buffer-backup)

(defun maybe-reset-major-mode ()
  "Reset the buffer's major-mode if a different mode seems like a better fit.

Mostly useful as a before-save-hook, to guess mode when saving a
new file for the first time."

  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))

(add-hook 'before-save-hook 'maybe-reset-major-mode)

;; If a file looks scripty and it isn't executable at save time, make it so.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; Save minibuffer data between sessions.
(setq savehist-file (make-emacs-dir-path "tmp/savehist"))
(savehist-mode t)

;; Set up exec-path to inherit values from the shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Load Windows-specific tweaks to environment, if we're running Windows.
(if (eq system-type 'windows-nt)
    (set-windows-env))

;; Tramp provides secure remote editing, via SFTP/SSH.
;; We don't load it by default, but we do config it. To load it, just do
;; (require 'tramp)
(setq tramp-default-method "scp")

;; Normally I want to load remote directory variable files. This can cause
;; efficiency problems, but I currently only use Emacs to edit remotely in a
;; dev environment. I may need to think this through more carefully...
;;
;; Enable directory local variables with remote files. This facilitates both
;; the (dir-locals-set-class-variables ...)(dir-locals-set-directory-class ...)
;; and the dir-locals.el approaches.
;; (defadvice hack-dir-local-variables (around my-remote-dir-local-variables)
;;   "Allow directory local variables with remote files, by temporarily redefining
;;      `file-remote-p' to return nil unconditionally."
;;   (cl-flet ((file-remote-p (&rest) nil))
;;     ad-do-it))
;; (ad-activate 'hack-dir-local-variables)

;; Make changing tabs re-guess the file's indentation style.
(defadvice untabify (after untabify-set-indent-tabs-mode
                           (start end  &optional _arg))
  "Set `indent-tabs-mode' after untabifying."
  (setq indent-tabs-mode nil))

(defadvice tabify (after tabify-set-indent-tabs-mode
                           (start end  &optional _arg))
  "Set `indent-tabs-mode' after tabifying."
  (setq indent-tabs-mode t))

(ad-activate 'untabify)
(ad-enable-advice 'untabify 'after 'untabify-set-indent-tabs-mode)

(ad-activate 'tabify)
(ad-enable-advice 'tabify 'after 'tabify-set-indent-tabs-mode)


;; TODO It'd be nice if this hook didn't run when I'm just opening a buffer to
;; compile the file, like when installing packages. Advising the appropriate
;; functions would probably do it, but I'm not sure which ones need the advice.
(defun my-prog-mode-init ()
  "General setup for programming modes."

  ;; "Emacs is a great OS, but a terrible text editor.
  ;; Fortunately, it's possible to write a great text editor for a great OS..."
  ;; -- some wag discussing evil-mode

  (evil-local-mode)

  (unicode-troll-stopper-mode)
  (diminish 'unicode-troll-stopper-mode)

  ;; Auto-fill comments, but not code.
  (comment-auto-fill)

  ;; Smartparens beat auto-pair after a long, hard struggle.
  (smartparens-mode t)
  (diminish 'smartparens-mode)

  ;; yasnippet is a powerful library for inserting snippets.
  (yas-minor-mode t)

  ;; Everyone loves autocompletion.
  (auto-complete-mode t)
  (setq ac-sources (list ac-source-yasnippet
                         ac-source-words-in-same-mode-buffers))
  (diminish 'auto-complete-mode)

  ;; Get clickable code-folding indicators in the fringe.
  ;; DEBUG This is *deathly* slow when you open files over about 7000 lines.
  ;; Not sure if it's so much linecount as just lots of folding points?
  ;; Regardless, opening something like web-mode.el effectively freezes emacs
  ;; for minutes. If I could optimize it or give it some kind of cutoff, that
  ;; might make it usable again.
  ;;
  ;; (hideshowvis-minor-mode t)

  ;; Pressing RET should do newline-then-indent, and continue any comment we
  ;; happen to be in.
  (local-set-key (kbd "RET") (key-binding (kbd "M-j")))

  ;; Flycheck does on-the-fly syntax checking, if the appropriate tools are
  ;; installed.
  (flycheck-mode t)

  ;; For programming, it's convenient if your editor guesses indentation styles
  ;; automatically.
  (guess-style-guess-all)

  ;; Make camelCaseNames more readable. See custom.el for settings.
  (glasses-mode)

  ;; Similarly, make camelCaseNames more navigable (for vanilla Emacs - looks
  ;; like I need evil-little-word to let evil behave this way).
  (subword-mode t)

  ;; It doesn't work everywhere, but which-function-mode is nice when it does.
  (which-function-mode t)

  ;; Everyone likes spell-checking.
  (flyspell-prog-mode)
  (diminish 'flyspell-mode)

  ;; Highlight TODO and friends as warnings. Yanked from this blog post:
  ;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|DEBUG\\|OPTIMIZE\\|HACK\\|REFACTOR\\)"
          1 font-lock-string-face t)))

  ;; Turn on smart-dash-mode if it's not a bad idea in our current mode.
  (if (not (member major-mode '(emacs-lisp-mode
                                lisp-mode
                                lisp-interaction-mode
                                css-mode
                                scss-mode)))
      (progn
        (smart-dash-mode)
        (diminish 'smart-dash-mode))))

(add-hook 'prog-mode-hook 'my-prog-mode-init)

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))

(eval-after-load 'subword
  '(diminish 'subword-mode))

;; Run smartparens customizations when it's started.
(eval-after-load 'smartparens
  '(smartparens-init))

;; Run auto-complete customizations when it's started.
(add-hook 'auto-complete-mode-hook 'auto-complete-init)

;; Run emmet-mode customizations when it's started.
(eval-after-load 'emmet-mode
  '(emmet-mode-init))


;; Major mode setup and registration.

;; SQL mode.
(defun load-sql-mode-accessories ()
  (interactive)

  (when (locate-library "sql-indent")
    (load-library "sql-indent")))
(add-hook 'sql-mode-hook 'load-sql-mode-accessories)
(add-auto-mode 'sql-mode "\\.sql$")

;; Text-editing modes of various stripes.
(defun text-mode-init ()
  "Configuration that is shared across my various text modes."

  (evil-local-mode)

  (turn-on-flyspell)
  (ac-ispell-setup)
  (ac-ispell-ac-setup)

  (auto-fill-mode t)
  (aggressive-fill-mode t)

  (smartparens-mode)

  ;; I occasionally want to use yasnippet in text mode.
  (yas-minor-mode)

  ;; Turn on flycheck-mode for prose-lint.
  (flycheck-mode t))

;; Everyone needs text-mode.
(add-hook 'text-mode-hook 'text-mode-init)

;; lilypond-mode - ripped from the lilypond repo.
(add-auto-mode 'LilyPond-mode "\\.ly$" "\\.ily$")
(add-hook 'LilyPond-mode-hook 'text-mode-init)
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; Commit message mode.
(defun git-commit-mode-hook ()
  "My settings for writing commit messages."
  (auto-fill-mode t)
  (setq fill-column 72)
  (turn-on-flyspell))
(add-hook 'git-commit-mode-hook 'git-commit-mode-hook)
(add-auto-mode 'git-commit-mode "COMMIT_EDITMSG$")

;; General config mode hook.
(defun conf-mode-init ()
  "My settings for editing config files."

  (my-prog-mode-init))
(add-hook 'conf-mode-hook 'conf-mode-init)

;; Unix config mode (mostly used for .git* files)
(defun conf-unix-mode-init ()
  "My settings for editing Unix config files."
  (auto-fill-mode))
(add-hook 'conf-unix-mode-hook 'conf-unix-mode-init)

;; Set up gitconfig mode.
(add-auto-mode 'gitconfig-mode ".gitconfig$")

;; reStructuredText mode.
(add-auto-mode 'rst-mode "\\.rst$" "\\.rest$")
(add-hook 'rst-mode-hook (lambda ()
                           (text-mode-init)
                           ;; rst uses these for link delimiters. In other
                           ;; modes I'm more prone to use them for tags.
                           (sp-pair "<" ">")))

;; Markdown mode.
(add-auto-mode 'markdown-mode "\\.md")
(add-hook 'markdown-mode-hook 'text-mode-init)

;; Include my PHP editing settings.
(add-auto-mode 'php-mode "\\.php$")
(add-hook 'php-mode-hook 'php-mode-init)

;; Tweak CSS mode a bit.
;; Note that for skewer-mode to be useful, you'll need to first call
;; the function (run-skewer). The following bookmarklet can then be used to
;; skewer-ify a page:
;; javascript:(function(){var d=document;var s=d.createElement('script');s.src='http://localhost:9000/skewer';d.body.appendChild(s);})()
(setq httpd-port 9000)
(add-hook 'css-mode-hook 'css-mode-init)

;; JavaScript Mode.
(add-hook 'js2-mode-hook 'js-mode-init)
(add-hook 'js2-mode-hook '(lambda ()
                            (setq mode-name "js2-mode")))
(add-auto-mode 'js2-mode "\\.js\\'")

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

;; js2-mode works poorly for me on .json files.
(add-hook 'js-mode-hook 'js-mode-init)
(add-hook 'js-mode-hook '(lambda () (setq mode-name "js-mode")))
(add-auto-mode 'js-mode
               "\\.json\\'"
               "\\.*jsbeautifyrc\\'"
               "\\.*jshintrc\\'"
               "\\.tern-project\\'")

;; If we're running in a window system, start an emacs server, so emacsclient
;; can connect to this instance.
(require 'server)
(when (and (display-graphic-p) (not (eq t (server-running-p))))
    (server-start))

;; Just for grins, see how long starting up took.
(add-hook 'after-init-hook
          (lambda ()
            (message (emacs-init-time))
            (when (display-graphic-p)
              (my-set-up-frame))))

;; Now that we're done with setup, stop debugging on error.
(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
