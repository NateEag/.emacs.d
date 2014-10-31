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

;; Set up my default font.
(my-set-default-font)

;; Let's try out the solarized theme. In theory, it should be easier on my
;; eyes...
(load-theme 'solarized-light t)

;; Most of the time, I want Unix-style line endings, and UTF-8 is generally a
;; good thing.
(setq-default buffer-file-coding-system 'utf-8-unix)

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

;; Autosave's defaults are not very nice. Here, we fix them.
;; Create autosave dir if it doesn't exist.
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


;;; Save-related hooks.

(defun force-buffer-backup ()
  "Force a backup every time we save a file."

  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-buffer-backup)

(defun maybe-reset-major-mode ()
  "Reset the buffer's major-mode if appropriate."
  (when (and
         ;; The buffer's visited file does not exist.
         (eq (file-exists-p (buffer-file-name)) nil)
         (eq major-mode 'fundamental-mode))
    (normal-mode)))
(add-hook 'before-save-hook 'maybe-reset-major-mode)

;; I generally prefer to strip trailing whitespace on saves, but not when I'm
;; editing diffs, where whitespace is crucial. If I ever start using another
;; file format where trailing whitespace matters, this might need upgrading,
;; but for now, this hack should do the job.
;; I originally tried to remove the delete-trailing-whitespace hook when
;; loading diff-mode, but for some reason, that only worked when I removed it
;; globally. I'm guessing before-save-hook is not a buffer-local variable.
(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if the current buffer is not a patch."
  (unless (string-match "\\.*.\\(patch\\|diff\\)$" (buffer-file-name))
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

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


;; Minor mode setup and registration.

;; Activate key-chord-mode so I can bind actions to character pairs.
;; Since key-chord-mode is not a true minor mode, there's no need to diminish
;; it.
(key-chord-mode t)

;; Activate undo-tree-mode globally and diminish it.
;;
;; It seems to me that undo-tree-mode and backup-walker might be good candidates
;; for merging somehow - they're like two sides of the same coin. I'll need a
;; lot more hands-on experience with both to have any idea how that would look
;; in practice.
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Some experiments *seem* to indicate that this shouldn't ever bite me. I'm
;; turning it on and hoping that's the case.
;; I've mainly turned it on globally because I can't figure out a better way to
;; diminish auto-revert-mode in git-managed buffers now that I have magit
;; installed.
(global-auto-revert-mode)
(diminish 'auto-revert-mode)

;; TODO It'd be nice if this hook didn't run when I'm just opening a buffer to
;; compile the file, like when installing packages. Advising the appropriate
;; functions would probably do it, but I'm not sure which ones need the advice.
(defun my-prog-mode-init ()
  "General setup for programming modes."

  ;; "Emacs is a great OS, but a terrible text editor.
  ;; Fortunately, it's possible to write a great text editor for a great OS..."
  ;; -- some wag discussing evil-mode

  (evil-local-mode)

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
  (diminish 'glasses-mode)

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

(eval-after-load 'simple
  '(diminish 'auto-fill-function))

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))

(eval-after-load 'subword
  '(diminish 'subword-mode))

;; Run yasnippet customizations when it's started.
(eval-after-load 'yasnippet
  '(yasnippet-init))

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

;; Shell script mode.
(add-auto-mode 'sh-mode "\\.bats$")

;; Text-editing modes of various stripes.
(defun text-mode-init ()
  "Configuration that is shared across my various text modes."

  (evil-local-mode)

  (ac-ispell-setup)
  (ac-ispell-ac-setup)
  (auto-fill-mode t)
  (smartparens-mode)

  ;; I occasionally want to use yasnippet in text mode.
  (yas-minor-mode))

;; Everyone needs text-mode.
(add-hook 'text-mode-hook 'text-mode-init)

;; lilypond-mode - ripped from the lilypond repo.
(add-auto-mode 'LilyPond-mode "\\.ly$" "\\.ily$")
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
(add-hook 'rst-mode-hook 'text-mode-init)

;; Markdown mode.
(add-auto-mode 'markdown-mode "\\.md")
(add-hook 'markdown-mode-hook 'text-mode-init)

;; Emacs Lisp mode.
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init)

;; Python mode.
(require 'jedi-force)
(add-auto-mode 'python-mode "\\.py$")
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(add-hook 'python-mode-hook 'python-mode-accessories-init)
(jedi-force-set-up-hooks)


;; DEBUG I cannot get this to autoload, and I don't know why.
(require 'python-mode-accessories-init)

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

(add-auto-mode 'scss-mode
               "\\.scss\\'")

;; Web mode.
;; For editing web templates of various stripes.
(add-auto-mode 'web-mode
               ".*\\.html\\'"
               ".*\\.html\\..*"
               ".*\\.twig\\..*"
               ".*\\.tmpl\\..*"
               "/\\(views\\|templates\\|include\\)/.*\\.php$"
               ".*.hbs\\'")
(add-hook 'web-mode-hook 'web-mode-init)

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

;; Groovy mode
(add-auto-mode 'groovy-mode
               "\\.groovy$"
               "\\.gradle")
(add-hook 'groovy-mode-hook 'my-prog-mode-init)

;; If we're running in a window system, start an emacs server, so emacsclient
;; can connect to this instance.
(require 'server)
(when (and (display-graphic-p) (not (eq t (server-running-p))))
    (server-start))

;;; My global keybindings, now that everything's loaded and defined.

;; Keybindings outside the "reserved for user" namespace (C-c <key>). These are
;; prone to stomp on keybindings from core or third-party packages (usually on
;; purpose).

;; I'd much rather have a sane way to goto-line than be able to easily change
;; my font settings.
(global-set-key "\M-g" 'goto-line)

;; Sometimes you want to toggle the current line's comment state.
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; I'd rather have regexes available when I search. Sorry, RMS.
;; Also, my Windows box at work has C-M-s bound OS-wide to pop a pointless
;; dialog box telling me something about HP.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Global keybindings inside the "reserved for user" namespace.

;; M-x means a lot of finger-scrunching.
(global-set-key [f8] 'execute-extended-command)

;; Reverting a buffer is much like refreshing.
(global-set-key [f5] '(lambda () (interactive) (revert-buffer t t)))

;; Switch buffers/find likely files via Helm.
(global-set-key (kbd "C-c b") 'my-helm-for-files)

;; Change flycheck's prefix-key to "C-c e". Code is taken from a docstring in
;; flycheck.
(eval-after-load 'flycheck
  '(progn
      (define-key flycheck-mode-map flycheck-keymap-prefix nil)
      (setq flycheck-keymap-prefix (kbd "C-c e"))
      (define-key flycheck-mode-map flycheck-keymap-prefix
        flycheck-command-map)))

;; Change names from snake_case to ALL_CAPS to StudlyCaps to camelCase.
;; TODO Make this just toggle between snake_case and camelCase. They're what I
;; usually use.
(global-set-key (kbd "C-c c") 'string-inflection-toggle)

;; g is for git, which is oh so much fun.
(global-set-key (kbd "C-c g") 'magit-status)

;; Dates and times are handy to be able to insert.
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c t") 'insert-time)

;; Search through buffers with helm-swoop.
(global-set-key (kbd "C-c s") 'helm-swoop)

;; Expand-region lets you select delimited regions quickly.
(global-set-key (kbd "C-c r") 'er/expand-region)

;; Try out multi-term as my terminal emulator.
(global-set-key (kbd "C-c m") 'multi-term-dedicated-toggle)
;; DEBUG These don't do what I'd like. They open new windows, and I'd like them
;; to just move to the next term-buffer in my selected window.
(global-set-key (kbd "s-[") 'multi-term-prev)
(global-set-key (kbd "s-]") 'multi-term-next)

;; toggle-quote lets you toggle a string between single- and double-quoted.
;; This will probably be deprecated in favor of evil-surround, once I'm more
;; fluent in evil-mode.
(global-set-key (kbd "C-c '") 'toggle-quotes)

;; Just for grins, see how long starting up took.
(add-hook 'after-init-hook
          (lambda ()
            (message (emacs-init-time))
            (when (display-graphic-p)
              (my-set-up-frame (selected-frame)))))

;; Now that we're done with setup, stop debugging on error.
(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
