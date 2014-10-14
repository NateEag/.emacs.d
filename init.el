;;; init.el --- Nate Eagleson's Emacs config.

;;; Commentary:

;; I use Emacs as my primary text editor. This is my init file.
;;
;; See readme.rst for more info.

;;; Code:

;; Sometimes you want to debug when there are errors, but not nearly as often
;; as I believed at the first.
;(setq debug-on-error t)

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
(load-theme 'solarized-dark t)

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

;; I generally prefer to strip trailing whitespace on saves, but not when I'm
;; editing diffs, where whitespace is crucial. If I ever start using another
;; file format where trailing whitespace matters, this might need upgrading,
;; but for now, this hack should do the job.
;; I originally tried to remove the delete-trailing-whitespace hook when
;; loading diff-mode, but for some reason, that only worked when I removed it
;; globally. I'm guessing before-save-hook is not a buffer-local variable.
(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if the current buffer's filename allows it."
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
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink"))
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

;; Random functions worth having around.

;; Function to refresh a file in my jstoolkit sandbox.
;;
;; Used as a file-local after-save-hook so I don't have to recompile the
;; toolkit every time I update a file.
;;
;; DEBUG I'm not really sure where this should live. It doesn't belong in
;; .dir-locals.el, since it's a not-entirely-trivial function, it doesn't
;; belong in jstoolkit since it's my own tooling, and it doesn't belong here
;; because it's project-specific.
(defun hit-servlet ()
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (start-process "refresh-js"
                   nil
                   "curl"
                   (concat "http://localhost:3003/controller/pp?file="
                           filename))
    (message (concat "Processing " filename))))

;; From http://stackoverflow.com/a/9697222
(defun comment-or-uncomment-region-or-line ()
    "Comment/uncomment region, or current line if there is no region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; From http://stackoverflow.com/a/17859480/1128957
(defun add-auto-mode (mode &rest patterns)
  "Add filename `patterns' to `auto-mode-alist' for MODE."
  (mapc (lambda (pattern)
          (add-to-list 'auto-mode-alist (cons pattern mode)))
        patterns))

;; GRIPE If I just generalized a tiny bit, I could probably make this work in a
;; lot more languages than just PHP, since comma for separator is a really
;; common idiom, and it'd be easy to accept different delimiters.
(defun wrap-args ()
  "Split function arg/array contents to multiple lines in PHP code."
  (interactive)
  (let ((close-paren-pos (search-forward ")" nil 't))
        (open-paren-pos (search-backward "(" nil 't)))

    (goto-char (- close-paren-pos 1))
    (insert "\n")
    (setq close-paren-pos (+ close-paren-pos 1))

    (goto-char (+ open-paren-pos 1))
    (insert "\n")

    (while (search-forward "," close-paren-pos 't)
      (insert "\n")
      (setq close-paren-pos (+ close-paren-pos 1)))
    (indent-region open-paren-pos (+ close-paren-pos 1))))

;; Move the current buffer to a new location on disk, then rename the buffer.
;; From http://www.stringify.com/2006/apr/24/rename/
(defun move-current-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

;; GRIPE Consider moving these to their own file.
;; Insert the current date.
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use dd-mm-YYYY format. With
     two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%B %d, %Y"))))
      (insert (format-time-string format))))

;; Insert the current time.
(defun insert-time (prefix)
    "Insert current time. With prefix-argument, use a full timestamp in 24-hour
     format."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%l:%M %p")
                   ((equal prefix '(4)) "%Y-%m-%d %H:%m:%s"))))
      (insert (concat " " (s-trim (format-time-string format))))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

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

;; After a few months, I can definitively say that Helm beats pressing Tab.
(helm-mode)
(diminish 'helm-mode)

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
                   helm-source-recentf)))

(defun my-prog-mode-init ()
  "General setup for programming modes."

  ;; "Emacs is a great OS, but a terrible text editor.
  ;; Fortunately, it's possible to write a great text editor for a great OS..."
  ;; -- some wag discussing evil-mode

  (require 'evil)
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
  (require 'glasses)
  (glasses-mode)
  (diminish 'glasses-mode)

  ;; Similarly, make camelCaseNames more navigable (for vanilla Emacs - looks
  ;; like I need evil-little-word to let evil behave this way).
  (subword-mode t)

  ;; Everyone likes spell-checking.
  (flyspell-prog-mode)
  (diminish 'flyspell-mode)

  ;; Turn on smart-dash-mode if it's not a bad idea in our current mode.
  (if (not (member major-mode '(emacs-lisp-mode
                                lisp-mode
                                lisp-interaction-mode
                                css-mode
                                scss-mode)))
      (progn
        (smart-dash-mode)
        (diminish 'smart-dash-mode))))

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

  (my-prog-mode-init)

  (when (locate-library "sql-indent")
    (load-library "sql-indent")))
(add-hook 'sql-mode-hook 'load-sql-mode-accessories)
(add-auto-mode 'sql-mode "\\.sql$")

;; Shell script mode.
(add-hook 'sh-mode-hook 'my-prog-mode-init)
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

;; At least get basic amenities in Java. I should probably get eclim up and
;; running someday, since $DAYJOB uses a lot of it, even if I am frontend.
(add-hook 'java-mode-hook 'my-prog-mode-init)

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
               ".*\\.html\\..*" ".*\\.twig\\..*" ".*\\.tmpl\\..*"
               "/\\(views\\|templates\\|include\\)/.*\\.php$"
               ".*.hbs\\'")
(add-hook 'web-mode-hook 'web-mode-init)

;; JavaScript Mode.
(add-hook 'js2-mode-hook 'js-mode-init)
(add-auto-mode 'js2-mode "\\.js\\'")

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

;; js2-mode works poorly for me on .json files.
(add-hook 'js-mode-hook 'js-mode-init)
(add-auto-mode 'js-mode
               "\\.json\\'"
               "\\.*jsbeautifyrc\\'"
               "\\.*jshintrc\\'"
               "\\.tern-project\\'")

;; xml-mode
;; IIS's 'config' files are actually XML.
(add-auto-mode 'xml-mode "web.config$")
(setq nxml-child-indent 4)
(setq nxml-slash-auto-complete-flag t)
(add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))

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

(provide 'init)
;;; init.el ends here
