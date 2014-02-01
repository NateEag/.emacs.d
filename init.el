;; Nate Eagleson's Emacs config.

;; Set up the package library per my desires.
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" .
                                 "http://melpa.milkbox.net/packages/"))

;; Explicitly call package-initialize so that modes which trigger on start
;; (like emacs-lisp-mode) don't cause horkage when running in batch mode.
(package-initialize)

;;; General preferences.

;; Set up my default font. Work in progress.
(defun my-get-default-font-size ()
  "Return a good(?) size for my default font based on monitor resolution."

  (if (> (display-pixel-width) 1280)
      14
    12))

(defun my-get-default-font-name ()
  "Return the name of my preferred font."
  "Anonymous Pro")

(defun my-get-default-font ()
  "Return a string specifying my default font."

  (setq my-font-size (my-get-default-font-size))
  (setq my-default-font (concat (my-get-default-font-name) "-"
                                (number-to-string my-font-size))))

;; Do not set a font if it is not available - keeps us from crashing in a
;; font-free setting.
(if (member (my-get-default-font-name) (font-family-list))
    (set-face-attribute 'default nil :font (my-get-default-font)))

;; Sometimes you want to debug when there are errors, but not nearly as often
;; as I believed at the first.
;(setq debug-on-error t)

;; I'd rather have regexes available when I search. Sorry, RMS.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Add appropriate directories to load-path.
(defun add-subdirs-to-front-of-load-path (path)
  "Add directories beneath path to the beginning of load-path."
  (let ((default-directory path))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
                (normal-top-level-add-subdirs-to-load-path))
                 load-path))))

(add-subdirs-to-front-of-load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d")

;; GRIPE These two dirs should be merged into site-lisp.
(add-to-list 'load-path "~/.emacs.d/libraries")
(progn (cd "~/.emacs.d/libraries")
       (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/modes")

(add-to-list 'load-path "~/.emacs.d/site-lisp/tern-mode/emacs")

;; Everyone likes syntax coloration.
(global-font-lock-mode 1)

;; I like seeing my selections.
(setq transient-mark-mode 1)

;; STOP THE RINGING
(setq visible-bell 1)

;; The emacs startup message is a needless annoyance.
(setq inhibit-startup-message t)

;; The toolbar is an even more needless annoyance.
(tool-bar-mode -1)

;; I do not use double-spaces after sentences. Neither should you.
(setq sentence-end-double-space nil)

;; I dislike using tabs for indentation. Spaces are a simpler way to indent.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; That said, adhering to project styles is more important than my personal
;; preferences. Therefore, here are some tab-related settings, for when I'm
;; editing code that uses Tab for indentation.
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; GNU-style indentation on C-like languages is stupid. Linux style is much
;; more sane.
(setq c-default-style "linux"
      c-basic-offset 4)

;; Most of the time, I want Unix-style line endings, and UTF-8 is generally a
;; good thing.
(setq-default buffer-file-coding-system 'utf-8-unix)

;; I don't like to type 'yes' when I could just type 'y'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; I'd much rather have a sane way to goto-line than be able to easily change
;; my font settings.
(global-set-key "\M-g" 'goto-line)

;; Enable useful functions that are disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Default frames to a width of 80 chars.
(add-to-list 'default-frame-alist '(width . 80))

;; Default fill-column should be 79.
(setq-default fill-column 79)

;; I like to know what line/column I'm in, always.
(line-number-mode 1)
(column-number-mode 1)

;; Yay for highlighting parentheses!
(show-paren-mode 1)

;; Autosave's defaults are not very nice. Here, we fix them.
;; Create autosave dir if it doesn't exist.
(setq my-autosaves-dir (expand-file-name "~/.emacs.d/autosaves/"))
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

;; Force a backup every time we save a file.
;; Does it get this every time I save now?
(defun force-buffer-backup ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-buffer-backup)

;; Activate undo-tree-mode globally and diminish it.
;;
;; It seems to me that undo-tree-mode and backup-walker might be good candidates
;; for merging somehow - they're like two sides of the same coin. I'll need a
;; lot more hands-on experience with both to have any idea how that would look
;; in practice.
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Save minibuffer data between sessions.
(setq savehist-file "~/.emacs.d/tmp/savehist")
(savehist-mode t)

;; Set up manually-maintained autoloads. Mostly defines mode hooks.
(require 'nateeag-autoloads-init)
(nateeag-autoloads-init)

;; Set up exec-path to inherit values from the shell.
(when (memq window-system '(mac ns))
  ;; DEBUG This should Just Work with autoloading, installed from MELPA as it
  ;; is, but it doesn't actually seem to. Why not?
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Tweak exec-path to include binaries in this repo.
(setq exec-path
      (append
       exec-path
       (list "~/.emacs.d/php-cs-1.4.4/scripts")))

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
;; dev environment. I may need to think this through more carefully
;; Enable directory local variables with remote files. This facilitates both
;; the (dir-locals-set-class-variables ...)(dir-locals-set-directory-class ...)
;; and the dir-locals.el approaches.
(defadvice hack-dir-local-variables (around my-remote-dir-local-variables)
  "Allow directory local variables with remote files, by temporarily redefining
     `file-remote-p' to return nil unconditionally."
  (cl-flet ((file-remote-p (&rest) nil))
    ad-do-it))
(ad-activate 'hack-dir-local-variables)

;; I generally prefer to strip trailing whitespace on saves, but not when I'm
;; editing diffs, where whitespace is crucial. If I ever start using another
;; file format where trailing whitespace matters, this might need upgrading,
;; but for now, this hack should do the job.
;; I originally tried to remove the delete-trailing-whitespace hook when
;; loading diff-mode, but for some reason, that only worked when I removed it
;; globally. I'm guessing before-save-hook is not a buffer-local variable.
(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if the current buffer's filename allows it."
  (unless (string-match "\\.*.\\(patch\\|diff\\)" (buffer-file-name))
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)


;; The uniquify package names buffers uniquely and readably.
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;; Load the revbufs command.
(require 'revbufs)

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
(global-set-key (kbd "C-c d") 'insert-date)

;; Insert the current time.
(defun insert-time (prefix)
    "Insert current time. With prefix-argument, use a full timestamp in 24-hour
     format."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%l:%M %p")
                   ((equal prefix '(4)) "%Y-%m-%d %H:%m:%s"))))
      (insert (format-time-string format))))
(global-set-key (kbd "C-c t") 'insert-time)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Minor mode setup and registration.

;; smart-dash-mode saves a lot of stupid SHIFT-ing in languages that favor
;; underscore as a word separator.
(require 'smart-dash)


;; Set up diff-mode.
(defun update-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-added nil
                      :foreground "darkgreen" :background "grey80")
  (set-face-attribute 'diff-removed nil
                      :foreground "darkred" :background "grey80")
  (set-face-attribute 'diff-changed nil
                      :foreground "darkgreen" :background "grey80"))

(eval-after-load "diff-mode"
  '(update-diff-colors))


;; Random functions worth having around.

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

;; Major mode setup and registration.

;; SQL mode.
(defun load-sql-mode-accessories ()
  (interactive)
  (smart-dash-mode t)
  (comment-auto-fill)
  (auto-complete-init)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers))
  (when (locate-library "sql-indent")
    (load-library "sql-indent")))
(add-hook 'sql-mode-hook 'load-sql-mode-accessories)
(setq auto-mode-alist (cons '("\\.sql$" . sql-mode) auto-mode-alist))

;; Shell script mode.

(defun load-shell-mode-accessories ()
  (interactive)
  (comment-auto-fill)
  (smart-dash-mode t)
  (autopair-init))
(add-hook 'sh-mode-hook 'load-shell-mode-accessories)

;; Text-editing modes of various stripes.
(defun text-mode-init ()
  "Configuration that is shared across my various text modes."
  (auto-fill-mode t)
  (autopair-init)

  ;; I occasionally want to use yasnippet in text mode.
  (yasnippet-init))

;; Everyone needs text-mode.
(add-hook 'text-mode-hook 'text-mode-init)

;; lilypond-mode - ripped from
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; Turn off default vc-mode, because I never use it.
(setq vc-handled-backends)

;; Commit message mode.
(defun git-commit-mode-hook ()
  "My settings for writing commit messages."
  (auto-fill-mode t)
  (setq fill-column 72)
  (turn-on-flyspell))
(add-hook 'git-commit-mode-hook 'git-commit-mode-hook)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . git-commit-mode))

;; Set up gitconfig mode.
(add-to-list 'auto-mode-alist '(".gitconfig$" . gitconfig-mode))

;; reStructuredText mode.
(setq auto-mode-alist (cons '("\\.rst$" . rst-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rest$" . rst-mode) auto-mode-alist))
(add-hook 'rst-mode-hook 'text-mode-init)

;; Markdown mode.
(setq auto-mode-alist
  (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'text-mode-init)

;; Emacs Lisp mode.
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-init)

;; Python mode.
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))
(add-hook 'python-mode-hook 'load-python-mode-accessories)

(require 'python-mode-init)

;; Include my PHP editing settings.
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(add-hook 'php-mode-hook 'php-mode-init)

;; Tweak CSS mode a bit.
;; Note that for skewer-mode to be useful, you'll need to first call
;; the function (run-skewer). The following bookmarklet can then be used to
;; skewer-ify a page:
;; javascript:(function(){var d=document;var s=d.createElement('script');s.src='http://localhost:8081/skewer';d.body.appendChild(s);})()ema
(setq httpd-port 8081)
(add-hook 'css-mode-hook 'skewer-reload-stylesheets-mode)

;; Web mode.
;; For editing web templates of various stripes.
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-hook 'web-mode-hook 'web-mode-init)

;; JavaScript Mode.
(add-hook 'js-mode-hook 'js-mode-init)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

;; xml-mode
;; IIS's 'config' files are actually XML.
(add-to-list 'auto-mode-alist '("web.config$" . xml-mode))
(setq nxml-child-indent 4)

;; If we're running in a window system, start an emacs server, so emacsclient
;; can connect to this instance.
(require 'server)
(when (and (not (server-running-p)) (display-graphic-p))
    (server-start))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
