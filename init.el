;; Nate Eagleson's Emacs config.


;; Global Preferences

;; Everyone likes syntax coloration.
(global-font-lock-mode 1)

;; I do not use double-spaces after sentences. Neither should you.
(setq sentence-end-double-space nil)

;; I dislike using tabs for indentation. A programmer should think about
;; his code's formatting, and different settings for tab-width will screw with
;; it. Spaces preserve formatting consistently across platforms. Thus, we
;; eliminate tabs with aggression here.
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

;; I'd much rather have a sane way to goto-line than be able to easily change
;; my font settings.
(global-set-key "\M-g" 'goto-line)

;; Everyone likes narrow-to-region. (Though I don't use it much these days.)
(put 'narrow-to-region 'disabled nil)

;; Default frames to a width of 80 chars.
(add-to-list 'default-frame-alist '(width . 80))

;; Default fill-column should be 79.
(setq-default fill-column 79)

;; I like to know what line/column I'm in, always.
(line-number-mode 1)
(column-number-mode 1)

;; Save setup.
;; Create autosave dir if it doesn't exist.
(make-directory "~/.emacs.d/autosaves/" t)
(setq
   backup-by-copying t      ; Don't clobber symlinks.
   backup-by-copying-when-linked t    ; Don't break multiple hardlinks.
   backup-directory-alist
    '(("." . "~/.emacs.d/autosaves"))    ; Don't litter the filesystem.
   delete-old-versions t    ; Delete old backups silently.
   kept-new-versions 2
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; I generally prefer to strip trailing whitespace on saves.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Include third-party libraries.
(add-to-list 'load-path "~/.emacs.d/libraries")
(progn (cd "~/.emacs.d/libraries")
       (normal-top-level-add-subdirs-to-load-path))

;; The uniquify package names buffers uniquely and readably.
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;; We like auto-compiling .el files to bytecode whenever they're loaded.
;; Note that byte-code-cache.el apparently will break on Windows, so I might
;; want to apply a patch to it, since I do sometimes get stuck on Windows.
;; See http://www.emacswiki.org/emacs/AutoRecompile#toc3
(require 'byte-code-cache)

;; Load the revbufs command.
(require 'revbufs)

;; DEBUG Consider moving these to their own file.
;; Insert the current date in ISO format.
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

;; Minor mode setup and registration.

;; Include the modes directory.
(add-to-list 'load-path "~/.emacs.d/modes")

;; smart-dash-mode saves a lot of stupid SHIFT-ing in languages that favor
;; underscore as a word separator.
(require 'smart-dash)
(setq smart-dash-c-modes (cons 'php-mode smart-dash-c-modes))

;; Automatic insertion/deletion of paired characters. Not flawless, but much
;; better than just typing.
(require 'autopair)
(autopair-global-mode t)


;; Random functions worth having around.

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
  (smart-dash-mode t))
(add-hook 'sql-mode-hook 'load-sql-mode-accessories)

;; Shell script mode.

(defun load-shell-mode-accessories ()
  (interactive)
  (smart-dash-mode t))
(add-hook 'sh-mode-hook 'load-shell-mode-accessories)

;; reStructuredText mode.
(require 'rst)
(setq auto-mode-alist '(("\\.rst$" . rst-mode)
                        ("\\.rest$" . rst-mode)))

;; Python mode.

;; I use python-mode.el, with the TQS-coloration patch applied.
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; The somewhat-convoluted setup for ropemacs/autocomplete.el integration
;; in python-mode follows.
(defvar pymacs-initialized nil)
(defun initialize-pymacs ()
  "Load pymacs if it is not already loaded."
  (interactive)
  (if (not pymacs-initialized)
      (do-initialize-pymacs)))

(defun do-initialize-pymacs ()
  "Does the work of loading pymacs."
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (setq pymacs-initialized 't))

(defvar rope-initialized nil)
(defun initialize-rope ()
  "Loads and configures rope as I like to use it."
  (if (not rope-initialized)
      ((lambda ()
        (pymacs-load "ropemacs" "rope-")
        (setq ropemacs-enable-autoimport t)
        (setq rope-initialized 't)))))

(defvar auto-complete-python-initialized nil)
(defvar ac-initialized nil)
(defun initialize-auto-complete-python ()
  "Loads and configures auto-complete for Python hacking."
  (if (not ac-initialized)
      (ac-personal-setup))

  (ac-ropemacs-init)
  (setq ac-sources (append '(ac-source-ropemacs
                             ac-source-ropemacs-dot) ac-sources)))

(defun ac-personal-setup ()
  "My basic configuration for autocomplete.el. Call this to activate it."
  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (setq ac-trigger-key "TAB")
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (auto-complete-mode t)
  (setq ac-initialized 't))

;; Mostly based on
;; https://github.com/gabrielelanaro/emacs-for-python/blob/master/epy-completion.el
;; with some shades of the old auto-complete-config.el still hanging on.
;; Hybridization brought on by vague feeling that it used to run faster than
;; Gabriel's code when I tried his (but I might well just be crazy, and I know
;; virtually nothing about elisp).
(defun ac-ropemacs-init ()
  (defvar ac-ropemacs-completions-cache nil)

  (ac-define-source ropemacs
    '((init
       . (fill-ac-ropemacs-completions-cache))
      (candidates . ac-ropemacs-completions-cache)
      (prefix . "[_a-zA-Z0-9]+")))

  (ac-define-source ropemacs-dot
    '((init
       . (fill-ac-ropemacs-completions-cache))
      (candidates . ac-ropemacs-completions-cache)
      (prefix . c-dot)
      (requires . 0))))

(defun fill-ac-ropemacs-completions-cache ()
  (setq ac-ropemacs-completions-cache
      (mapcar
          (lambda (completion)
      	          (concat ac-prefix completion))
          	  (ignore-errors (rope-completions)))))

;; Load my python-mode accessories only when python-mode kicks in.
(defun load-python-mode-accessories ()
  "Loads all the libraries/tools I want to have when I'm in python-mode."
  (initialize-pymacs)
  (initialize-rope)
  (initialize-auto-complete-python)
  (smart-dash-mode t)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action)))
(add-hook 'python-mode-hook 'load-python-mode-accessories)

;; PHP Mode.

;; I use php-mode, and I kinda hate it - but I have neither the skills nor the
;; time to write a better one, so I try to be grateful that I have anything
;; that handles PHP at all.
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "PHP editing mode." t)

(defun load-php-mode-accessories ()
  (smart-dash-mode t))
(add-hook 'php-mode-hook 'load-php-mode-accessories)

;; JavaScript Mode.

;; For Javascript, I currently use an old build of Karl Langstrom's
;; javascript.el.
(when (locate-library "javascript")
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode)))

(defadvice javascript-mode (after load-smart-dash-mode)
  "Load smart dash mode, since javascript-mode doesn't have a hook."
  (smart-dash-mode t))
(ad-activate 'javascript-mode)

;; Initializing yasnippet
(defun initialize-yasnippet ()
  "Loads and configures yasnippet."
  (require 'yasnippet)
  (setq yas/trigger-key (kbd "C-c y"))
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets"))
(initialize-yasnippet)

;; If we're running in a window system, start an emacs server, so emacsclient
;; can connect to this instance.
(if window-system
    (server-start))
