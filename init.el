;; Nate Eagleson's .emacs file.

;; If, for some reason, you've found a copy of this and would like to speak to
;; the creator (for instance, to point out much more elegant and correct ways
;; of doing things), try nate@ishness.net.

;;;;;;;;;;;;;;;;;;;;;
;; Global Preferences
;;;;;;;;;;;;;;;;;;;;;

;; Everyone likes syntax coloration.
(global-font-lock-mode 1)

;; The next few chunks of code tell a long, sad story about my relationship
;; with ASCII character 9 (a.k.a the Tab character). Once, we were the closest
;; of friends, but one day, I decided I hated it... Thus, the current
;; incarnation of my .emacs file. The goal is to ensure that it never occurs in
;; any source file I edit.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; I do not use double-spaces after sentences. Neither should you.
(setq sentence-end-double-space nil)

;; I dislike using tabs for indentation, but adhering to project styles is
;; more important than my personal likes and dislikes. Therefore, here are my
;; default settings for tab-indented code.
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; GNU-style indentation on C-like languages is stupid. Linux style is much
;; more sane.
(setq c-default-style "linux"
      c-basic-offset 4)

;; I'd much rather have a sane way to goto-line than be able to easily change
;; my font settings...
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

;; I generally prefer to strip trailing whitespace on saves.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Include third-party libraries.
(add-to-list 'load-path "~/.emacs.d/libraries")
(progn (cd "~/.emacs.d/libraries")
       (normal-top-level-add-subdirs-to-load-path))

;; We like auto-compiling .el files to bytecode whenever they're loaded.
;; Note that byte-code-cache.el apparently will break on Windows, so I might
;; want to apply a patch to it, since I do sometimes get stuck on Windows.
;; See http://www.emacswiki.org/emacs/AutoRecompile#toc3
(require 'byte-code-cache)

;; Load the revbufs command.
(require 'revbufs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode setup and registration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include the modes directory.
(add-to-list 'load-path "~/.emacs.d/modes")

;; smart-dash-mode saves a lot of stupid SHIFT-ing in languages that favor
;; underscore as a word separator.
(require 'smart-dash)
;; Add PHP to smart-dash's list of modes that need C-style treatment of ->.
(setq smart-dash-c-modes (cons 'php-mode smart-dash-c-modes))

;; Trying this out to see how I like it.
(require 'autopair)
(autopair-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random functions worth having around.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode setup and registration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

