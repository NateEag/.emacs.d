;; Nate Eagleson's .emacs file.

;; Just symlink ~/.emacs to ~/.emacs.d/main-emacs-file, and I should be good
;; to go.

;; This is the first import, so there's probably some dumb stuff in here. I've
;; never taken Emacs all that seriously up to this point - I've finally decided
;; to buckle down and start using it semi-seriously.

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

;; Include third-party libraries.
(add-to-list 'load-path "~/.emacs.d/libraries")
(progn (cd "~/.emacs.d/libraries")
       (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode setup and registration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include the modes directory.
(add-to-list 'load-path "~/.emacs.d/modes")

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
      (do-rope-initialize)))

(defun do-rope-initialize ()
  "Does the work of loading rope."
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  (setq rope-initialized 't))

(defvar auto-complete-python-initialized nil)
(defun initialize-auto-complete-python ()
  "Loads and configures auto-complete for Python hacking."
  (if (not auto-complete-python-initialized)
        (do-initialize-auto-complete-python)
      ;; If we've already initialized autocomplete, then we only need to set up
      ;; ropemode completions.
      ;; DEBUG I'm not exactly sure why I have to do this for every buffer.
      (setq ac-sources '(ac-source-ropemacs))))

(defun do-initialize-auto-complete-python ()
  "Sets up auto-completion for python buffers."
  (ac-personal-setup)
  (ac-ropemacs-init)
  (setq ac-sources '(ac-source-ropemacs))
  (setq auto-complete-python-initialized 't))

(defvar ac-initialized nil)
(defun ac-personal-setup ()
  "My basic configuration for autocomplete.el. Call this to activate it."
  (interactive)
  (if (not ac-initialized)
      (do-ac-personal-setup)))

(defun do-ac-personal-setup ()
  (require 'auto-complete)
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)
  (setq ac-trigger-key "TAB")
  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous)
  (auto-complete-mode t)
  (setq ac-initialized 't))

;; A hacked-up version of the rope support from auto-complete-config.el,
;; since it doesn't work any more.
;; DEBUG This works, but it's slow. I should really see what I can figure out
;; about speeding it up.
(defun ac-ropemacs-init ()
  (defvar ac-ropemacs-completions-cache nil)
  (defvar ac-source-ropemacs
    '((init
       . (fill-ac-ropemacs-completions-cache))
      (candidates . ac-ropemacs-completions-cache)
      ;; Setting prefix regex so that we autocomplete valid Python identifiers
      ;;  and immediately after .s.
      (prefix . "\\([_a-zA-Z0-9]+[.]?\\)"))))

(defun fill-ac-ropemacs-completions-cache ()
  (setq ac-ropemacs-completions-cache
      (mapcar
          (lambda (completion)
      	          (concat ac-prefix completion))
          	  (ignore-errors (rope-completions)))))


;; JavaScript Mode.

;; For Javascript, I currently use an old build of Karl Langstrom's
;; javascript.el.
(when (locate-library "javascript")
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode)))

(setq javascript-mode-hook 
      (function (lambda ()
                (setq indent-tabs-mode nil))))

;; Initializing yasnippet
; Note that we don't map tab to yasnippet, since I'm planning to hack up the
; ultimate tab-handling function... If I can snag it from someone else, that'd
; be plenty satisfactory.
(defun initialize-yasnippet ()
  "Loads and configures yasnippet the way I want it."
  (require 'yasnippet)
  (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets"))

;; Loading my python-mode accessories only when python-mode kicks in.
(defun load-python-mode-accessories ()
  "Loads all the libraries/tools I want to have when I'm in python-mode."
  (initialize-pymacs)
  (initialize-rope)
  (initialize-yasnippet)
  (initialize-auto-complete-python))
(add-hook 'python-mode-hook 'load-python-mode-accessories)
