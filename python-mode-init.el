;; Set up Python mode.
;; (This is rather involved, and in desperate need of an overhaul.)

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

  (require 'auto-complete-init)

  (if (not ac-initialized)
      (ac-personal-setup))

  (ac-ropemacs-init)
  (setq ac-sources (append '(ac-source-ropemacs
                             ac-source-ropemacs-dot) ac-sources)))

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

(provide 'python-mode-init)
