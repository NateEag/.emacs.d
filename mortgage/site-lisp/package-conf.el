;;; package-conf.el --- My package configuration.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:
;;;
;;; Declare the packages I use and how I like them configured.
;;;
;;; The heart of my Emacs mortgage setup.
;;;
;;; Assumes elpaca-init.el has been loaded.

;;; Code:

;; I'm not confident compile-angel is actually better than auto-compile.
;;
;; I'm giving it a try. We'll see how it feels.
(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-path-suffixes'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-path-suffixes)
  (push "/early-init.el" compile-angel-excluded-path-suffixes)
  ;; Files I load directly in my init.el via (require) seem to choke the startup
  ;; process.
  ;;
  ;; TODO: Figure out why and optimize this a bit.
  (push "/elpaca-init.el" compile-angel-excluded-path-suffixes)
  (push "/package-conf.el" compile-angel-excluded-path-suffixes)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files when they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1))

;; Make emacs' built-in help functions more helpful.
;;
;; They're already way better than those of any other program I use to begin
;; with, but helpful does make them noticeably more useful.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; Many minor modes don't need to be documented in the precious space of the
;; modeline.
;;
;; ...and diminish.el is a lovely reflection on the nature of those things we
;; take for granted and thus no longer notice, if you take the time to read the
;; source.
(use-package diminish
  :ensure t)

;; I love project-specific shell environments.
;;
;; Direnv is the best tool I've encountered for them.
(use-package envrc
  :ensure t
  :diminish
  :init
  (envrc-global-mode))

(provide 'package-conf)
;;; package-conf.el ends here
