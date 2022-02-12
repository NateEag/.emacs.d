;;; init-new.el --- a step towards a more robust Emacs config.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; My current Emacs configuration has become unwieldy, bloated, slow, and is
;; generally aging poorly.
;;
;; I know a lot more about how to configure Emacs than I did back in 2009, and
;; the state of the art has improved a lot since I first got serious about it.
;;
;; I am thus starting a project of trying a new, more streamlined init file, in
;; hopes that I'll be able to get to something that runs more quickly and with
;; fewer quirks (like the occasional screen blanking mentioned in todo.txt).

;;; Code:

;; Set up my load path and a few other core things.
(load-file (concat user-emacs-directory "site-lisp/bootstrap.el"))

;; TODO Ignore site lisp files, as much as we can. Mac OS X Mojave included
;; some that made make-process take a minimum of five seconds, at least with my
;; setup. :/
;;
;; This change didn't actually fix that, but in principle I want my config to
;; be as isolated from the host system as possible. I wound up customizing an
;; Emacs build to do it
(setq inhibit-default-init t)

;; Enable useful functions that are disabled by default.
(put 'narrow-to-region 'disabled nil)

;; Set up exec-path to inherit values from the shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Set up package-specific autoloads and settings.
;;
;; This is what makes my init take about a minute.
;; (load-file (make-emacs-dir-path "site-lisp/config-packages.el"))

(provide 'init-new)
;;; init-new.el ends here
