;;; php-mode-init --- Nate Eagleson's php-mode configuration.

;;; Commentary:
;;;
;;; This is a rough-around-the-edges setup for hacking on PHP in emacs. It uses
;;; lsp-mode and the php-language-server project for PHP intelligence (e.g.,
;;; jump-to-definition and auto-completion), and has scraps of other stuff
;;; floating around the edges (including cruft from probably a decade ago).


;;; Code:

;; Used to use this for highlighting SQL in PHP strings, but it hasn't worked
;; in a long time, so there's no point wasting time on defining it.
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (setq php-sql-mmm-submode-enabled nil)
;; (defun php-sql-mmm-submode ()
;;   "Provides a very minimal embedding of SQL in PHP, via mmm-mode."
;;   (interactive)
;;   (when (not php-sql-mmm-submode-enabled)
;;     (set-face-background 'mmm-default-submode-face nil)
;;     (mmm-add-classes
;;      '((embedded-sql
;;         :submode sql-mode
;;         :front "$\\(sql\\|query\\) = \""
;;         :back "\";"
;;         :face mmm-code-submode-face)))
;;     (mmm-add-mode-ext-class 'php-mode "\\.php$" 'embedded-sql)
;;     (setq php-sql-mmm-submode-enabled t)))


;; jump-to-def and completion-at-point are both working in my current setup.
;;
;; lsp-mode can in principle support a lot more than that, and I have hopes of
;; eventually getting to a state where I can do exploratory programming
;; (relying on auto-complete to show you what's available and give you docs on
;; the different options).
;;
;; The PHP language server itself could still use plenty of improvement, too,
;; but it does seem to be making forward headway.
(require 'lsp-mode)

(require 'lsp-php)

(defun php-mode-init ()
  "Load my particular tweaks for php-mode."

  (interactive)

  (setq-local ne-yas-auto-insert-snippet-name "php-file")

  ;; Turn off aggressive-fill-paragraph-mode if it's on, because my hacked
  ;; version gets deathly slow in php-mode. Not sure why.
  (if (equal aggressive-fill-paragraph-mode t)
      (aggressive-fill-paragraph-mode nil))

  ;; Initialize my php-sql submode.
  ;; Commented, because it breaks even worse than it used to under Emacs 24.
  ;; :(
  ;; (php-sql-mmm-submode)

  ;; Everyone loves code folding.
  (hs-minor-mode-init)

  ;; w00t for auto-yasnippets!
  ;; (In principle the autocomplete setup should usually handle the job, but
  ;; if I ever use this with end-user code, this might be necessary)
  (require 'php-auto-yasnippets)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

  ;; Delete php-mode's tab keybinding, which was interfering with my
  ;; auto-complete setup. Perhaps related to this issue:
  ;;
  ;; https://github.com/ejmr/php-mode/issues/91
  (define-key php-mode-map (kbd "<tab>") nil)

  ;; Set up ac-sources. We have to call payas/ac-setup to make sure the
  ;; ac-source-php-auto-yasnippets is defined, but we don't want it to be the
  ;; first source...
  (payas/ac-setup)
  (setq ac-sources nil)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-php-auto-yasnippets t)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers t)

  ;; Yay for squiggly red lines!
  (setq flycheck-phpcs-standard "PSR2")
  (setq flycheck-php-phpcs-executable "phpcs")

  ;; How about PHP intelligence?
  ;;
  ;; The server takes forever to analyze large projects, but you can mostly
  ;; still use Emacs while it does it.

  (setq lsp-php-language-server-command
        (list "php"
              (expand-file-name "~/third-party/php-language-server/bin/php-language-server.php")))

  (lsp-php-enable))

;; Make this requireable.
(provide 'php-mode-init)
