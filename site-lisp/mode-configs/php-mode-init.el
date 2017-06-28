;;; php-mode-init --- Nate Eagleson's php-mode configuration.

;;; Commentary:
;;;
;;; Emacs is not a first-class environment for editing PHP. php-mode has some
;;; neat features, but really good jump-to-def and context-aware auto-complete
;;; are, as far as I know, an unsolved problem.
;;;
;;; Still, this setup eases many of the pain points of writing PHP.
;;;
;;; ...at least, it did five years ago. Now lsp-mode and the
;;; php-language-server project promise to make hacking PHP in Emacs not suck,
;;; but the edges are extremely rough so far.
;;;
;;; Since I'm writing PHP again for the first time in years, I'm starting to
;;; work on getting a really good PHP env up and running, but my old config has
;;; gotten quite broken in the meantime.


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

(defun php-mode-init ()
  "Load my particular tweaks for php-mode."
  (interactive)

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

  ;; Set up ac-sources. We have to call payas/ac-setup to make sure the
  ;; ac-source-php-auto-yasnippets is defined, but we don't want it to be the
  ;; first source...
  (payas/ac-setup)
  (setq ac-sources nil)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-php-auto-yasnippets t)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers t)

  ;; Turn on lsp-mode by default. The server takes forever to analyze large
  ;; projects, but you can mostly still use Emacs while it does it.
  (lsp-mode 1)

  ;; Yay for squiggly red lines!
  (setq flycheck-phpcs-standard "PSR2")
  (setq flycheck-php-phpcs-executable "phpcs"))

;; This is a very primitive config for using
;; https://github.com/emacs-lsp/lsp-mode with
;; https://github.com/felixfbecker/php-language-server (which I'm running
;; straight out of a local dev repo) to get PHP intelligence inside Emacs.
;;
;; Thus far, I've only tried jump-to-def within the php-language-server project
;; itself. That did work, so this is definitely a direction worth pursuing.
(require 'lsp-mode)
(lsp-define-stdio-client 'php-mode "php" 'stdio
                         ;; TODO Use Projectile to find project root.
                         (lsp-make-traverser ".git")
                         "php-lang-server"
                         ;; TODO Quit using my own script for this.
                         "php-lang-server")

;; Make this requireable.
(provide 'php-mode-init)
