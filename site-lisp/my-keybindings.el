;;; my-keybindings.el --- My global keybindings.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; nothing much to say. Just continuing my slow march towards an organized
;; .emacs.d.
;;
;; It is probably worth noting I could do this in my use-package invocations,
;; but for some reason I prefer having all the keybindings defined in one
;; place. I may well decide this is stupid later.

;;; Code:

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

;; Look up URLs quickly.
(global-set-key (kbd "C-c u") 'browse-url)

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

(provide 'my-keybindings)
;;; my-keybindings.el ends here
