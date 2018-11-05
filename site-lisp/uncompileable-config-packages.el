;;; uncompileable-config-packages.el --- Workaround for auto-compile choking.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; auto-compile dies on files that eventually lead it to compile compressed
;; elisp files. The way I caused this was a use-package invocation that touches
;; on files in core Emacs, and therefore is loaded from byte-compiled versions
;; of files that are themselves compressed and stored with the extension
;; '.el.gz'.
;;
;; I tried adding a function to auto-compile-inhibit-compile-hook that would
;; abort on attempts to compile compressed files, but that had no effect on
;; this auto-compile failure.
;;
;; Hence this workaround - by keeping the actual issue-causer in its own file,
;; I can at least compile most of my use-package logic.

;;; Code:

;; cc-mode defines several derived packages, so this setup will probably grow
;; to cover more than one mode.
(use-package cc-mode
  :defer t
  ;; FIXME This should just map cc-mode to lsp-cquery-enable. However, when I
  ;; do that lsp-cquery-enable runs in PHP files. This is arguably a bug in
  ;; php-mode, which I have filed an issue for:
  ;; https://github.com/ejmr/php-mode/issues/407
  :hook ((c-mode . (lambda ()
                      (when (equal major-mode 'c-mode)
                        (lsp-cquery-enable)))))
  :config
  (progn
    ;; java-mode setup
    (add-hook 'java-mode-hook
              '(lambda ()
                 ;; If eclimd is running, use it.
                 (when (eclimd--running-p)
                   (eclim-mode)
                   (require 'ac-emacs-eclim-source)

                   ;; Set up eclim-mode-specific keybindings.
                   ;; TODO This jumps to def in a different window, and I don't
                   ;; see an equivalent command for returning that I can map to
                   ;; "M-,". Fix that if I start doing more Java stuff.
                   (define-key java-mode-map (kbd "M-.") 'eclim-java-find-declaration)
                   )))))

(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package cquery)

(provide 'uncompileable-config-packages)
;;; uncompileable-config-packages.el ends here
