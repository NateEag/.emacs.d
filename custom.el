(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-commands-on-completing
   (quote
    (delete-backward-char backward-delete-char backward-delete-char-untabify autopair-backspace paredit-backward-delete paredit-backward-delete-word smart-dash-insert)))
 '(ac-trigger-key "TAB")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(guess-style-guesser-alist
   (quote
    ((indent-tabs-mode . guess-style-guess-tabs-mode)
     (tab-width . guess-style-guess-tab-width)
     (c-basic-offset . guess-style-guess-c-basic-offset)
     (nxml-child-indent . guess-style-guess-indent)
     (css-indent-offset . guess-style-guess-indent)
     (python-indent . guess-style-guess-indent)
     (js2-basic-offset . guess-style-guess-indent))))
 '(ispell-program-name "aspell")
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(magit-use-overlays nil)
 '(php-mode-coding-style (quote psr2) nil nil "PSR-2 is not ideal, but it's a standard.")
 '(safe-local-variable-values
   (quote
    ((php-auto-yasnippet-required-files
      (list "~/github/gitlist/vendor/autoload.php"))
     (eval guess-style-guess-all)
     (eval highlight-regexp "^ *"))))
 '(smart-dash-c-modes (quote (c-mode c++-mode objc-mode php-mode web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil))))
