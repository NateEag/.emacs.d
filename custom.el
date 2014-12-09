(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-commands-on-completing
   (quote
    (delete-backward-char backward-delete-char backward-delete-char-untabify autopair-backspace paredit-backward-delete paredit-backward-delete-word smart-dash-insert)))
 '(ac-trigger-key "TAB")
 '(blink-cursor-blinks 0 nil nil "blink!!!!")
 '(blink-cursor-mode t nil nil "For some reason, I really like blinky cursors.")
 '(c-basic-offset 4 nil nil "4 spaces is, on average, the standard.")
 '(c-default-style (quote ((php-mode . "php") (cc-mode . "linux"))))
 '(column-number-mode t)
 '(create-lockfiles nil nil nil "Since these lockfiles are only respected by Emacs, and I don't think I've ever even shared a machine with another emacser, let alone a specific file, I'm turning this off. It horked the build process for a project at work.")
 '(cursor-type (quote bar) nil nil "I use vertical bar so I can tell quickly when I'm in a buffer without evil-mode.")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(fill-column 79 nil nil "Code should generally be under 80 columns wide for readability.")
 '(glasses-face (quote bold) nil nil "Bold isn't quite strong enough - I'll have to look into this further.")
 '(glasses-original-separator "" nil nil "Don't change existing separators.")
 '(glasses-separate-parentheses-p nil nil nil "spaces before parens are ugly.")
 '(glasses-separator "" nil nil "Rather than putting in separators, just use a different face for camelHumps.")
 '(global-font-lock-mode t nil nil "Everyone likes syntax coloration.")
 '(guess-style-guesser-alist
   (quote
    ((indent-tabs-mode . guess-style-guess-tabs-mode)
     (tab-width . guess-style-guess-tab-width)
     (c-basic-offset . guess-style-guess-c-basic-offset)
     (nxml-child-indent . guess-style-guess-indent)
     (css-indent-offset . guess-style-guess-indent)
     (python-indent . guess-style-guess-indent)
     (js2-basic-offset . guess-style-guess-indent))))
 '(indent-tabs-mode nil nil nil "I dislike using tabs for indentation. Spaces are a simpler way to indent.")
 '(inhibit-startup-screen t nil nil "The emacs startup message is a needless annoyance.")
 '(ispell-extra-args nil)
 '(ispell-program-name "aspell")
 '(js2-indent-switch-body t nil nil "Crockford may not like it, but I do.")
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(line-number-mode t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil nil nil "I don't really use the menus, either.")
 '(mo-git-blame-git-blame-args "-M -C -w" nil nil "Follow movement in a file, look for movement between files in the same commit, and ignore whitespace changes.")
 '(multi-term-dedicated-window-height 24)
 '(php-mode-coding-style (quote psr2) nil nil "PSR-2 is not ideal, but it's a standard.")
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-eslint-rulesdir
           (concat nateeag/dir-locals-dir "node_modules/camel_case"))
     (flycheck-eslint-rulesdir concat nateeag/dir-locals-dir "node_modules/camel_case")
     (eval set
           (make-local-variable
            (quote nateeag/dir-locals-dir))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (flycheck-eslint-rulesdir concat nateeag/project-path "node_modules/camel_case")
     (eval set
           (make-local-variable
            (quote nateeag/project-path))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (scss-compile-at-save)
     (eval add-to-list
           (quote after-save-hook)
           (quote hit-servlet))
     (eval add-hook
           (quote after-save-hook)
           (quote hit-servlet)
           nil t)
     (php-auto-yasnippet-required-files
      (list "~/github/gitlist/vendor/autoload.php"))
     (eval guess-style-guess-all)
     (eval highlight-regexp "^ *"))))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil nil nil "I do not use double-spaces after sentences. Neither should you.")
 '(show-paren-mode t)
 '(smart-dash-c-modes
   (quote
    (c-mode c++-mode objc-mode php-mode web-mode js2-mode)))
 '(tab-width 4 nil nil "When a project prefers tabs, this is the width I usually see.")
 '(tool-bar-mode nil nil nil "The toolbar is an even more needless annoyance.")
 '(transient-mark-mode 1 nil nil "I like seeing my selections.")
 '(visible-bell t nil nil "STOP THE RINGING")
 '(yas-expand-only-for-last-commands
   (quote
    (self-insert-command smart-dash-insert web-mode-smart-dash-insert ac-complete ac-expand)) nil nil "Only trigger yasnippet while first typing. Makes TAB indent even if inadvertantly over a yasnippet's key.")
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil))))
