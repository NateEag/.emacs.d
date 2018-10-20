(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ignore-case (quote smart) nil nil "Use the case I used, blast it. Case-insensitivity should be opt-in. :P")
 '(ac-max-width 0.5)
 '(ac-trigger-commands-on-completing
   (quote
    (delete-backward-char backward-delete-char backward-delete-char-untabify autopair-backspace paredit-backward-delete paredit-backward-delete-word smart-dash-insert ne-smart-dash-hacks-sh-mode-insert)))
 '(ac-trigger-key "TAB")
 '(afp-always-fill-on-self-insert t)
 '(afp-fill-after-functions
   (quote
    (evil-delete backward-delete-char backward-delete-char-untabify kill-region evil-change yank yank-pop evil-paste-after)))
 '(afp-fill-comments-only-mode-list
   (quote
    (emacs-lisp-mode sh-mode python-mode js-mode php-mode xml-mode nxml-mode diff-mode gitconfig-mode)))
 '(afp-fill-keys
   (quote
    (32 46 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90)) nil nil "A very stupid way to make paragraphs fill fluidly both while writing and editing - make sure the hook runs after all letters. It would be smarter to submit a PR to bypass the filter var in the first place, probably, maybe with support for filling after deletion (if that is feasible).")
 '(afp-fill-on-self-insert t)
 '(ag-ignore-list (quote ("node_modules")))
 '(ascii-window-size 10)
 '(auto-insert-query nil)
 '(auto-revert-verbose t nil nil "I can't remember ever caring that auto-revert did its job. Thus, I'm asking it to stop telling me.")
 '(blink-cursor-blinks 0 nil nil "blink!!!!")
 '(blink-cursor-mode t nil nil "For some reason, I really like blinky cursors.")
 '(c-basic-offset 4 nil nil "4 spaces is, on average, the standard.")
 '(c-default-style (quote ((php-mode . "php") (cc-mode . "linux"))))
 '(column-number-mode t)
 '(cquery-executable "~/third-party/cquery/build/release/bin/cquery")
 '(create-lockfiles nil nil nil "Since these lockfiles are only respected by Emacs, and I don't think I've ever even shared a machine with another emacser, let alone a specific file, I'm turning this off. It horked the build process for a project at work.")
 '(cursor-type (quote bar) nil nil "I use vertical bar so I can tell quickly when I'm in a buffer without evil-mode.")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(debug-on-quit nil)
 '(delete-selection-mode t)
 '(evil-mode-line-format nil nil nil "Since I use a block cursor in normal mode and a bar for insert, I don't need to see my evil-state in the modeline.")
 '(evil-want-fine-undo t)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "EMAIL")) nil nil "Since I use notmuch and mbsync at work and home, it's convenient to have Emacs pick up my EMAIL var.")
 '(fill-column 79 nil nil "Code should generally be under 80 columns wide for readability.")
 '(flyspell-issue-message-flag nil)
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
     (js2-indent-level . guess-style-guess-indent)
     (js-switch-indent-offset . guess-style-guess-indent)
     (web-mode-markup-indent-offset . guess-style-guess-indent)
     (web-mode-css-indent-offset . guess-style-guess-indent)
     (web-mode-code-indent-offset . guess-style-guess-indent))))
 '(helm-for-files-preferred-list
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir)))
 '(indent-tabs-mode nil nil nil "I dislike using tabs for indentation. Spaces are a simpler way to indent.")
 '(inhibit-startup-screen t nil nil "The emacs startup message is a needless annoyance.")
 '(ispell-extra-args (quote ("--ignore-case")) nil nil "When programming, sometimes you lowercase things that are normally uppercased. I should figure out how to do this only in prog-modes, but I'm lazy and I rarely screw up capitalization.")
 '(ispell-program-name "aspell")
 '(js2-indent-switch-body t nil nil "Crockford may not like it, but I do.")
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(ledger-reconcile-default-commodity "USD" nil nil "I'm not sure whether I should use '$' or 'USD', but I have USD at present.")
 '(line-number-mode t)
 '(lsp-php-show-file-parse-notifications nil nil nil "I don't really care about these messages. I just want to know when things fail.")
 '(lsp-print-io nil)
 '(magit-blame-arguments (quote ("-w" "-M" "-C" "-C" "-C")) nil nil "Look hard for places other than right here that the text may have originated. See git-blame manual page for details.")
 '(magit-diff-refine-hunk (quote all))
 '(magit-diff-use-overlays nil)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t t)
 '(magit-use-overlays nil)
 '(mailcap-download-directory "~/Downloads")
 '(markdown-command "cmark")
 '(max-specpdl-size 5000 nil nil "I get the \"Variable binding depth exceeds max-specpdl-size\" error a lot when using lsp-mode on a large PHP codebase at $DAYJOB. I'm therefore bumping this up to see if it helps.")
 '(menu-bar-mode nil nil nil "I don't really use the menus, either.")
 '(message-kill-buffer-on-exit t nil nil "If I have sent a message, I have little reason to keep the buffer around.")
 '(mm-inline-large-images (quote resize))
 '(mo-git-blame-git-blame-args "-M -C -w" nil nil "Follow movement in a file, look for movement between files in the same commit, and ignore whitespace changes.")
 '(moody-slant-function (quote moody-slant-apple-rgb))
 '(multi-term-dedicated-window-height 24)
 '(notmuch-address-command (quote internal))
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-saved-searches
   (quote
    ((:name "Learn Tech" :query "tag:learn-tech")
     (:name "Third-party software" :query "tag:third-party-software" :sort-order nil)
     (:name "inbox" :query "tag:inbox AND tag:unread" :key "i" :sort-order nil)
     (:name "unread" :query "tag:unread" :key "u" :sort-order nil)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order nil)
     (:name "sent" :query "tag:sent" :key "t" :sort-order nil)
     (:name "Unread Emails In Watched Threads" :query "tag:watched and tag:unread" :key "w")
     (:name "drafts" :query "tag:draft" :key "d" :sort-order nil)
     (:name "all mail" :query "*" :key "a" :sort-order nil)
     (:name "Bug Reports" :query "tag:bug-reports")
     (:name "Sent" :query "from:neagleson@nxtbookmedia.com" :sort-order newest-first :search-type nil))) nil nil "Oldest first is generally the most convenient sort order, but in a few cases I have customized it, such as in Sent, where I'm usually looking for something I sent recently.")
 '(notmuch-search-oldest-first t nil nil "This is the default value, but I wanted to explicitly record that I prefer it, after trying both. If you're running a workflow, you usually want to see the oldest thing first, as all else being equal older emails should get responses sooner than newer ones. When you're just searching, if you get lots of results you can just refine your search to find what you're looking for, so the order isn't really relevant there.")
 '(notmuch-show-all-tags-list t nil nil "I like seeing these. I'm not exactly sure why.")
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (elisp-def git-gutter origami free-keys helm-rg elpygen lsp-ui magit-svn pdf-tools dynamic-spaces sicp neotree polymode eldoc-overlay-mode flycheck-objc-clang web-mode-edit-element jq-mode csv string-edit aggressive-fill-paragraph evil-magit evil-matchit evil-surround flycheck-css-colorguard ag nginx-mode lorem-ipsum git-blamed regex-tool s virtualenvwrapper use-package undo-tree solarized-theme macrostep puppet-mode pip-requirements omnisharp multiple-cursors markdown-mode helm-projectile groovy-mode expand-region evil-commentary beacon aggressive-indent ace-window evil-escape writeroom-mode unicode-troll-stopper toggle-quotes tern-auto-complete tea-time tagedit sublimity string-inflection speed-type smart-tabs-mode skewer-reload-stylesheets scss-mode sane-term rainbow-mode pyimpsort pyenv-mode popwin php-auto-yasnippets names multi-term mocha-snippets mo-git-blame minimap md-readme literate-coffee-mode know-your-http-well key-chord json-mode jscs js-doc jedi iss-mode hideshowvis helm-swoop helm-ls-git helm-git-files gitignore-mode gitconfig-mode gitattributes-mode git-timemachine frame-cmds focus-autosave-mode exec-path-from-shell evil-tutor evil-leader evil-indent-textobject evil-exchange esup elisp-slime-nav el2markdown diffview cygwin-mount cycle-quotes compact-docstrings cask-mode browse-url-dwim browse-kill-ring bats-mode backup-walker auto-compile ascii apache-mode ace-jump-mode ac-slime ac-ispell ac-html-csswatcher ac-html ac-helm ac-emmet shell-pop git-gutter-fringe+ gnuplot gnuplot-mode slack yasnippet-classic-snippets)))
 '(php-mode-coding-style (quote psr2) nil nil "PSR-2 is not ideal, but it's a standard.")
 '(php-template-compatibility nil nil nil "I use web-mode for PHP templates, and therefore I really don't want php-mode trying to deal with them.")
 '(projectile-indexing-method (quote alien))
 '(projectile-use-git-grep t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((helm-ag--extra-options . "--ignore elpa/archives")
     (ne-yas-auto-insert-snippet-name . "how-i-code-post")
     (ne-yas-auto-insert-snippet-name . "package")
     (eval setq flycheck-eslint-rulesdir
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
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil nil nil "I do not use double-spaces after sentences. Neither should you.")
 '(shell-pop-full-span t)
 '(shell-pop-universal-key "C-'")
 '(show-paren-mode t)
 '(smart-dash-c-modes
   (quote
    (c-mode c++-mode objc-mode php-mode web-mode js2-mode)))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type (quote ssl))
 '(tab-width 4 nil nil "When a project prefers tabs, this is the width I usually see.")
 '(tool-bar-mode nil nil nil "The toolbar is an even more needless annoyance.")
 '(tramp-default-method "ssh" t)
 '(transient-mark-mode 1 nil nil "I like seeing my selections.")
 '(undo-tree-auto-save-history nil nil nil "This is disabled in hopes of working around an intermittent bug in undo-tree.el that eats undo history. Rumor has it turning this off may help.")
 '(undo-tree-enable-undo-in-region nil nil nil "I have turned this off in hopes of defeating the horrible history-eating bug in undo-tree.el that's documented in my readme.")
 '(visible-bell t nil nil "STOP THE RINGING")
 '(word-wrap t nil nil "I can't believe I went this long not realizing I could just set this to get more readable word wrap, while still seeing logical lines as logical lines.")
 '(yas-also-auto-indent-first-line t)
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
