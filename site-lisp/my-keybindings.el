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

;; Change flycheck's prefix-key to "C-c e". Code is taken from a docstring in
;; flycheck.
(eval-after-load 'flycheck
  '(progn
      (define-key flycheck-mode-map flycheck-keymap-prefix nil)
      (setq flycheck-keymap-prefix (kbd "C-c e"))
      (define-key flycheck-mode-map flycheck-keymap-prefix
        flycheck-command-map)))

(defvar nateeag/command-mnemonics
  '(
    ;; Jump to any place in the current buffer quickly.
    ("a w" . evil-avy-goto-word-1)
    ("a p" . ace-window)

    ("e" . eval-buffer)

    ;; Change names from snake_case to ALL_CAPS to StudlyCaps to camelCase.
    ("u" . string-inflection-toggle)

    ;; Switch buffers/find likely files via Helm.
    ("b" . helm-for-files)

    ;; TODO Make this just toggle between snake_case and camelCase. They're
    ;; what I usually use.
    ("c" . string-inflection-toggle)

    ("f" . helm-find-files)

    ;; A few project-related keybinds.
    ;; ...all this stuff kinda makes me think I should look into hydra.
    ;; TODO Make/find a command that finds file in project *if* in project,
    ;; falls back to just finding files across my machine, and also provides
    ;; all open buffers. Basically, I don't want to have to think about where
    ;; files live or whether I've opened it unless necessary.
    ("p f" . helm-projectile-find-file)
    ("p s" . helm-projectile-ag)
    ("p b" . helm-projectile-switch-to-buffer)
    ("p p" . helm-projectile-switch-project)

    ;; Look up URLs quickly.
    ("u" . browse-url)

    ;; g is for git, which is oh so much fun.
    ("g g" . magit-status)
    ("g n" . git-gutter:next-hunk)
    ("g p" . git-gutter:previous-hunk)
    ("g d" . git-gutter:popup-diff)
    ("g r" . git-gutter:revert-hunk)

    ;; Dates and times are handy to be able to insert.
    ("d" . insert-date)
    ("t" . insert-time)

    ("o" . other-window)

    ;; HTML involves lots of extraneous angle brackets. Emmet-mode makes typing
    ;; it less annoying.
    ("j" . emmet-expand-yas)

    ;; Search through buffers with helm-swoop.
    ("s" . helm-swoop)

    ;; Expand-region lets you select delimited regions quickly.
    ("r" . er/expand-region)

    ;; Try out multi-term as my terminal emulator.
    ("m" . multi-term-dedicated-toggle)

    ;; because I've gotten used to a shortcut inspired by vim saving.
    ("w" . save-buffer)

    ;; M-x is too much work, and F8 is a bit of a reach.
    ("x" . helm-M-x)

    ;; Jump-to-def and go-back-where-I-came from are things I like to be able
    ;; to do from normal-mode.
    ;;
    ;; TODO Do this in a smarter way. Binding straight to these function defs
    ;; doesn't work for non-LSP backends like Tern.
    ;;
    ;; ...though I probably am going to move away from Tern soon since it's not
    ;; supported any more, so y'know. Mileage may vary.
    ("." . xref-find-definitions)
    ("," . xref-pop-marker-stack)

    ;; DEBUG These don't do what I'd like. They open new windows, and I'd like
    ;; them to just move to the next term-buffer in my selected window.
    ("[" . multi-term-prev)
    ("]" . multi-term-next)

    ;; toggle-quote lets you toggle a string between single- and double-quoted.
    ;; This will probably be deprecated in favor of evil-surround, once I'm more
    ;; fluent in evil-mode.
    ("'" . toggle-quotes)
    )
  "An alist mapping mnemonics to commands.

Used to define keyboard shortcuts.")

(defun nateeag/create-keybindings ()
  "Generate keybindings from `nateeag/command-mnemonics'."

  (interactive)

  ;; TODO Should unset prefix keys by parsing mnemonic in the loop. This is WET.
  (global-unset-key (kbd "s-a"))
  (global-unset-key (kbd "s-p"))
  (global-unset-key (kbd "s-g"))

  (dolist (elt nateeag/command-mnemonics)
    (let ((mnemonic (car elt))
          (command (cdr elt)))

      ;; Use Super for my personal keybindings. The Emacs manual says that C-c
      ;; <key> is reserved for user keybindings, but in practice there's not a
      ;; lot mapped to the Super key.
      ;;
      ;; TODO Stop using Super bindings? Or at least use them for something
      ;; else. I'm becoming so much a vimmer that I almost only need the leader
      ;; key.
      ;;
      ;; I should probably look into hydra, too, if it comes to that...
      (global-set-key (kbd (concat "s-" mnemonic)) command)

      ;; Tell evil-leader-mode to use the mnemonic for this command.
      ;; Note that evil-leader expects key sequences to have no separating
      ;; whitespace.
      (evil-leader/set-key (s-replace " " "" mnemonic) command))) )

(nateeag/create-keybindings)

(provide 'my-keybindings)
;;; my-keybindings.el ends here
