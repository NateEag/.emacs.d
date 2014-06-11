;;; web-mode-init.el -- Configure web-mode for my personal use.

;;; Commentary:

;; web-mode is a major-mode for editing HTML templates. Here, I define my
;; mode-hook for it, and several supporting functions for libraries that
;; don't work with it seamlessly out-of-the-box.

;;; Code:

(defun web-mode-smart-dash-insert ()
  "A wrapper around smart-dash-mode for use with web-mode."
  ;; Only be smart about dashes in languages where it makes sense.
  (if (member (web-mode-language-at-pos) (list "html" "css"))
      (self-insert-command)
    (smart-dash-insert)))

(defun web-mode-install-smart-dash-insert ()
  "When called, override smart-dash-mode's usual keybinding for '-'."
  (let ((map smart-dash-mode-keymap))
    (make-local-variable 'smart-dash-mode-keymap)
    (setq smart-dash-mode-keymap (copy-keymap map)))
  (define-key smart-dash-mode-keymap "-" 'web-mode-smart-dash-insert))

(defvar web-mode-before-auto-complete-hooks nil
  "List of functions to run before triggering the auto-complete library.

Auto-complete sources will sometimes need some tweaking to work
nicely with web-mode. This hook gives users the chance to adjust
the environment as needed for ac-sources, right before they're used.")

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (plist-get (web-mode-point-context (point)) :language)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil))
               )))

(defvar web-mode-ac-sources-alist nil
  "alist mapping language names as string to auto-complete sources for that language.")

(defcustom web-mode-ac-trigger-key nil
  "Non-nil means `auto-complete' will start by typing this key.
It will first set ac-sources based on the current context."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Key"))
  :group 'auto-complete
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and value
                    (fboundp 'ac-set-trigger-key))
           (web-mode-ac-set-trigger-key value))))

(defun web-mode-ac-set-trigger-key (key)
  "Set `ac-trigger-key' to `KEY'."
  ;; Remove old mapping
  (when web-mode-ac-trigger-key
    (define-key ac-mode-map (read-kbd-macro ac-trigger-key) nil))

  ;; Make new mapping
  (setq web-mode-ac-trigger-key key)
  (when key
    (define-key
      ac-mode-map
      (read-kbd-macro key)
      'web-mode-ac-trigger-key-command)))

(defun web-mode-ac-trigger-key-command (&optional force)
  "Trigger auto-complete if relevant. Else, fallback to prev keybinding."
  (interactive "P")
  (let (started)
    (when (or force (ac-trigger-command-p last-command))
        (run-hooks 'web-mode-before-auto-complete-hooks)
        (let ((new-web-mode-ac-sources
               (assoc (plist-get (web-mode-point-context (point)) :language)
                      web-mode-ac-sources-alist)))
          (setq ac-sources (cdr new-web-mode-ac-sources)))
      (setq started (auto-complete-1 :triggered 'trigger-key)))
    (unless started
      (ac-fallback-command 'web-mode-ac-trigger-key-command))))

(defun web-mode-init ()
  "My web-mode config."

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)
  (setq web-mode-comment-style 2)

  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 0)

  (skewer-html-mode)

  (comment-auto-fill)

  (yas-minor-mode)
  (yas-activate-extra-mode 'php-mode)

  (smartparens-mode)

  (auto-complete-mode)

  (emmet-mode)

  ;; Prototype of web-mode context-aware auto-complete config.
  ;; If we can move the "before completion starts" hook into auto-complete,
  ;; we'd only need to set up web-mode-ac-sources-alist.
  (custom-set-variables '(web-mode-ac-trigger-key "TAB"))
  (auto-complete-tab-noconflict)
  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-php-auto-yasnippets ac-source-yasnippet))
          ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ;; DEBUG It would be nice to have an ac-source for CSS property names
          ;; as well as values. Since auto-complete already has the alist
          ;; mapping property names to legal values, I should add one that uses
          ;; the keys and issue a merge request.
          ("css" . (ac-source-css-property))))

  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (tagedit-mode 1)

  ;; See if we can hack in support for tagedit at all... web-mode seems like it
  ;; must already have most of these covered in some way.
  (set (make-local-variable 'te/skip-tag-forward-fn)
       'web-mode-te-skip-tag-forward-fn)

  ;; Similarly
  (set (make-local-variable 'te/skip-tag-backward-fn)
       'web-mode-te-skip-tag-backward-fn)

  (set (make-local-variable 'te/empty-tag-p-fn)
       'web-mode-element-is-void)

  (set (make-local-variable 'te/current-tag-fn)
       'web-mode-te-current-tag-fn)

  (setq smart-dash-c-modes (cons 'web-mode smart-dash-c-modes))
  (smart-dash-mode)
  (web-mode-install-smart-dash-insert))

(defun web-mode-te-skip-tag-forward-fn ()
  "Move to end of current (or next) tag.

web-mode's nearest equivalent that I could find only handles current tag.
Hence this wrapper."
  (skip-syntax-forward " <")
  (web-mode-element-end))

(defun web-mode-te-skip-tag-backward-fn ()
  "Move to beginning of current (or previous) tag.

web-mode's nearest equivalent that I could find only handles current tag.
Hence this wrapper."
  (skip-syntax-backward " >")
  (backward-char 1)
  (web-mode-element-beginning))

(defun web-mode-te-current-tag-fn ()
  "Return current tag alist for tagedit."
  (ignore-errors
    (save-excursion
     (let* ((beg (web-mode-element-beginning-position))
            ;; web-mode's end-position gets the position of the closing >,
            ;; while tagedit expects the position *after* it.
            (end (+ 1 (web-mode-element-end-position)))
            (name (get-text-property beg 'tag-name))
            (self-closing (if (web-mode-element-is-void name)
                              :t
                            :f)))
       `((:name . ,name)
         (:self-closing . ,self-closing)
         (:beg . ,beg)
         (:end . ,end))))))

(provide 'web-mode-init)
;;; web-mode-init.el ends here
