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
      (self-insert-command 1)
    (smart-dash-insert)))

(defun web-mode-install-smart-dash-insert ()
  "When called, override smart-dash-mode's usual keybinding for '-'."
  (let ((map smart-dash-mode-keymap))
    (make-local-variable 'smart-dash-mode-keymap)
    (setq smart-dash-mode-keymap (copy-keymap map)))
  (define-key smart-dash-mode-keymap "-" 'web-mode-smart-dash-insert))

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

  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))
                 )))

  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
          ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ;; DEBUG It would be nice to have an ac-source for CSS property names
          ;; as well as values. Since auto-complete already has the alist
          ;; mapping property names to legal values, I should add one that uses
          ;; the keys and issue a merge request.
          ("css" . (ac-source-css-property
                    ac-source-css-property-names
                    ac-source-emmet-css-snippets))))

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

  (define-key tagedit-mode-map (kbd "C-k") 'web-mode-tagedit-kill)

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

(defun web-mode-tagedit-kill ()
  "Call tagedit-kill if in HTML and kill-line otherwise."
  (interactive)
  (if (equal (web-mode-language-at-pos) "html")
      (tagedit-kill)
    (kill-line)))

(provide 'web-mode-init)
;;; web-mode-init.el ends here
