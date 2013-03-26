;; The following are workarounds since web-mode expects Emacs >= 23, and OS X's
;; built-in Emacs is v. 22. Sadly, they are still not enough to quite make it
;; work.
(defun my-string-match-p (regexp string &optional start)
  "Same as `string-match' except this function does not change the match data."
  (let ((inhibit-changing-match-data t))
(string-match regexp string start)))

(when (not (fboundp 'string-match-p))
  (fset 'string-match-p (symbol-function 'my-string-match-p)))

;; Actually load and configure web-mode.
(add-to-list 'load-path "~/.emacs.d/web-mode")

(autoload 'web-mode "web-mode" "Web template editing mode")
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

(autoload 'autopair-init "autopair-init.el")

(autoload 'auto-complete-init "auto-complete-init.el")

(defun web-mode-init ()
  "My web-mode config."

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 2)

  (comment-auto-fill)

  (autopair-init)
  (push ?{ (getf autopair-dont-pair :code))

  (auto-complete-init))

(add-hook 'web-mode-hook 'web-mode-init)

(provide 'web-mode-init)
