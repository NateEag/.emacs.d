;;; ne-evil-textobjects.el --- A few custom text objects for evil-mode.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; There are some text objects I keep wishing I had.
;;
;; A quick search turned up one of them that's not available on MELPA, as far
;; as I can see, so I figured I'd just throw it in here and maybe someday add a
;; few more.

;;; Code:

(require 'evil)

;; The c-defun textobjects are taken from:
;; http://seanbowman.me/blog/emacs-evil-function-objects/
;;
;; and seem to work in C-like languages, elisp, and Python.
;;
;; Renamed the prefix just for the sake of file namespacing.

(evil-define-text-object ne/textobj-inner-c-defun (count &optional beg end type)
  (save-excursion
    (mark-defun)
    (re-search-forward "{")
    (exchange-point-and-mark)
    (re-search-backward "}")
    (evil-range (region-beginning) (region-end) type :expanded t)))

;; FIXME Include trailing semicolons in JS.
;;
;; FIXME Include comments immediately preceding the functions.
(evil-define-text-object ne/textobj-outer-c-defun (count &optional beg end type)
  :type line
  (save-excursion
    (mark-defun)
    (if (looking-at "[:space:]*$")
        (forward-line))
    (exchange-point-and-mark)
    (unless (save-excursion
              (forward-line)
              (looking-at "[:space:]*$"))
      (forward-line))
    (evil-range (region-beginning) (region-end) type :expanded t)))

;; Let me talk about URLs using vim operators.
;;
;; FIXME Make this actually work.
(evil-define-text-object ne/textobj-url (count &optional beg end type)
  (save-excursion
    (let* ((bounds (thing-at-point-bounds-of-url-at-point)))
      (list (car bounds) (cdr bounds)))))

(defun ne/install-textobjects ()
  "Install my custom textobjects into evil's keymaps."

  ;; FIXME 'd' might be a terrible binding for this. It's a reference to Emacs
  ;; Lisps' defun.
  (define-key evil-inner-text-objects-map "d" 'ne/textobj-inner-c-defun)
  (define-key evil-outer-text-objects-map "d" 'ne/textobj-outer-c-defun)

  (define-key evil-inner-text-objects-map "U" 'ne/textobj-url)
  )

(provide 'ne-evil-textobjects)
;;; ne-evil-textobjects.el ends here
