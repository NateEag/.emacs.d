;;; eldoc-overlay-mode.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Keywords: eldoc overlay
;; Package-Version: 20170826.434
;; URL: https://github.com/stardiviner/eldoc-overlay-mode
;; Created: 14th Jan 2017
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (inline-docs "1.0.1"))

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------

(defvar eldoc-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defcustom eldoc-overlay-function 'inline-docs
  "Specify the function for displaying eldoc.
Two functions currently supported: `inline-docs', and `eldoc-overlay-mode-quick-peek'.")

(defun eldoc-overlay-disable-in-org-mode ()
  (setq-local eldoc-message-function #'eldoc-minibuffer-message))


;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc with contextual documentation overlay."
  :init-value t
  :lighter " ElDoc overlay"
  :keymap eldoc-overlay-mode-map
  :global t
  (if eldoc-overlay-mode
      (progn
        (setq eldoc-message-function eldoc-overlay-function)
        (add-hook 'org-mode-hook #'eldoc-overlay-disable-in-org-mode))
    (setq eldoc-message-function #'eldoc-minibuffer-message)
    )
  )

;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay-mode)

;;; eldoc-overlay-mode.el ends here
