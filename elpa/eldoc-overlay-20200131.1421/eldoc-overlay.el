;;; eldoc-overlay.el --- Display eldoc with contextual documentation overlay.

;; Author: stardiviner <numbchild@gmail.com>
;; Author: Robert Weiner <rsw@gnu.org>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: documentation, eldoc, overlay
;; Package-Version: 20200131.1421
;; URL: https://github.com/stardiviner/eldoc-overlay
;; Created:  14th Jan 2017
;; Modified: 18th Dec 2017
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (inline-docs "1.0.1") (quick-peek "1.0"))

;;; Commentary:
;;
;; Eldoc displays the function signature of the closest function call
;; around point either in the minibuffer or in the modeline.
;;
;; This package modifies Eldoc to display this documentation inline
;; using a buffer text overlay.
;;
;;  `eldoc-overlay-mode' is a per-buffer minor mode.
;;    A call to `eldoc-overlay-enable' turns it on.
;;    A call to `eldoc-overlay-disable' turns it off
;;
;;    {C-x C-h} interactively calls `eldoc-overlay-toggle' and tells
;;    you the mode's new state.
;;
;;  `global-eldoc-overlay-mode' can be used to toggle this for all buffers.
;;    A call to `global-eldoc-overlay-enable' turns it on.
;;    A call to `global-eldoc-overlay-disable' turns it off
;;
;;    {C-u C-x C-h} interactively calls `global-eldoc-overlay-toggle' and tells
;;    you the mode's new state.
;;
;; By default, the overlay is not used in the minibuffer, eldoc is shown in the modeline
;; in this case.  Set the option `eldoc-overlay-in-minibuffer-flag' non-nil if you want
;; to enable overlay use in the minibuffer.
;;
;; Finally, see the documentation for `eldoc-overlay-backend' if you want to try
;; a different overlay display package backend.

;;; Code:
;;; ----------------------------------------------------------------------------

(require 'eldoc)

;; User Options
(defgroup eldoc-overlay nil
  "Display Eldoc function signatures using in-buffer text overlays"
  :prefix "eldoc-overlay-"
  :group 'eldoc)

(defcustom eldoc-overlay-in-minibuffer-flag nil
  "Non-nil (default: nil) means enable `eldoc-overlay-mode' in the minibuffer.
When nil and in the minibuffer, if standard `eldoc-mode' is
enabled, it displays function signatures in the modeline."
  :type 'boolean
  :group 'eldoc-overlay)


;; Variables
(defvar eldoc-overlay-backend 'quick-peek
  "The backend library that displays eldoc overlays.
Two backends are supported: `inline-docs' and `quick-peek'.")

;; Functions
(defun eldoc-overlay-inline-docs (format-string &rest args)
  "Inline-docs backend function to show FORMAT-STRING and ARGS."
  (inline-docs format-string args))

(defun eldoc-overlay-quick-peek (format-string &rest args)
  "Quick-peek backend function to show FORMAT-STRING and ARGS."
  (when format-string
    (quick-peek-show
     (apply 'format format-string args)
     (point)
     1)))

(defun eldoc-overlay-display (format-string &rest args)
  "Display eldoc for the minibuffer when there or call the function indexed by `eldoc-overlay-backend'."
  (unless (company-tooltip-visible-p)
    (if (and (minibufferp) (not eldoc-overlay-in-minibuffer-flag))
        (apply #'eldoc-minibuffer-message format-string args)
      (funcall
       (pcase eldoc-overlay-backend
	       (`inline-docs 'eldoc-overlay-inline-docs)
         (`quick-peek 'eldoc-overlay-quick-peek))
	     (funcall eldoc-documentation-function)))))

;;;###autoload
(define-minor-mode eldoc-overlay-mode
  "Minor mode for displaying eldoc contextual documentation using a text overlay."
  :require 'eldoc-overlay-mode
  :group 'eldoc-overlay
  :init-value nil
  :global t
  :lighter " ElDocOver"
  (if eldoc-overlay-mode
      (progn
        (eldoc-mode 1)
        (setq eldoc-message-function #'eldoc-overlay-display)
        (when (eq eldoc-overlay-backend 'quick-peek)
          (add-hook 'post-command-hook #'quick-peek-hide)))
    (quick-peek-hide)
    ;; Remove hook when no buffers have any peek overlays
    (unless (delq nil (mapcar (lambda (buf) (buffer-local-value 'quick-peek--overlays buf)) (buffer-list)))
      (remove-hook 'post-command-hook #'quick-peek-hide))
    (setq eldoc-message-function #'eldoc-minibuffer-message)))

;;; ----------------------------------------------------------------------------

(provide 'eldoc-overlay)

;;; eldoc-overlay.el ends here
