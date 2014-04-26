;; My adaptation of the code for an elegant, minimalist fullscreen mode from
;; "#Emacs, naked": http://bzg.fr/emacs-strip-tease.html
;; Relies on Emacs 24.4's toggle-frame-fullscreen function.

;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; A small minor mode to hide mode line.
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; A small minor mode for a minimal editing environment.
(defvar focus-mode-off-hook nil)
(defvar focus-mode nil)
(define-minor-mode focus-mode
  "A minor mode to make Emacs go full-screen minimalist."
  :init-value nil
  :global t
  :variable focus-mode
  :group 'editing-basics
  (if focus-mode
      (progn
          (add-hook 'focus-mode-off-hook 'focus-mode-off)
          (set-scroll-bar-mode nil)
          (hidden-mode-line-mode 1)
          (toggle-frame-fullscreen)
          ;; On OS X 10.6, toggle-frame-fullscreen takes time to run when
          ;; turning on. If you turn on the big-fringe-mode before it's
          ;; finished, the fringe sizes are all wrong. Yay race conditions.
          (run-at-time "2 sec" nil
                       (lambda () (bzg-big-fringe-mode 1))))))

(defun focus-mode-off (&optional activate)
  "Function to turn off focus mode."
  (set-scroll-bar-mode 'right)
  (bzg-big-fringe-mode 0)
  (hidden-mode-line-mode 0)
  (toggle-frame-fullscreen)
  (remove-hook 'focus-mode-off-hook 'focus-mode-off))
