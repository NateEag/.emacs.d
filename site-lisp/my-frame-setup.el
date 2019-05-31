;;; my-frame-setup.el --- configure frames the way I like.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;;

;;; Code:

(defvar my-window-width 83
  "The number of columns I like my emacs windows to have.

The default value of 83 is a result of wrapping code at 80
columns, having 1 extra column on each side for the fringe,
and finally 1 more column of width for git status characters
in the gutter.")

(defun my-get-default-font-size ()
  "Return a good size for my default font based on monitor's pixel density."

  (let ((my-display-pixel-width (display-pixel-width))
        (my-display-mm-width (display-mm-width)))
    (if (and my-display-pixel-width my-display-mm-width)
        ;; Guess a font size based on monitor display density.
        (floor
         (*
          (/ (float (display-pixel-width)) (display-mm-width))
          ;; Multiplier chosen experimentally - there's no smarts to this.
          4.44))

      ;; If we can't compute display density, 14 pixels is a decent default.
      14)))

(defun my-get-default-font-name ()
  "Return the name of my preferred font."
  "Anonymous Pro")

(defun my-get-default-font (&optional size)
  "Return a string specifying my default font."

  (setq my-font-size (if size size (my-get-default-font-size)))
  (setq my-default-font (concat (my-get-default-font-name) "-"
                                (number-to-string my-font-size))))

(defun my-set-default-font (&optional size)
  "Set my default font, if possible, optionally at point size `size`."
  ;; Do not set a font if it is not available - keeps us from crashing in a
  ;; font-free setting.
  (if (member (my-get-default-font-name) (font-family-list))
      (set-frame-font (my-get-default-font size) nil t)))

(defun my-set-up-frame ()
  "Configure current frame's layout and font size based on display size."

  (interactive)

  (my-set-default-font)

  ;; N.B.: This depends on frame-cmds.el, which I installed via MELPA.
  (maximize-frame-vertically)

  (delete-other-windows)

  ;; On smaller displays, I usually only want one window so I have space left
  ;; to look at a browser window while coding. On larger, seeing two code pages
  ;; at once is handy.
  ;;
  ;; FIXME Take current window width into account. What I really care about is
  ;; how many characters of width I can reasonably fit on a given monitor.
  (let* ((frame-screen-ratio
          (/ (float (frame-pixel-width (selected-frame))) (display-pixel-width)))
         (num-windows (if (> frame-screen-ratio 0.34)
                          1
                        2)))
    (my-set-frame-width-by-window-count num-windows)
    ))

(defun default-font-width ()
  "Return the width in pixels of a character in the current
window's default font.

More precisely, this returns the width of the letter ‘m’. If the
font is mono-spaced, this will also be the width of all other
printable characters.

Yanked from https://emacs.stackexchange.com/a/5511/351."

  (let ((window (selected-window))
        (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert "m")
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))

(defun my-set-frame-width-by-window-count (num-windows)
  "Set frame width by number of desired 80-char windows."

  (interactive "N")

  (set-frame-width (selected-frame) (* my-window-width num-windows))

  ;; HACK I never use more than two windows.
 (if (= 2 num-windows)
        (split-window-right)))

(defadvice make-frame-command (after set-up-new-frame activate)
  "After creating a new frame, size it the way I like."
  (my-set-up-frame))

(provide 'my-frame-setup)
;;; my-frame-setup.el ends here
