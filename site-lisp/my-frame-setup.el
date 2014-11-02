;;; my-frame-setup.el --- configure frames the way I like.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;;

;;; Code:

(defun my-get-default-font-size ()
  "Return a good size for my default font based on monitor's pixel density."

  (let ((my-display-pixel-width (display-pixel-width))
        (my-display-mm-width (display-mm-width)))
    (if (and my-display-pixel-width my-display-mm-width)
        ;; Guess a font size based on monitor resolution.
        (floor
         (*
          (/ (float (display-pixel-width)) (display-mm-width))
          4.44))
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
  (let* ((frame-screen-ratio
          (/ (float (frame-pixel-width (selected-frame))) (display-pixel-width)))
         (num-windows (if (> frame-screen-ratio 0.34)
                          1
                        2)))
    ;; 80 chars + 1 for each fringe.
    (set-frame-width (selected-frame) (* 82 num-windows))
    (when (= 2 num-windows)
        (split-window-right))))

(provide 'my-frame-setup)
;;; my-frame-setup.el ends here
