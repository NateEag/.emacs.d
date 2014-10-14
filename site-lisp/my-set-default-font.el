;; A few functions I'm evolving for choosing my default font.
;;
;; ...and my default frame config, it seems. Probably should rename this file.

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

  (interactive)
  ;; Do not set a font if it is not available - keeps us from crashing in a
  ;; font-free setting.
  (if (member (my-get-default-font-name) (font-family-list))
      (set-frame-font (my-get-default-font size) nil t)))

(defun my-set-up-frame (frame)
  "Set up `FRAME' the way I like."

  ;; N.B.: This depends on frame-cmds.el, which I installed via MELPA.
  (maximize-frame-vertically)

  ;; Two 80-column windows. The extra chars are for the fringes.
  (set-frame-width frame 164)

  (delete-other-windows)
  (split-window-right))
