(defun autopair-init ()
  "My setup for autopair.el."

  (require 'autopair)
  (autopair-mode)

  ;; In my experience, languages that don't pair backtick are pretty rare.
  (push '(?`)
        (getf autopair-extra-pairs :comment))
  (push '(?`)
        (getf autopair-extra-pairs :string))
  (push '(?`)
        (getf autopair-extra-pairs :code))

  (diminish 'autopair-mode))
