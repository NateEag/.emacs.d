(defun autopair-init ()
  "My setup for autopair.el."

  (require 'autopair)
  (autopair-mode)

  ;; In my experience, languages that don't pair backtick are pretty rare.
  ;; DEBUG I don't know how to get this working - I'm not sure it ever has.
  ;; (push '(?` . ?')
  ;;       (getf autopair-extra-pairs :comment))
  ;; (push '(?` . ?')
  ;;       (getf autopair-extra-pairs :string))
  ;; (push '(?` . ?')
  ;;       (getf autopair-extra-pairs :code))

  (diminish 'autopair-mode))
