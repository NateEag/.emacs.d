;; So far I'm only using this for jedi-mode, but there's enough complexity to
;; it that it seems like it should have its own setup file.
(add-to-list 'load-path "~/.emacs.d/emacs-epc")
(add-to-list 'load-path "~/.emacs.d/emacs-ctable")
(add-to-list 'load-path "~/.emacs.d/emacs-deferred")

(defun emacs-epc-init ()
  (require 'epc))

(provide 'emacs-epc-init)
