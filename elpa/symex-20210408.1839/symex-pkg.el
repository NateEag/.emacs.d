(define-package "symex" "20210408.1839" "An evil way to edit Lisp symbolic expressions as trees"
  '((emacs "24.4")
    (lispy "0.26.0")
    (paredit "24")
    (evil-cleverparens "20170718.413")
    (evil "1.2.14")
    (evil-surround "1.0.4")
    (hydra "0.15.0")
    (seq "2.22")
    (undo-tree "0.7.5"))
  :commit "feaf6d847bbff6642cd3c4926899eee3cbac261b" :authors
  '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainer
  '("Siddhartha Kasivajhula" . "sid@countvajhula.com")
  :keywords
  '("lisp" "evil")
  :url "https://github.com/countvajhula/symex.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
