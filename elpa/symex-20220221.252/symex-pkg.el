(define-package "symex" "20220221.252" "An evil way to edit Lisp symbolic expressions as trees"
  '((emacs "25.1")
    (lispy "0.26.0")
    (paredit "24")
    (evil-cleverparens "20170718.413")
    (evil "1.2.14")
    (evil-surround "1.0.4")
    (hydra "0.15.0")
    (seq "2.22")
    (undo-tree "0.7.5"))
  :commit "520682d49e4e0684d6ee45cfa8d3157e830778b8" :authors
  '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainer
  '("Siddhartha Kasivajhula" . "sid@countvajhula.com")
  :keywords
  '("lisp" "convenience" "languages")
  :url "https://github.com/countvajhula/symex.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
