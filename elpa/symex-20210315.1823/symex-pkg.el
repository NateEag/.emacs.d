(define-package "symex" "20210315.1823" "An evil way to edit Lisp symbolic expressions as trees"
  '((emacs "24.4")
    (lispy "0.26.0")
    (paredit "24")
    (evil-cleverparens "20170718.413")
    (dash "2.18.0")
    (evil "1.2.14")
    (smartparens "1.11.0")
    (evil-surround "1.0.4")
    (hydra "0.15.0")
    (seq "2.22")
    (undo-tree "0.7.5"))
  :commit "86396c399bd366582df585de3fe3590bb3544c13" :authors
  '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainer
  '("Siddhartha Kasivajhula" . "sid@countvajhula.com")
  :keywords
  '("lisp" "evil")
  :url "https://github.com/countvajhula/symex.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
