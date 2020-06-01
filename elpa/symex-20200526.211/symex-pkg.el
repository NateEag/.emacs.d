(define-package "symex" "20200526.211" "An evil way to edit Lisp symbolic expressions as trees"
  '((emacs "24.4")
    (cl-lib "0.6.1")
    (lispy "0.26.0")
    (paredit "24")
    (evil-cleverparens "20170718.413")
    (dash-functional "2.15.0")
    (evil "1.2.14")
    (smartparens "1.11.0")
    (racket-mode "20181030.1345")
    (geiser "0.10")
    (evil-surround "1.0.4")
    (hydra "0.15.0")
    (cider "0.21.0")
    (slime "2.24"))
  :commit "b35e3622601ca5b27bb8e1b7ab301cc6dd674a62" :keywords
  '("lisp" "evil")
  :authors
  '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainer
  '("Siddhartha Kasivajhula" . "sid@countvajhula.com")
  :url "https://github.com/countvajhula/symex.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
