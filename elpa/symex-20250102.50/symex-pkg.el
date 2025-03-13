;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "symex" "20250102.50"
  "An evil way to edit Lisp symbolic expressions as trees."
  '((emacs             "25.1")
    (tsc               "0.15.2")
    (tree-sitter       "0.15.2")
    (lispy             "0.26.0")
    (paredit           "24")
    (evil-cleverparens "20170718.413")
    (evil              "1.2.14")
    (evil-surround     "1.0.4")
    (hydra             "0.15.0")
    (seq               "2.22"))
  :url "https://github.com/countvajhula/symex.el"
  :commit "94e5040f0c24f870b7217d8ddf6675c6b17eaff4"
  :revdesc "94e5040f0c24"
  :keywords '("lisp" "convenience" "languages")
  :authors '(("Siddhartha Kasivajhula" . "sid@countvajhula.com"))
  :maintainers '(("Siddhartha Kasivajhula" . "sid@countvajhula.com")))
