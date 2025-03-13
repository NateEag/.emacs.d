;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil-cleverparens" "20240529.1025"
  "Evil friendly minor-mode for editing lisp."
  '((evil        "1.0")
    (paredit     "1")
    (smartparens "1.6.1")
    (emacs       "24.4")
    (dash        "2.12.0"))
  :url "https://github.com/emacs-evil/evil-cleverparens"
  :commit "6637717af0bdac55f97eef98433d53a10395cf77"
  :revdesc "6637717af0bd"
  :keywords '("convenience" "emulations")
  :authors '(("Olli Piepponen" . "opieppo@gmail.com"))
  :maintainers '(("Olli Piepponen" . "opieppo@gmail.com")))
