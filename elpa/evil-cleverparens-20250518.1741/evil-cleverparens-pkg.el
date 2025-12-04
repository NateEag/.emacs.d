;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil-cleverparens" "20250518.1741"
  "Evil friendly minor-mode for editing lisp."
  '((evil        "1.0")
    (paredit     "1")
    (smartparens "1.6.1")
    (emacs       "24.4")
    (dash        "2.12.0"))
  :url "https://github.com/emacs-evil/evil-cleverparens"
  :commit "4c413a132934695b975004d429b0b0a6e3d8ca38"
  :revdesc "4c413a132934"
  :keywords '("convenience" "emulations")
  :authors '(("Olli Piepponen" . "opieppo@gmail.com"))
  :maintainers '(("Olli Piepponen" . "opieppo@gmail.com")))
