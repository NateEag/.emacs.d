;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "notmuch-addr" "20250117.1240"
  "Improved address completion for Notmuch."
  '((emacs   "29.1")
    (compat  "30.0.2.0")
    (notmuch "0.38.2"))
  :url "https://github.com/tarsius/notmuch-addr"
  :commit "7dde87a44b6576eb736cb6a3b69df6b9946c5ecc"
  :revdesc "7dde87a44b65"
  :keywords '("mail")
  :authors '(("Jonas Bernoulli" . "emacs.notmuch-addr@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.notmuch-addr@jonas.bernoulli.dev")))
