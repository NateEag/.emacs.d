;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "notmuch-addr" "20251101.2052"
  "Improved address completion for Notmuch."
  '((emacs   "29.1")
    (compat  "30.1")
    (notmuch "0.38"))
  :url "https://github.com/tarsius/notmuch-addr"
  :commit "0883cd753f0a7a204f41c7311d3282ca3e53f869"
  :revdesc "0883cd753f0a"
  :keywords '("mail")
  :authors '(("Jonas Bernoulli" . "emacs.notmuch-addr@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.notmuch-addr@jonas.bernoulli.dev")))
