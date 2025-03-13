;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "notmuch-transient" "20250117.1241"
  "Command dispatchers for Notmuch."
  '((emacs     "29.1")
    (compat    "30.0.2.0")
    (notmuch   "0.38.2")
    (transient "0.8.2"))
  :url "https://github.com/tarsius/notmuch-transient"
  :commit "4902879d93402e140a048def88f8fec8cf45d9cb"
  :revdesc "4902879d9340"
  :keywords '("mail")
  :authors '(("Jonas Bernoulli" . "emacs.notmuch-transient@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.notmuch-transient@jonas.bernoulli.dev")))
