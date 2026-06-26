;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "notmuch-transient" "20260601.1531"
  "Command dispatchers for Notmuch."
  '((emacs     "29.1")
    (compat    "31.0")
    (notmuch   "0.39")
    (transient "0.13"))
  :url "https://github.com/tarsius/notmuch-transient"
  :commit "41090076ad90b579ec48058d3edd135a1b9d05b5"
  :revdesc "41090076ad90"
  :keywords '("mail")
  :authors '(("Jonas Bernoulli" . "emacs.notmuch-transient@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.notmuch-transient@jonas.bernoulli.dev")))
