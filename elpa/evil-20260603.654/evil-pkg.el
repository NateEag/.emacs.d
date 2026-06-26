;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20260603.654"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "3b678a221ee99cc6a95b01d7a3129ce5efc4c3da"
  :revdesc "3b678a221ee9"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
