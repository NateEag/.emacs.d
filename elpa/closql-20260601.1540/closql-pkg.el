;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20260601.1540"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (emacsql  "4.4")
    (llama    "1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "d382e7427f5d375ffc872851b049e9f9c4a43dfc"
  :revdesc "d382e7427f5d"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
