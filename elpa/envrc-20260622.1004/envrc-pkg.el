;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20260622.1004"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "28.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "77e9dec1563bc204cc9e086cd8a7d3622196224c"
  :revdesc "77e9dec1563b"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
