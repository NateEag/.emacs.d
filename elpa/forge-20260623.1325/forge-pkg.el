;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20260623.1325"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "31.0")
    (closql        "2.4")
    (cond-let      "1.1")
    (emacsql       "4.4")
    (ghub          "5.2.1")
    (llama         "1.0")
    (magit         "4.5")
    (markdown-mode "2.8")
    (transient     "0.13")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "0c9407833bd688f83ad496b37ea71381e758d65b"
  :revdesc "0c9407833bd6"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
