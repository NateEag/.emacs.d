;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250301.2353"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.2.0")
    (closql        "2.2.1")
    (emacsql       "4.2.0")
    (ghub          "4.2.2")
    (let-alist     "1.0.6")
    (llama         "0.6.1")
    (magit         "4.3.1")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.8.5")
    (yaml          "1.2.0"))
  :url "https://github.com/magit/forge"
  :commit "1c904090dfdcd201d9170997052c43846ddce149"
  :revdesc "1c904090dfdc"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
