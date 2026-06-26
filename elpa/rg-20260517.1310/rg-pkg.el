;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20260517.1310"
  "A search tool based on ripgrep."
  '((emacs     "28.1")
    (transient "0.9.2")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "e46a16b8bdba111c9c0036d0e209490dd7a3690f"
  :revdesc "e46a16b8bdba"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
