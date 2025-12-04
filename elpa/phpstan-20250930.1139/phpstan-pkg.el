;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "phpstan" "20250930.1139"
  "Interface to PHPStan (PHP static analyzer)."
  '((emacs       "25.1")
    (compat      "30")
    (php-mode    "1.22.3")
    (php-runtime "0.2"))
  :url "https://github.com/emacs-php/phpstan.el"
  :commit "07ef7531f2ec73b90a965ac865cca8c96086f9de"
  :revdesc "07ef7531f2ec"
  :keywords '("tools" "php")
  :authors '(("USAMI Kenta" . "tadsan@zonu.me"))
  :maintainers '(("USAMI Kenta" . "tadsan@zonu.me")))
