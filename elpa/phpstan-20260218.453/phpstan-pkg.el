;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "phpstan" "20260218.453"
  "Interface to PHPStan (PHP static analyzer)."
  '((emacs       "25.1")
    (compat      "30")
    (php-mode    "1.22.3")
    (php-runtime "0.2"))
  :url "https://github.com/emacs-php/phpstan.el"
  :commit "77fba8fe9d63661e940b392a0e4b573e7edafb7b"
  :revdesc "77fba8fe9d63"
  :keywords '("tools" "php")
  :authors '(("USAMI Kenta" . "tadsan@zonu.me"))
  :maintainers '(("USAMI Kenta" . "tadsan@zonu.me")))
