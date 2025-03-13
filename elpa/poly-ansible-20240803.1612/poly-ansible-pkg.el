;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "poly-ansible" "20240803.1612"
  "Polymode for Ansible: Jinja2 in YAML."
  '((ansible     "0.4.1")
    (ansible-doc "0.4")
    (emacs       "24.1")
    (jinja2-mode "0.2")
    (polymode    "0.2")
    (systemd     "1.4")
    (yaml-mode   "0.0.13"))
  :url "https://gitlab.com/mavit/poly-ansible/"
  :commit "6fcfbb7163f7a74db9da0d54a5ecaec2ac93b315"
  :revdesc "6fcfbb7163f7"
  :keywords '("languages")
  :authors '(("Peter Oliver" . "poly-ansible@mavit.org.uk"))
  :maintainers '(("Peter Oliver" . "poly-ansible@mavit.org.uk")))
