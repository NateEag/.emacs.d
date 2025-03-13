;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "friendly-shell-command" "20230916.1426"
  "Better shell-command API."
  '((emacs                  "24.1")
    (cl-lib                 "0.6.1")
    (dash                   "2.17.0")
    (with-shell-interpreter "0.2.5"))
  :url "https://github.com/p3r7/friendly-shell"
  :commit "5cafa3f6313ce04a47c8996ea1ac6b617d155d46"
  :revdesc "5cafa3f6313c"
  :keywords '("processes" "terminals"))
