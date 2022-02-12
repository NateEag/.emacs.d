;;; magrant.el --- Transient Interface to Vagrant  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/magrant
;; Package-Requires: ((emacs "25.1")(dash "2.17.0")(s "1.12.0")(tablist "0.70")(transient "0.2.0")(friendly-shell-command "0.2.3"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; An attempt at a Vagrant procelain inside Emacs.
;;
;; Allows you to perform (almost) any action you could do with the command-line at the comfort of an interface inspired by magit.
;;
;; No special configuration is needed, just call `magrant` or directly `magrant-boxes` or `magrant-machines` to get started.
;;
;; All action work on any number of entry.  So you could for example start all your machines at once or SSH into half of them.


;;; Code:



;; REQUIRES

(require 'magrant-core)
(require 'magrant-box)
(require 'magrant-machine)




(provide 'magrant)

;;; magrant.el ends here
