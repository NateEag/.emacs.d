;; Copyright (c) 2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;; Facilitate lazy-require of package.rkt by command-server.rkt, by
;; moving this to its own, non-lazy module.

(provide package-notify-channel)

(define package-notify-channel (make-channel))
