;; Copyright (c) 2025 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;; Facilitate lazy-require of hash-lang-bridge.rkt by
;; command-server.rkt, by moving this to its own, non-lazy module.

(require racket/async-channel)

(provide hash-lang-notify-channel)

(define hash-lang-notify-channel (make-async-channel))

