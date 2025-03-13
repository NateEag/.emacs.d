;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require (only-in scribble/core tag?)
         scribble/xref
         racket/contract
         racket/format
         racket/match
         "../identifier.rkt"
         "../xref.rkt")

(provide doc)

;; Once upon a time, you could enter commands in the REPL like ",doc".
;; It made sense to open the browser here -- despite needing junk to
;; convince macOS to open a file: URL using anchors a.k.a. fragments.
;;
;; But nowadays? Just return the URL. Let Emacs open the browser.
;; Especially because now check-syntax sometimes returns a help URL,
;; in which case the front end should just open the browser. Given
;; that case, let's always open the browser one consistent way -- in
;; Emacs using browse-url.

(define/contract (doc how str)
  (-> how/c string? (or/c #f string?))
  (->identifier how str stx->uri-string))

(define (stx->uri-string stx)
  (define xref (get-xref))
  (match (and xref (xref-binding->definition-tag xref stx 0))
    [(? tag? tag)
     (define-values (path anchor) (xref-tag->path+anchor xref tag))
     (~a "file://" path "#" anchor)]
    [_ #f]))
