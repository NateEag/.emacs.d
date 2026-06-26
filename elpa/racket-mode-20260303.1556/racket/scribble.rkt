;; Copyright (c) 2013-2026 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/format
         racket/match
         racket/promise
         racket/string
         (only-in scribble/core
                  tag?
                  content->string)
         scribble/blueboxes
         scribble/manual-struct
         scribble/xref
         scribble/tag
         setup/main-doc
         "define-fallbacks.rkt"
         "lib-pkg.rkt"
         "util.rkt"
         "xref.rkt")

;; Fallbacks when new index structs aren't available (before
;; scribble-lib 1.54, which ~= Racket 8.14.0.6).
(define-fallbacks scribble/manual-struct
  [(exported-index-desc*? _) #f]
  [(exported-index-desc*-extras _) #hasheq()]
  [(index-desc? _) #f]
  [(index-desc-extras _) #hasheq()])

(provide binding->path+anchor
         identifier->bluebox
         bluebox-command
         doc-search
         doc-families
         module-doc-path
         refresh-doc-index!)

(module+ test
  (require rackunit))

(define/contract (binding->path+anchor stx)
  (-> identifier? (or/c #f (cons/c path-string? (or/c #f string?))))
  (let* ([xref (get-xref)]
         [tag (xref-binding->definition-tag xref stx 0)]
         [p+a (and tag (tag->path+anchor xref tag))])
    p+a))

(define (tag->path+anchor xref tag)
  (define-values (path anchor) (xref-tag->path+anchor xref tag))
  (and path anchor (cons path anchor)))

;;; Blueboxes

(define bluebox-cache #f)

(define (get-bluebox-string tag)
  (unless bluebox-cache
    (set! bluebox-cache (make-blueboxes-cache #t)))
  (match (fetch-blueboxes-strs tag
                               #:blueboxes-cache bluebox-cache)
    [(list* _kind strs)
     (string-replace (string-join strs "\n")
                     "\u00A0"
                     " ")]
    [_ #f]))

(define/contract (identifier->bluebox stx)
  (-> identifier? (or/c #f string?))
  (match (xref-binding->definition-tag (get-xref) stx 0)
    [(? tag? tag) (get-bluebox-string tag)]
    [_ #f]))

(define (bluebox-command str)
  (match (read (open-input-string str))
    [(? tag? tag) (get-bluebox-string tag)]
    [_ #f]))

(module+ test
  (cond
    ;; racket-doc has an unfortunate dependency -- gui-lib -- meaning
    ;; it might not be installed on a headless server. See #738.
    [(not (xref-binding->definition-tag (get-xref) #'cons 0))
     (displayln "Skipping identifier->bluebox tests (racket-doc seemingly not installed).")]
    [else
     ;; This test succeeds on all Racket versions before and after 6.10.
     ;; I spent an hour installing 6.10 locally and exploring the problem
     ;; but so far have no clue. As neither 6.10 nor I are getting any
     ;; younger, I am choosing to ignore this, for now.
     ;;
     ;; Probably https://github.com/racket/drracket/issues/118
     (check-equal? (identifier->bluebox #'list)
                   "(list v ...) -> list?\n  v : any/c")
     (check-false (identifier->bluebox (datum->syntax #f (gensym))))]))

;;; Documentation search

;; A trie where a Node's children are represented as an alist from
;; char to Node. Each Node also has zero or more values (multiple,
;; because we have so many duplicate keys e.g. various flavors of
;; "define" or "print"). The values are promises: Although the trie is
;; built for all search terms, the initial promise contains just a
;; `desc` and tag; the full doc-trie-value is not computed until
;; needed for a search.
(struct Node
  ([kids   #:mutable] ;(list/c (cons/c char? node?))
   [values #:mutable] ;(list/c (promise/c doc-trie-value))
   ))
(define (empty-node) (Node null null))

(define doc-index-trie-root #f)

(define (refresh-doc-index!)
  (set! doc-index-trie-root
        (with-memory-use/log "build-doc-search-trie"
          (with-time/log "build-doc-search-trie"
            (build-doc-search-trie)))))

(define (doc-search prefix [limit 256])
  (unless doc-index-trie-root
    (refresh-doc-index!))
  (trie-find doc-index-trie-root prefix limit))

(define (alist-ref a k default-thunk)
  (define ((make-pred a) b)
    (char=? a b))
  (match (assf (make-pred k) a)
    [(cons _k v) v]
    [_ (default-thunk)]))

;; Find values for all nodes for which `prefix` is an exact match, and
;; up to `limit` nodes for which `prefix` is an initial match.
;; Oriented toward producing completion candidates.
(define (trie-find root prefix limit)
  (match (string->list prefix)
    [(list) null]
    [chs
     (define results null)
     (let find! ([node root]
                 [chs  chs])
       (match-define (cons ch more-chs) chs)
       (define sub-node (alist-ref (Node-kids node) ch empty-node))
       (cond
         [(null? more-chs)
          (define (report! node)
            (for ([v (in-list (Node-values node))])
              (set! results (cons (force v) results)))
            (when (< (length results) limit)
              (for ([kv (in-list (Node-kids node))])
                (report! (cdr kv)))))
          (report! sub-node)]
         [else
          (find! sub-node more-chs)]))
     results]))

(define (trie-add! root str value)
  (let add! ([node root]
             [chs  (string->list str)])
    (match-define (cons ch more-chs) chs)
    (define sub-node
      (alist-ref (Node-kids node) ch
                 (λ ()
                   (define sub-node (empty-node))
                   (define new-mapping (cons ch sub-node))
                   (set-Node-kids! node
                                   (cons new-mapping
                                         (Node-kids node)))
                   sub-node)))
    (cond
      [(null? more-chs)
       (set-Node-values! sub-node
                         (cons value (Node-values sub-node)))]
      [else
       (add! sub-node more-chs)])))

(define (build-doc-search-trie)
  (define (get-extras desc . keys)
    (and (exported-index-desc*? desc)
         (let ([ht (exported-index-desc*-extras desc)])
           (for/or ([key (in-list keys)]) (hash-ref ht key #f)))))
  (define (hide? desc)
    ;; Don't show doc for constructors; class doc suffices.
    (or (constructor-index-desc? desc)
        (get-extras desc 'hidden? 'constructor?)))
  (define root (empty-node))
  (define (add! term desc tag)
    (trie-add! root
               term
               (delay (doc-trie-value desc term tag))))
  (for* ([entry (in-list (xref-index (get-xref)))]
         [desc (in-value (entry-desc entry))]
         #:when desc
         #:unless (hide? desc)
         [term (in-value (car (entry-words entry)))]
         [also (in-value (get-extras desc 'long-key))]
         [tag (in-value (entry-tag entry))])
    (add! term desc tag)
    (when also
      (add! also desc tag)))
  root)

(define (doc-trie-value desc term tag)
  (define-values (path anchor) (xref-tag->path+anchor (get-xref) tag))
  (define (method-what)
    (cond
      [(method-tag? tag)
       (define-values (c/i _m) (get-class/interface-and-method tag))
       (format "method of ~a" c/i)]
      [else "method"]))
  (define (doc-from)
    (match (path->main-doc-relative path)
      [(list* 'doc dir _) (~a "in " dir)]
      [_ ""]))
  (define-values (what from fams pkg-sort sort-order)
    (cond
      ;; New structs
      [(exported-index-desc*? desc)
       (define ht (exported-index-desc*-extras desc))
       (define kind (hash-ref ht 'kind))
       (define what (if (string=? kind "method")
                        (method-what)
                        kind))
       (define from
         (string-join (match (hash-ref ht 'display-from-libs #f)
                        [(? list? contents)
                         (map content->string contents)]
                        [#f
                         (map ~s (exported-index-desc-from-libs desc))])
                      ", "))
       (define fams (match (hash-ref ht 'language-family #f)
                      [(? list? fams) fams]
                      [#f '("Racket")]))
       (define pkg-sort (lib-pkg-sort
                         (match (exported-index-desc-from-libs desc)
                           [(cons lib _) lib]
                           [_            #f])))
       (define sort-order (hash-ref ht 'sort-order 0))
       (values what from fams pkg-sort sort-order)]
      [(index-desc? desc)
       (define ht (index-desc-extras desc))
       (define module-kind (hash-ref ht 'module-kind #f))
       (define what (match module-kind
                      ['lib    "module"]
                      ['lang   "language"]
                      ['reader "reader"]
                      [#f      "documentation"]
                      [v       (~a v)]))
       (define from
         (match (hash-ref ht 'display-from-libs #f)
           [(? list? contents)
            (string-join (map content->string contents) ", ")]
           [#f
            (match module-kind
              [(or 'lib 'lang 'reader) term]
              [_ (doc-from)])]))
       (define fams (match (hash-ref ht 'language-family #f)
                      [(? list? fams) fams]
                      [#f '("Racket")]))
       (define pkg-sort (lib-pkg-sort
                         (match module-kind
                           ['lib (string->symbol term)]
                           [_    #f])))
       (define sort-order (hash-ref ht 'sort-order 0))
       (values what from fams pkg-sort sort-order)]
      ;; Older structs
      [(exported-index-desc? desc)
       (define what
         (match desc
           [(? language-index-desc?)  "language"]
           [(? reader-index-desc?)    "reader"]
           [(? form-index-desc?)      "syntax"]
           [(? procedure-index-desc?) "procedure"]
           [(? thing-index-desc?)     "value"]
           [(? struct-index-desc?)    "structure"]
           [(? class-index-desc?)     "class"]
           [(? interface-index-desc?) "interface"]
           [(? mixin-index-desc?)     "mixin"]
           [(? method-index-desc?)    (method-what)]
           [_ ""]))
       (define from (string-join (map ~s (exported-index-desc-from-libs desc)) ", "))
       (define pkg-sort (lib-pkg-sort
                         (match (exported-index-desc-from-libs desc)
                           [(cons lib _) lib]
                           [_            #f])))
       (values what from null pkg-sort 0)]
      [(module-path-index-desc? desc)
       (define pkg-sort (lib-pkg-sort (string->symbol term)))
       (values "module" "" "" pkg-sort 0)]
       [else
       (define pkg-sort (lib-pkg-sort #f))
       (values "documentation" (doc-from) null pkg-sort 0)]))
  (list term sort-order what from fams pkg-sort path anchor))

;; This is for package-details
(define (module-doc-path mod-path-str lang?)
  (for/or ([v (in-list (doc-search mod-path-str 0))])
    (match-define (list term _sort what _from _fams _pkg path anchor) v)
    (and (equal? term mod-path-str)
         (equal? what (if lang? "language" "module"))
         (cons path anchor))))

;; Documentation language family information added in Racket 9.1.
(define doc-families
  ;; Require some functions both dynamically (in case don't exist in
  ;; older Racket) and lazily (faster initial load of back end).
  (let ([get-main-language-family #f]
        [get-language-families #f]
        [get-doc-search-dirs #f])
    (λ ()
      (define-syntax-rule (req/set! mod fn def-val)
        (unless fn
          (set! fn (safe-dynamic-require 'mod 'fn (λ () (λ () def-val))))))
      (req/set! setup/dirs get-main-language-family "Racket")
      (req/set! setup/language-family get-language-families null)
      (req/set! setup/dirs get-doc-search-dirs null)
      ;; Return cons of main family name, and, list of details for all
      ;; families. Adjust the details somewhat, from what
      ;; get-language-families supplies.
      (define xref (get-xref))
      (define doc-dirs (get-doc-search-dirs))
      (cons
       (get-main-language-family)
       (for/list ([ht (in-list (get-language-families))])
         (let* (;; Convert abstract module paths to concrete absolute paths.
                [ht (for/hash ([(k v) (in-hash ht)])
                      (cond [(memq k '(doc start-doc describe-doc))
                             (values k (module-path->absolute-path xref v))]
                            [else
                             (values k v)]))]
                ;; If describe-doc not mapped, use doc.
                [ht (cond
                      [(hash-has-key? ht 'describe-doc) ht]
                      [(hash-ref ht 'doc #f)
                       => (λ (v) (hash-set ht 'describe-doc v))]
                      [else ht])]
                ;; If start-doc not mapped, use doc, else use
                ;; family-root. See similar logic in
                ;; send-language-family-page and send-main-page.
                [ht (cond
                      [(hash-has-key? ht 'start-doc) ht]
                      [(hash-ref ht 'doc #f)
                       => (λ (v) (hash-set ht 'start-doc v))]
                      [(hash-ref ht 'family-root #f)
                       =>
                       (λ (sub)
                         (or (for/or ([dir (in-list doc-dirs)])
                               (define path (build-path dir sub "index.html"))
                               (and (file-exists? path)
                                    (hash-set ht 'start-doc path)))
                             ht))]
                      [else ht])]
                [ht (hash-remove ht 'doc)])
           (hash->list ht)))))))

;; Convert mod path => part tag => absolute path.
;; See send-language-family-page.

(define (module-path->absolute-path xref mp)
  (define-values (path _anchor)
    (xref-tag->path+anchor xref (module-path->part-tag mp)))
  path)

(define (module-path->part-tag mp)
  `(part (,(format "~a" mp) "top")))
