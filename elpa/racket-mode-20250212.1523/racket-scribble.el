;;; racket-scribble.el -*- lexical-binding: t -*-

;; Copyright (c) 2021-2024 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'dom)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'url-util)
(require 'tramp)

(eval-when-compile
  (unless (fboundp 'dom-remove-node)    ;added circa Emacs 27
    (defun dom-remove-node (dom node)
      "Remove NODE from DOM."
      ;; If we're removing the top level node, just return nil.
      (dolist (child (dom-children dom))
        (cond
         ((eq node child)
          (delq node dom))
         ((not (stringp child))
          (dom-remove-node child node))))))

  (unless (fboundp 'dom-search)         ;added circa Emacs 27
    (defun dom-search (dom predicate)
      "Return elements in DOM where PREDICATE is non-nil.
PREDICATE is called with the node as its only parameter."
      (let ((matches (cl-loop for child in (dom-children dom)
			      for matches = (and (not (stringp child))
					         (dom-search child predicate))
			      when matches
			      append matches)))
        (if (funcall predicate dom)
	    (cons dom matches)
          matches)))))

(defconst racket--scribble-temp-nbsp #x2020
  "Character we substitute for #xA0 non-breaking-space.

We do this because HTML rendered by Scribble relies heavily on
tables and &nbsp; for layout. But `shr-insert-document' treats nbsp
aka #xA0 as a plain, breaking space, and furthermore deletes
leading spaces in <td> elements.

After doing a `shr-insert-document' you need to replace this in
the buffer with a plain space, e.g.

  (goto-char (point-min))
  (while (re-search-forward (string racket--scribble-temp-nbsp) nil t)
    (replace-match \" \" t t))

This will ensure that the non-breaking-space chars actually have
the effect of being non-breaking.")

(defun racket--scribble-path->shr-dom (path)
  (let* ((tramp-verbose 2) ;avoid excessive messages
         (base (file-name-directory path))
         (dom  (with-temp-message (format "Getting %s..." path)
                 (racket--html-file->dom path)))
         (body (with-temp-message (format "Adjusting %s..." path)
                 (racket--massage-scribble-dom path
                                               base
                                               (dom-child-by-tag dom 'body)))))
    `(html ()
           (head () (base ((href . ,base))))
           ,body)))

(defun racket--html-file->dom (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (libxml-parse-html-region (point-min) (point-max))))

;; Dynamically bound (like Racket parameters).
(defvar racket--scribble-file nil)
(defvar racket--scribble-base nil)

(defun racket--massage-scribble-dom (file base dom)
  "Simplify the HTML so that `shr-insert-document' renders better.
In some cases we resort to returning custom elements for
`racket-describe' to handle specially."
  (let ((racket--scribble-file file)
        (racket--scribble-base base))
    (save-match-data
      (racket--walk-dom dom))))

(defsubst racket--walk-kids (v)
  "A DRY convenience for `racket--walk-dom'. `defsubst' to
avoid penalty."
  (mapcar #'racket--walk-dom (dom-children v)))

(defun racket--walk-dom (v)
  "Recursively walk and massage the dom V."
  (cond
   ;; Optimization: First do fast checks for frequent, atomic
   ;; elements.
   ((stringp v) (subst-char-in-string #xA0 racket--scribble-temp-nbsp v))
   ((numberp v) (string v))
   ((symbolp v) (racket--html-char-entity-symbol->string v))
   (t
    (pcase (dom-tag v)
      ('span
       (pcase (dom-attr v 'class)
         ;; Ignore new <span class="button-group"> elements.
         ("button-group" `(span))
         ;; <span class="mywbr"> </span> added in e.g. "tocsub" for
         ;; "case/equal". As rendered in shr, undesired space.
         ("mywbr" "")
         (_
          (pcase (dom-attr v 'style)
            ;; For some reason scribble renders this, which shr
            ;; doesn't handle, instead of <i>, which it does.
            ("font-style: italic"
             `(i () ,@(racket--walk-kids v)))
            (_
             `(span ,(dom-attributes v) ,@(racket--walk-kids v)))))))

      ('p
       (pcase (dom-attr v 'class)
         ;; Unwanted blank lines or indents
         ("RForeground"
          `(div () ,@(mapcar #'racket--walk-dom (dom-children v))))
         (_
          `(p ,(dom-attributes v) ,@(racket--walk-kids v)))))
      ('div
       (pcase (dom-attr v 'class)
         ;; Page navigation.
         ("navsettop"
          (pcase-let* ((navleft (car (dom-by-class v "navleft")))
                       (top (dom-attr (car (dom-by-tag navleft 'a)) 'href))
                       (navright (car (dom-by-class v "navright")))
                       (`(,prev ,up ,next)
                        (mapcar (lambda (v) (dom-attr v 'href))
                                (dom-by-tag navright 'a))))
            (if (and top up)
                `(racket-nav
                  ((top  . ,(expand-file-name top racket--scribble-base))
                   (prev . ,(and prev (expand-file-name prev racket--scribble-base)))
                   (up   . ,(expand-file-name up racket--scribble-base))
                   (next . ,(and next (expand-file-name next racket--scribble-base)))))
              `(span))))
         ("navsetbottom" `(span))
         ;; The kind (e.g. "procedure" or "syntax"): Add <hr>
         ("RBackgroundLabel SIEHidden"
          `(div ()
                (hr)
                (span ((class . "RktCmt")) ,(dom-texts v))))
         ;; Change SIntrapara div to p, which helps shr supply
         ;; sufficient line-breaks.
         ("SIntrapara"
          `(p () ,@(racket--walk-kids v)))
         (_
          `(div ,(dom-attributes v) ,@(racket--walk-kids v)))))
      ('table
       (pcase (dom-attr v 'class)
         ;; Hack: Handle tables of class "RktBlk" whose tr's contain
         ;; only a single td --- which, weirdly, Scribble uses for
         ;; code blocks like "Examples" --- by "un-table-izing" them
         ;; to simple divs. This is to prevent shr from trying too
         ;; hard to handle table widths and indent but just messing it
         ;; up for code blocks (e.g. the first and second lines will
         ;; be indented too much).
         ("RktBlk"
          `(div ()
                ,@(mapcar
                   (pcase-lambda (`(tr ,_ (td ,_ . ,xs)))
                     ;; Unwrap Rkt{Res Out Err} in a <p> that causes excess
                     ;; line breaks.
                     (let ((xs (pcase xs
                                 (`((p ,_ . ,xs)) xs)
                                 (xs              xs))))
                       `(div () ,@(mapcar #'racket--walk-dom xs))))
                   (dom-children v))))
         ;; Hack: Ensure blank line after defmodule blocks
         ("defmodule"
          `(div ()
                (table () ,@(racket--walk-kids v))
                (p ())))
         (_
          `(table ,(dom-attributes v) ,@(racket--walk-kids v)))))
      ('a
       ;; Replace some <a> with <racket-anchor> because shr in Emacs
       ;; 25.2 doesn't seem to handle these well.
       (if-let (name (dom-attr v 'name))
           `(racket-anchor ,(dom-attributes v) ,@(racket--walk-kids v))
         ;; Replace <a> with <racket-doc-link> or <racket-ext-link>.
         ;; The former are links to follow using racket-describe-mode,
         ;; the latter using browse-url (a general-purpose, probably
         ;; external web browser).
         (if-let (href (dom-attr v 'href))
             (cond
              ;; Handle "local-redirect" links. Scribble writes these
              ;; as external links, and generates
              ;; doc/local-redirect.js to adjust these on page load.
              ;; Partially mimic that js here.
              ((or
                (string-match         ;as for installed releases
                 "^https?://download.racket-lang.org/releases/[^/]+/doc/local-redirect/index.html[?]\\(.*\\)$"
                 href)
                (string-match       ;as for local builds from source
                 "^https?://docs.racket-lang.org/local-redirect/index.html[?]\\(.*\\)$"
                 href)
                (string-match      ;as for installed snapshot builds
                 "^https?://.+?/snapshots/[^/]+/doc/local-redirect/index.html[?]\\(.*\\)$"
                 href))
               (let ((qps (url-parse-query-string (match-string 1 href))))
                 (if (assoc "tag" qps)
                     ;; don't handle
                     `(span () ,@(racket--walk-kids v))
                   ;; Assume local-redirect.js has a "boring"
                   ;; link_dirs where the second element of each
                   ;; sub-array is simply the first one with "../"
                   ;; prepended. We can simply use the value of the
                   ;; `doc` query parameter with "../" prepended.
                   (let* ((doc (cadr (assoc "doc" qps)))
                          (rel (cadr (assoc "rel" qps)))
                          (rel-path (concat "../" doc "/" rel))
                          (abs-path (expand-file-name rel-path racket--scribble-base)))
                     ;; recur to do our usual path/anchor processing for
                     ;; local hrefs
                     (racket--walk-dom
                      `(a ((href  . ,abs-path)
                           (class . ,(dom-attr v 'class)))
                          ,@(dom-children v)))))))
              ;; Some other, truly external links
              ((or (string-match-p "^https?://" href)
                   (string-match-p "^mailto:" href))
               `(racket-ext-link ((href  . ,href)
                                  (class . ,(dom-attr v 'class)))
                                 ,@(racket--walk-kids v)))
              ;; Lazy hack to remove the "go to specific" links on the
              ;; top doc/index.html page. FIXME: Instead remove entire
              ;; paragraph?
              ((string-match-p "#$" href) `(span))
              ;; Otherwise the general case is some combo of local
              ;; path and/or anchor.
              (t
               (pcase-let*
                   ((`(,path . ,anchor)
                     (save-match-data
                       (cond
                        ((equal href "")
                         (cons racket--scribble-file nil))
                        ((string-match "^#\\(.+\\)$" href)
                         (cons racket--scribble-file (match-string 1 href)))
                        ((string-match "^\\(.*\\)#\\(.+\\)$" href)
                         (cons (expand-file-name (match-string 1 href)
                                                 racket--scribble-base)
                               (match-string 2 href)))
                        ((string-match "^\\(.+\\)$" href)
                         (cons (expand-file-name (match-string 1 href)
                                                 racket--scribble-base)
                               nil))
                        (t (error "unexpected href")))))
                    (anchor (and anchor (url-unhex-string anchor))))
                 `(racket-doc-link ((path   . ,path)
                                    (anchor . ,anchor)
                                    (class  . ,(dom-attr v 'class)))
                                   ,@(racket--walk-kids v)))))
           `(span () ,@(racket--walk-kids v)))))
      ('blockquote
       (pcase (dom-attr v 'class)
         ;; Unwanted blank lines or indents
         ((or "SVInsetFlow" "SubFlow")
          `(span () ,@(mapcar #'racket--walk-dom (dom-children v))))
         (_
          `(blockquote ,(dom-attributes v) ,@(racket--walk-kids v)))))
      ('img
       (pcase (dom-attr v 'src)
         ;; Finger or magnifier images in refpara blocks: Replace with
         ;; &loz;
         ((or "finger.png" "magnify.png")
          `(span () (strong () ,(racket--html-char-entity-symbol->string 'loz))))
         ;; Images generally: Convert src to "data:" uri scheme,
         ;; (Otherwise shr would try to `url-queue-retrieve' these.)
         (_
          (dom-set-attribute v
                             'src
                             (racket--scribble-file->data-uri
                              (expand-file-name (dom-attr v 'src)
                                                racket--scribble-base)))
          v)))
      (tag
       `(,tag ,(dom-attributes v) ,@(racket--walk-kids v)))))))

(defun racket--scribble-file->data-uri (image-file-name)
  (concat
   "data:image/png;base64,"
   (with-temp-buffer
     (insert-file-contents-literally image-file-name)
     (base64-encode-region (point-min) (point-max) t)
     (buffer-string))))

(defconst racket--html-char-entities
  `((quot     . 34)
    (amp      . 38)
    (apos     . 39)
    (lt       . 60)
    (gt       . 62)
    (nbsp     . ,racket--scribble-temp-nbsp)
    (iexcl    . 161)
    (cent     . 162)
    (pound    . 163)
    (curren   . 164)
    (yen      . 165)
    (brvbar   . 166)
    (sect     . 167)
    (uml      . 168)
    (copy     . 169)
    (ordf     . 170)
    (laquo    . 171)
    (not      . 172)
    (shy      . 173)
    (reg      . 174)
    (macr     . 175)
    (deg      . 176)
    (plusmn   . 177)
    (sup2     . 178)
    (sup3     . 179)
    (acute    . 180)
    (micro    . 181)
    (para     . 182)
    (middot   . 183)
    (cedil    . 184)
    (sup1     . 185)
    (ordm     . 186)
    (raquo    . 187)
    (frac14   . 188)
    (frac12   . 189)
    (frac34   . 190)
    (iquest   . 191)
    (Agrave   . 192)
    (Aacute   . 193)
    (Acirc    . 194)
    (Atilde   . 195)
    (Auml     . 196)
    (Aring    . 197)
    (AElig    . 198)
    (Ccedil   . 199)
    (Egrave   . 200)
    (Eacute   . 201)
    (Ecirc    . 202)
    (Euml     . 203)
    (Igrave   . 204)
    (Iacute   . 205)
    (Icirc    . 206)
    (Iuml     . 207)
    (ETH      . 208)
    (Ntilde   . 209)
    (Ograve   . 210)
    (Oacute   . 211)
    (Ocirc    . 212)
    (Otilde   . 213)
    (Ouml     . 214)
    (times    . 215)
    (Oslash   . 216)
    (Ugrave   . 217)
    (Uacute   . 218)
    (Ucirc    . 219)
    (Uuml     . 220)
    (Yacute   . 221)
    (THORN    . 222)
    (szlig    . 223)
    (agrave   . 224)
    (aacute   . 225)
    (acirc    . 226)
    (atilde   . 227)
    (auml     . 228)
    (aring    . 229)
    (aelig    . 230)
    (ccedil   . 231)
    (egrave   . 232)
    (eacute   . 233)
    (ecirc    . 234)
    (euml     . 235)
    (igrave   . 236)
    (iacute   . 237)
    (icirc    . 238)
    (iuml     . 239)
    (eth      . 240)
    (ntilde   . 241)
    (ograve   . 242)
    (oacute   . 243)
    (ocirc    . 244)
    (otilde   . 245)
    (ouml     . 246)
    (divide   . 247)
    (oslash   . 248)
    (ugrave   . 249)
    (uacute   . 250)
    (ucirc    . 251)
    (uuml     . 252)
    (yacute   . 253)
    (thorn    . 254)
    (yuml     . 255)
    (OElig    . 338)
    (oelig    . 339)
    (Scaron   . 352)
    (scaron   . 353)
    (Yuml     . 376)
    (fnof     . 402)
    (circ     . 710)
    (tilde    . 732)
    (Alpha    . 913)
    (Beta     . 914)
    (Gamma    . 915)
    (Delta    . 916)
    (Epsilon  . 917)
    (Zeta     . 918)
    (Eta      . 919)
    (Theta    . 920)
    (Iota     . 921)
    (Kappa    . 922)
    (Lambda   . 923)
    (Mu       . 924)
    (Nu       . 925)
    (Xi       . 926)
    (Omicron  . 927)
    (Pi       . 928)
    (Rho      . 929)
    (Sigma    . 931)
    (Tau      . 932)
    (Upsilon  . 933)
    (Phi      . 934)
    (Chi      . 935)
    (Psi      . 936)
    (Omega    . 937)
    (alpha    . 945)
    (beta     . 946)
    (gamma    . 947)
    (delta    . 948)
    (epsilon  . 949)
    (zeta     . 950)
    (eta      . 951)
    (theta    . 952)
    (iota     . 953)
    (kappa    . 954)
    (lambda   . 955)
    (mu       . 956)
    (nu       . 957)
    (xi       . 958)
    (omicron  . 959)
    (pi       . 960)
    (rho      . 961)
    (sigmaf   . 962)
    (sigma    . 963)
    (tau      . 964)
    (upsilon  . 965)
    (phi      . 966)
    (chi      . 967)
    (psi      . 968)
    (omega    . 969)
    (thetasym . 977)
    (upsih    . 978)
    (piv      . 982)
    (ensp     . 8194)
    (emsp     . 8195)
    (thinsp   . 8201)
    (zwnj     . 8204)
    (zwj      . 8205)
    (lrm      . 8206)
    (rlm      . 8207)
    (ndash    . 8211)
    (mdash    . 8212)
    (lsquo    . 8216)
    (rsquo    . 8217)
    (sbquo    . 8218)
    (ldquo    . 8220)
    (rdquo    . 8221)
    (bdquo    . 8222)
    (dagger   . 8224)
    (Dagger   . 8225)
    (bull     . 8226)
    (hellip   . 8230)
    (permil   . 8240)
    (prime    . 8242)
    (Prime    . 8243)
    (lsaquo   . 8249)
    (rsaquo   . 8250)
    (oline    . 8254)
    (frasl    . 8260)
    (euro     . 8364)
    (image    . 8465)
    (weierp   . 8472)
    (real     . 8476)
    (trade    . 8482)
    (alefsym  . 8501)
    (larr     . 8592)
    (uarr     . 8593)
    (rarr     . 8594)
    (darr     . 8595)
    (harr     . 8596)
    (crarr    . 8629)
    (lArr     . 8656)
    (uArr     . 8657)
    (rArr     . 8658)
    (dArr     . 8659)
    (hArr     . 8660)
    (forall   . 8704)
    (part     . 8706)
    (exist    . 8707)
    (empty    . 8709)
    (nabla    . 8711)
    (isin     . 8712)
    (notin    . 8713)
    (ni       . 8715)
    (prod     . 8719)
    (sum      . 8721)
    (minus    . 8722)
    (lowast   . 8727)
    (radic    . 8730)
    (prop     . 8733)
    (infin    . 8734)
    (ang      . 8736)
    (and      . 8743)
    (or       . 8744)
    (cap      . 8745)
    (cup      . 8746)
    (int      . 8747)
    (there4   . 8756)
    (sim      . 8764)
    (cong     . 8773)
    (asymp    . 8776)
    (ne       . 8800)
    (equiv    . 8801)
    (le       . 8804)
    (ge       . 8805)
    (sub      . 8834)
    (sup      . 8835)
    (nsub     . 8836)
    (sube     . 8838)
    (supe     . 8839)
    (oplus    . 8853)
    (otimes   . 8855)
    (perp     . 8869)
    (sdot     . 8901)
    (lceil    . 8968)
    (rceil    . 8969)
    (lfloor   . 8970)
    (rfloor   . 8971)
    (lang     . 9001)
    (rang     . 9002)
    (loz      . 9674)
    (spades   . 9824)
    (clubs    . 9827)
    (hearts   . 9829)
    (diams    . 9830)))

(defun racket--html-char-entity-symbol->string (sym)
  "HTML entity symbols to strings.
From <https://github.com/GNOME/libxml2/blob/master/HTMLparser.c>."
  (if-let (ch (cdr (assq sym racket--html-char-entities)))
      (string ch)
    (format "&%s;" sym)))

(provide 'racket-scribble)

;; racket-scribble.el ends here
