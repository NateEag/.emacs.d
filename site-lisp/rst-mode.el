;;; rst.el --- Mode for viewing and editing reStructuredText-documents.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   Free Software Foundation, Inc.

;; Maintainer: Stefan Merten <smerten@oekonux.de>
;; Author: Martin Blais <blais@furius.ca>,
;;         David Goodger <goodger@python.org>,
;;         Wei-Wei Guo <wwguocn@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides major mode rst-mode, which supports documents marked
;; up using the reStructuredText format. Support includes font locking as well
;; as a lot of convenience functions for editing. It does this by defining a
;; Emacs major mode: rst-mode (ReST). This mode is derived from text-mode. This
;; package also contains:
;;
;; - Functions to automatically adjust and cycle the section underline
;;   adornments;
;; - A mode that displays the table of contents and allows you to jump anywhere
;;   from it;
;; - Functions to insert and automatically update a TOC in your source
;;   document;
;; - Function to insert list, processing item bullets and enumerations
;;   automatically;
;; - Font-lock highlighting of most reStructuredText structures;
;; - Indentation and filling according to reStructuredText syntax;
;; - Cursor movement according to reStructuredText syntax;
;; - Some other convenience functions.
;;
;; See the accompanying document in the docutils documentation about
;; the contents of this package and how to use it.
;;
;; For more information about reStructuredText, see
;; http://docutils.sourceforge.net/rst.html
;;
;; For full details on how to use the contents of this file, see
;; http://docutils.sourceforge.net/docs/user/emacs.html
;;
;;
;; There are a number of convenient keybindings provided by rst-mode.
;; For more on bindings, see rst-mode-map below.  There are also many variables
;; that can be customized, look for defcustom in this file.
;;
;; If you use the table-of-contents feature, you may want to add a hook to
;; update the TOC automatically everytime you adjust a section title::
;;
;;   (add-hook 'rst-adjust-hook 'rst-toc-update)
;;
;; Syntax highlighting: font-lock is enabled by default.  If you want to turn
;; off syntax highlighting to rst-mode, you can use the following::
;;
;;   (setq font-lock-global-modes '(not rst-mode ...))
;;
;;
;;
;; Customization is done by customizable variables contained in customization
;; group "rst" and subgroups. Group "rst" is contained in the "wp" group.
;;

;;; DOWNLOAD

;; The latest release of this file lies in the docutils source code repository:
;;   http://svn.berlios.de/svnroot/repos/docutils/trunk/docutils/tools/editors/emacs/rst.el

;;; INSTALLATION

;; Add the following lines to your `.emacs' file:
;;
;;   (require 'rst)
;;
;; If you are using `.txt' as a standard extension for reST files as
;; http://docutils.sourceforge.net/FAQ.html#what-s-the-standard-filename-extension-for-a-restructuredtext-file
;; suggests you may use one of the `Local Variables in Files' mechanism Emacs
;; provides to set the major mode automatically.  For instance you may use::
;;
;;    .. -*- mode: rst -*-
;;
;; in the very first line of your file.  The following code is useful if you
;; want automatically enter rst-mode from any file with compatible extensions:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.txt$" . rst-mode)
;;                 ("\\.rst$" . rst-mode)
;;                 ("\\.rest$" . rst-mode)) auto-mode-alist))
;;

;;; Code:

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versions

(defun rst-extract-version (delim-re head-re re tail-re var &optional default)
  "Return the version matching RE after regex DELIM-RE and HEAD-RE
and before TAIL-RE and DELIM-RE in VAR or DEFAULT for no match"
  (if (string-match
       (concat delim-re head-re "\\(" re "\\)" tail-re delim-re)
       var)
      (match-string 1 var)
    default))

;; Use CVSHeader to really get information from CVS and not other version
;; control systems
(defconst rst-cvs-header
  "$CVSHeader: sm/rst_el/rst.el,v 1.233 2011-03-20 17:20:28 stefan Exp $")
(defconst rst-cvs-rev
  (rst-extract-version "\\$" "CVSHeader: \\S + " "[0-9]+\\(?:\\.[0-9]+\\)+"
		       " .*" rst-cvs-header "0.0")
  "The CVS revision of this file. CVS revision is the development revision.")
(defconst rst-cvs-timestamp
  (rst-extract-version "\\$" "CVSHeader: \\S + \\S + "
		       "[0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+" " .*"
		       rst-cvs-header "1970-01-01 00:00:00")
  "The CVS timestamp of this file.")

;; Use LastChanged... to really get information from SVN
(defconst rst-svn-rev
  (rst-extract-version "\\$" "LastChangedRevision: " "[0-9]+" " "
		       "$LastChangedRevision: 6993 $")
  "The SVN revision of this file.
SVN revision is the upstream (docutils) revision.")
(defconst rst-svn-timestamp
  (rst-extract-version "\\$" "LastChangedDate: " ".+?+" " "
		       "$LastChangedDate: 2011-03-20 18:20:36 +0100 (Son, 20 Mär 2011) $")
  "The SVN timestamp of this file.")

;; Maintained by the release process
(defconst rst-official-version
  (rst-extract-version "%" "OfficialVersion: " "[0-9]+\\(?:\\.[0-9]+\\)+" " "
		       "%OfficialVersion: 1.1.0 %")
  "Official version of the package.")
(defconst rst-official-cvs-rev
  (rst-extract-version "[%$]" "Revision: " "[0-9]+\\(?:\\.[0-9]+\\)+" " "
		       "%Revision: 1.233 %")
  "CVS revision of this file in the official version.")

(defconst rst-version
  (if (equal rst-official-cvs-rev rst-cvs-rev)
      rst-official-version
    (format "%s (development %s [%s])" rst-official-version
	    rst-cvs-rev rst-cvs-timestamp))
  "The version string.
Starts with the current official version. For developer versions
in parentheses follows the development revision and the timestamp.")

(defconst rst-package-emacs-version-alist
  '(("1.0.0" . "24.0")
    ("1.1.0" . "24.0")))

(unless (assoc rst-official-version rst-package-emacs-version-alist)
  (error "Version %s not listed in `rst-package-emacs-version-alist'"
	 rst-version))

(add-to-list 'customize-package-emacs-version-alist
	     (cons 'ReST rst-package-emacs-version-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize customization


(defgroup rst nil "Support for reStructuredText documents."
  :group 'wp
  :version "23.1"
  :link '(url-link "http://docutils.sourceforge.net/rst.html"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facilities for regular expressions used everywhere

;; The trailing numbers in the names give the number of referenceable regex
;; groups contained in the regex

;; Used to be customizable but really is not customizable but fixed by the reST
;; syntax
(defconst rst-bullets
  ;; Sorted so they can form a character class when concatenated
  '(?- ?* ?+ ?\u2022 ?\u2023 ?\u2043)
  "List of all possible bullet characters for bulleted lists.")

(defconst rst-uri-schemes
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https" "imap"
    "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero" "rtsp"
    "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Supported URI schemes.")

(defconst rst-adornment-chars
  ;; Sorted so they can form a character class when concatenated
  '(?\]
    ?! ?\" ?# ?$ ?% ?& ?' ?\( ?\) ?* ?+ ?, ?. ?/ ?: ?\; ?< ?= ?> ?? ?@ ?\[ ?\\
    ?^ ?_ ?` ?{ ?| ?} ?~
    ?-)
  "Characters which may be used in adornments for sections and transitions.")

(defconst rst-max-inline-length
  1000
  "Maximum length of inline markup to recognize.")

(defconst rst-re-alist-def
  ;; `*-beg' matches * at the beginning of a line
  ;; `*-end' matches * at the end of a line
  ;; `*-prt' matches a part of *
  ;; `*-tag' matches *
  ;; `*-sta' matches the start of * which may be followed by respective content
  ;; `*-pfx' matches the delimiter left of *
  ;; `*-sfx' matches the delimiter right of *
  ;; `*-hlp' helper for *
  ;;
  ;; A trailing number says how many referenceable groups are contained.
  `(

    ;; Horizontal white space (`hws')
    (hws-prt "[\t ]")
    (hws-tag hws-prt "*") ; Optional sequence of horizontal white space
    (hws-sta hws-prt "+") ; Mandatory sequence of horizontal white space

    ;; Lines (`lin')
    (lin-beg "^" hws-tag) ; Beginning of a possibly indented line
    (lin-end hws-tag "$") ; End of a line with optional trailing white space
    (linemp-tag "^" hws-tag "$") ; Empty line with optional white space

    ;; Various tags and parts
    (ell-tag "\\.\\.\\.") ; Ellipsis
    (bul-tag ,(concat "[" rst-bullets "]")) ; A bullet
    (ltr-tag "[a-zA-Z]") ; A letter enumerator tag
    (num-prt "[0-9]") ; A number enumerator part
    (num-tag num-prt "+") ; A number enumerator tag
    (rom-prt "[IVXLCDMivxlcdm]") ; A roman enumerator part
    (rom-tag rom-prt "+") ; A roman enumerator tag
    (aut-tag "#") ; An automatic enumerator tag
    (dcl-tag "::") ; Double colon

    ;; Block lead in (`bli')
    (bli-sfx (:alt hws-sta "$")) ; Suffix of a block lead-in with *optional*
				 ; immediate content

    ;; Various starts
    (bul-sta bul-tag bli-sfx) ; Start of a bulleted item

    ;; Explicit markup tag (`exm')
    (exm-tag "\\.\\.")
    (exm-sta exm-tag hws-sta)
    (exm-beg lin-beg exm-sta)

    ;; Counters in enumerations (`cnt')
    (cntany-tag (:alt ltr-tag num-tag rom-tag aut-tag)) ; An arbitrary counter
    (cntexp-tag (:alt ltr-tag num-tag rom-tag)) ; An arbitrary explicit counter

    ;; Enumerator (`enm')
    (enmany-tag (:alt
		 (:seq cntany-tag "\\.")
		 (:seq "(?" cntany-tag ")"))) ; An arbitrary enumerator
    (enmexp-tag (:alt
		 (:seq cntexp-tag "\\.")
		 (:seq "(?" cntexp-tag ")"))) ; An arbitrary explicit
					      ; enumerator
    (enmaut-tag (:alt
		 (:seq aut-tag "\\.")
		 (:seq "(?" aut-tag ")"))) ; An automatic enumerator
    (enmany-sta enmany-tag bli-sfx) ; An arbitrary enumerator start
    (enmexp-sta enmexp-tag bli-sfx) ; An arbitrary explicit enumerator start
    (enmexp-beg lin-beg enmexp-sta) ; An arbitrary explicit enumerator start
				    ; at the beginning of a line

    ;; Items may be enumerated or bulleted (`itm')
    (itmany-tag (:alt enmany-tag bul-tag)) ; An arbitrary item tag
    (itmany-sta-1 (:grp itmany-tag) bli-sfx) ; An arbitrary item start, group
					     ; is the item tag
    (itmany-beg-1 lin-beg itmany-sta-1) ; An arbitrary item start at the
				        ; beginning of a line, group is the
				        ; item tag

    ;; Inline markup (`ilm')
    (ilm-pfx (:alt "^" hws-prt "[-'\"([{<\u2018\u201c\u00ab\u2019/:]"))
    (ilm-sfx (:alt "$" hws-prt "[]-'\")}>\u2019\u201d\u00bb/:.,;!?\\]"))

    ;; Inline markup content (`ilc')
    (ilcsgl-tag "\\S ") ; A single non-white character
    (ilcast-prt (:alt "[^*\\]" "\\\\.")) ; Part of non-asterisk content
    (ilcbkq-prt (:alt "[^`\\]" "\\\\.")) ; Part of non-backquote content
    (ilcbkqdef-prt (:alt "[^`\\\n]" "\\\\.")) ; Part of non-backquote
					      ; definition
    (ilcbar-prt (:alt "[^|\\]" "\\\\.")) ; Part of non-vertical-bar content
    (ilcbardef-prt (:alt "[^|\\\n]" "\\\\.")) ; Part of non-vertical-bar
					      ; definition
    (ilcast-sfx "[^\t *\\]") ; Suffix of non-asterisk content
    (ilcbkq-sfx "[^\t `\\]") ; Suffix of non-backquote content
    (ilcbar-sfx "[^\t |\\]") ; Suffix of non-vertical-bar content
    (ilcrep-hlp ,(format "\\{0,%d\\}" rst-max-inline-length)) ; Repeat count
    (ilcast-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcast-prt ilcrep-hlp
			    ilcast-sfx))) ; Non-asterisk content
    (ilcbkq-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcbkq-prt ilcrep-hlp
			    ilcbkq-sfx))) ; Non-backquote content
    (ilcbkqdef-tag (:alt ilcsgl-tag
			 (:seq ilcsgl-tag
			       ilcbkqdef-prt ilcrep-hlp
			       ilcbkq-sfx))) ; Non-backquote definition
    (ilcbar-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcbar-prt ilcrep-hlp
			    ilcbar-sfx))) ; Non-vertical-bar content
    (ilcbardef-tag (:alt ilcsgl-tag
			 (:seq ilcsgl-tag
			       ilcbardef-prt ilcrep-hlp
			       ilcbar-sfx))) ; Non-vertical-bar definition

    ;; Fields (`fld')
    (fldnam-prt (:alt "[^:\n]" "\\\\:")) ; Part of a field name
    (fldnam-tag fldnam-prt "+") ; A field name
    (fld-tag ":" fldnam-tag ":") ; A field marker

    ;; Options (`opt')
    (optsta-tag (:alt "[-+/]" "--")) ; Start of an option
    (optnam-tag "\\sw" (:alt "-" "\\sw") "*") ; Name of an option
    (optarg-tag (:shy "[ =]\\S +")) ; Option argument
    (optsep-tag (:shy "," hws-prt)) ; Separator between options
    (opt-tag (:shy optsta-tag optnam-tag optarg-tag "?")) ; A complete option

    ;; Footnotes and citations (`fnc')
    (fncnam-prt "[^\]\n]") ; Part of a footnote or citation name
    (fncnam-tag fncnam-prt "+") ; A footnote or citation name
    (fnc-tag "\\[" fncnam-tag "]") ; A complete footnote or citation tag
    (fncdef-tag-2 (:grp exm-sta)
		  (:grp fnc-tag)) ; A complete footnote or citation definition
				  ; tag; first group is the explicit markup
				  ; start, second group is the footnote /
				  ; citation tag
    (fnc-sta-2 fncdef-tag-2 bli-sfx) ; Start of a footnote or citation
				     ; definition; first group is the explicit
				     ; markup start, second group is the
				     ; footnote / citation tag

    ;; Substitutions (`sub')
    (sub-tag "|" ilcbar-tag "|") ; A complete substitution tag
    (subdef-tag "|" ilcbardef-tag "|") ; A complete substitution definition
				       ; tag

    ;; Symbol (`sym')
    (sym-prt (:alt "\\sw" "\\s_"))
    (sym-tag sym-prt "+")

    ;; URIs (`uri')
    (uri-tag (:alt ,@rst-uri-schemes))

    ;; Adornment (`ado')
    (ado-prt "[" ,(concat rst-adornment-chars) "]")
    (adorep3-hlp "\\{3,\\}") ; There must be at least 3 characters because
			     ; otherwise explicit markup start would be
			     ; recognized
    (adorep2-hlp "\\{2,\\}") ; As `adorep3-hlp' but when the first of three
			     ; characters is matched differently
    (ado-tag-1-1 (:grp ado-prt)
		 "\\1" adorep2-hlp) ; A complete adornment, group is the first
				    ; adornment character and MUST be the FIRST
				    ; group in the whole expression
    (ado-tag-1-2 (:grp ado-prt)
		 "\\2" adorep2-hlp) ; A complete adornment, group is the first
				    ; adornment character and MUST be the
				    ; SECOND group in the whole expression
    (ado-beg-2-1 "^" (:grp ado-tag-1-2)
		 lin-end) ; A complete adornment line; first group is the whole
			  ; adornment and MUST be the FIRST group in the whole
			  ; expression; second group is the first adornment
			  ; character

    ;; Titles (`ttl')
    (ttl-tag "\\S *\\w\\S *") ; A title text
    (ttl-beg lin-beg ttl-tag) ; A title text at the beginning of a line

    ;; Directives and substitution definitions (`dir')
    (dir-tag-3 (:grp exm-sta)
	       (:grp (:shy subdef-tag hws-sta) "?")
	       (:grp sym-tag dcl-tag)) ; A directive or substitution definition
				       ; tag; first group is explicit markup
				       ; start, second group is a possibly
				       ; empty substitution tag, third group is
				       ; the directive tag including the double
				       ; colon
    (dir-sta-3 dir-tag-3 bli-sfx) ; Start of a directive or substitution
				  ; definition; groups are as in dir-tag-3

    ;; Literal block (`lit')
    (lit-sta-2 (:grp (:alt "[^.\n]" "\\.[^.\n]") ".*") "?"
	       (:grp dcl-tag) "$") ; Start of a literal block; first group is
				   ; any text before the double colon tag which
				   ; may not exist, second group is the double
				   ; colon tag

    ;; Comments (`cmt')
    (cmt-sta-1 (:grp exm-sta) "[^\[|_\n]"
	       (:alt "[^:\n]" (:seq ":" (:alt "[^:\n]" "$")))
	       "*$") ; Start of a comment block; first group is explicit markup
		     ; start

    ;; Paragraphs (`par')
    (par-tag- (:alt itmany-tag fld-tag opt-tag fncdef-tag-2 dir-tag-3 exm-tag)
	      ) ; Tag at the beginning of a paragraph; there may be groups in
		; certain cases
    )
  "Definition alist of relevant regexes.
Each entry consists of the symbol naming the regex and an
argument list for `rst-re'.")

(defun rst-re (&rest args)
  "Interpret ARGS as regular expressions and return a regex string.
Each element of ARGS may be one of the following:

A string which is inserted unchanged.

A character which is resolved to a quoted regex.

A symbol which is resolved to a string using `rst-re-alist-def'.

A list with a keyword in the car. Each element of the cdr of such
a list is recursively interpreted as ARGS. The results of this
interpretation are concatenated according to the keyword.

For the keyword `:seq' the results are simply concatenated.

For the keyword `:shy' the results are concatenated and
surrounded by a shy-group (\"\\(?:...\\)\").

For the keyword `:alt' the results form an alternative (\"\\|\")
which is shy-grouped (\"\\(?:...\\)\").

For the keyword `:grp' the results are concatenated and form a
referencable grouped (\"\\(...\\)\").

After interpretation of ARGS the results are concatenated as for
`:seq'.
"
  (apply 'concat
	 (mapcar
	  (lambda (re)
	    (cond
	     ((stringp re)
	      re)
	     ((symbolp re)
	      (cadr (assoc re rst-re-alist)))
	     ((characterp re)
	      (regexp-quote (char-to-string re)))
	     ((listp re)
	      (let ((nested
		     (mapcar (lambda (elt)
			       (rst-re elt))
			     (cdr re))))
		(cond
		 ((eq (car re) :seq)
		  (mapconcat 'identity nested ""))
		 ((eq (car re) :shy)
		  (concat "\\(?:" (mapconcat 'identity nested "") "\\)"))
		 ((eq (car re) :grp)
		  (concat "\\(" (mapconcat 'identity nested "") "\\)"))
		 ((eq (car re) :alt)
		  (concat "\\(?:" (mapconcat 'identity nested "\\|") "\\)"))
		 (t
		  (error "Unknown list car: %s" (car re))))))
	     (t
	      (error "Unknown object type for building regex: %s" re))))
	  args)))

(defconst rst-re-alist
  ;; Shadow global value we are just defining so we can construct it step by
  ;; step
  (let (rst-re-alist)
    (dolist (re rst-re-alist-def)
      (setq rst-re-alist
	    (nconc rst-re-alist
		   (list (list (car re) (apply 'rst-re (cdr re)))))))
    rst-re-alist)
  "Alist mapping symbols from `rst-re-alist-def' to regex strings.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition.

(defvar rst-deprecated-keys nil
  "Alist mapping deprecated keys to the new key to use and the definition.")

(require 'edmacro)

(defun rst-call-deprecated ()
  (interactive)
  (let* ((dep-key (this-command-keys-vector))
	 (dep-key-s (format-kbd-macro dep-key))
	 (fnd (assoc dep-key rst-deprecated-keys)))
    (if (not fnd)
	;; Exact key sequence not found. Maybe a deprecated key sequence has
	;; been followed by another key.
	(let* ((dep-key-pfx (butlast (append dep-key nil) 1))
	       (dep-key-def (vconcat dep-key-pfx '(t)))
	       (fnd-def (assoc dep-key-def rst-deprecated-keys)))
	  (if (not fnd-def)
	      (error "Unknown deprecated key sequence %s" dep-key-s)
	    ;; Don't execute the command in this case
	    (message "[Deprecated use of key %s; use key %s instead]"
		     (format-kbd-macro dep-key-pfx)
		     (format-kbd-macro (second fnd-def)))))
      (message "[Deprecated use of key %s; use key %s instead]"
	       dep-key-s (format-kbd-macro (second fnd)))
      (call-interactively (third fnd)))))

(defun rst-define-key (keymap key def &rest deprecated)
  "Bind like `define-key' using DEPRECATED as deprecated key definitions.
DEPRECATED key definitions should be in vector notation. These
are defined as well but give an additional message."
  (define-key keymap key def)
  (dolist (dep-key deprecated)
    (push (list dep-key key def) rst-deprecated-keys)
    (define-key keymap dep-key 'rst-call-deprecated)))

;; Key bindings.
(defvar rst-mode-map
  (let ((map (make-sparse-keymap)))

    ;; \C-c is the general keymap
    (rst-define-key map [?\C-c ?\C-h] 'describe-prefix-bindings)

    ;;
    ;; Section Adornments.
    ;;
    ;; The adjustment function that adorns or rotates a section title.
    (rst-define-key map [?\C-c ?\C-=] 'rst-adjust [?\C-c ?\C-a t])
    (rst-define-key map [?\C-=] 'rst-adjust) ; (Does not work on the Mac OSX.)

    ;; \C-c \C-a is the keymap for adornments
    (rst-define-key map [?\C-c ?\C-a ?\C-h] 'describe-prefix-bindings)
    ;; Display the hierarchy of adornments implied by the current document contents.
    (rst-define-key map [?\C-c ?\C-a ?\C-d] 'rst-display-adornments-hierarchy)
    ;; Homogeneize the adornments in the document.
    (rst-define-key map [?\C-c ?\C-a ?\C-s] 'rst-straighten-adornments
		    [?\C-c ?\C-s])

    ;;
    ;; Section Movement and Selection.
    ;;
    ;; Mark the subsection where the cursor is.
    (rst-define-key map [?\C-\M-h] 'rst-mark-section
		    ;; same as mark-defun sgml-mark-current-element
		    [?\C-c ?\C-m])
    ;; Move forward/backward between section titles.
    (rst-define-key map [?\C-\M-a] 'rst-forward-section
		    ;; same as beginning-of-defun
		    [?\C-c ?\C-n])
    (rst-define-key map [?\C-\M-e] 'rst-backward-section
		    ;; same as end-of-defun
		    [?\C-c ?\C-p])

    ;;
    ;; Operating on regions.
    ;;
    ;; \C-c \C-r is the keymap for regions
    (rst-define-key map [?\C-c ?\C-r ?\C-h] 'describe-prefix-bindings)
    ;; Makes region a line-block.
    (rst-define-key map [?\C-c ?\C-r ?\C-l] 'rst-line-block-region
		    [?\C-c ?\C-d])
    ;; Shift region left or right according to tabs
    (rst-define-key map [?\C-c ?\C-r tab] 'rst-shift-region
		    [?\C-c ?\C-r t] [?\C-c ?\C-l t])

    ;;
    ;; Operating on lists.
    ;;
    ;; \C-c \C-l is the keymap for lists
    (rst-define-key map [?\C-c ?\C-l ?\C-h] 'describe-prefix-bindings)
    ;; Makes paragraphs in region as a bullet list.
    (rst-define-key map [?\C-c ?\C-l ?\C-b] 'rst-bullet-list-region
		    [?\C-c ?\C-b])
    ;; Makes paragraphs in region as a enumeration.
    (rst-define-key map [?\C-c ?\C-l ?\C-e] 'rst-enumerate-region
		    [?\C-c ?\C-e])
    ;; Converts bullets to an enumeration.
    (rst-define-key map [?\C-c ?\C-l ?\C-c] 'rst-convert-bullets-to-enumeration
		    [?\C-c ?\C-v])
    ;; Make sure that all the bullets in the region are consistent.
    (rst-define-key map [?\C-c ?\C-l ?\C-s] 'rst-straighten-bullets-region
		    [?\C-c ?\C-w])
    ;; Insert a list item
    (rst-define-key map [?\C-c ?\C-l ?\C-i] 'rst-insert-list)

    ;;
    ;; Table-of-Contents Features.
    ;;
    ;; \C-c \C-t is the keymap for table of contents
    (rst-define-key map [?\C-c ?\C-t ?\C-h] 'describe-prefix-bindings)
    ;; Enter a TOC buffer to view and move to a specific section.
    (rst-define-key map [?\C-c ?\C-t ?\C-t] 'rst-toc)
    ;; Insert a TOC here.
    (rst-define-key map [?\C-c ?\C-t ?\C-i] 'rst-toc-insert
		    [?\C-c ?\C-i])
    ;; Update the document's TOC (without changing the cursor position).
    (rst-define-key map [?\C-c ?\C-t ?\C-u] 'rst-toc-update
		    [?\C-c ?\C-u])
    ;; Got to the section under the cursor (cursor must be in TOC).
    (rst-define-key map [?\C-c ?\C-t ?\C-j] 'rst-goto-section
		    [?\C-c ?\C-f])

    ;;
    ;; Converting Documents from Emacs.
    ;;
    ;; \C-c \C-c is the keymap for compilation
    (rst-define-key map [?\C-c ?\C-c ?\C-h] 'describe-prefix-bindings)
    ;; Run one of two pre-configured toolset commands on the document.
    (rst-define-key map [?\C-c ?\C-c ?\C-c] 'rst-compile
		    [?\C-c ?1])
    (rst-define-key map [?\C-c ?\C-c ?\C-a] 'rst-compile-alt-toolset
		    [?\C-c ?2])
    ;; Convert the active region to pseudo-xml using the docutils tools.
    (rst-define-key map [?\C-c ?\C-c ?\C-x] 'rst-compile-pseudo-region
		    [?\C-c ?3])
    ;; Convert the current document to PDF and launch a viewer on the results.
    (rst-define-key map [?\C-c ?\C-c ?\C-p] 'rst-compile-pdf-preview
		    [?\C-c ?4])
    ;; Convert the current document to S5 slides and view in a web browser.
    (rst-define-key map [?\C-c ?\C-c ?\C-s] 'rst-compile-slides-preview
		    [?\C-c ?5])

    map)
  "Keymap for reStructuredText mode commands.
This inherits from Text mode.")


;; Abbrevs.
(defvar rst-mode-abbrev-table nil
  "Abbrev table used while in `rst-mode'.")
(define-abbrev-table 'rst-mode-abbrev-table
  (mapcar (lambda (x) (append x '(nil 0 system)))
          '(("contents" ".. contents::\n..\n   ")
            ("con" ".. contents::\n..\n   ")
            ("cont" "[...]")
            ("skip" "\n\n[...]\n\n  ")
            ("seq" "\n\n[...]\n\n  ")
            ;; FIXME: Add footnotes, links, and more.
            )))


;; Syntax table.
(defvar rst-mode-syntax-table
  (let ((st (copy-syntax-table text-mode-syntax-table)))

    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?' "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "." st)
    (modify-syntax-entry (aref "\u00ab" 0) "." st)
    (modify-syntax-entry (aref "\u00bb" 0) "." st)
    (modify-syntax-entry (aref "\u2018" 0) "." st)
    (modify-syntax-entry (aref "\u2019" 0) "." st)
    (modify-syntax-entry (aref "\u201c" 0) "." st)
    (modify-syntax-entry (aref "\u201d" 0) "." st)

    st)
  "Syntax table used while in `rst-mode'.")


(defcustom rst-mode-hook nil
  "Hook run when `rst-mode' is turned on.
The hook for `text-mode' is run before this one."
  :group 'rst
  :type '(hook))


;; Use rst-mode for *.rst and *.rest files.  Many ReStructured-Text files
;; use *.txt, but this is too generic to be set as a default.
;;;###autoload (add-to-list 'auto-mode-alist (purecopy '("\\.re?st\\'" . rst-mode)))
;;;###autoload
(define-derived-mode rst-mode text-mode "ReST"
  "Major mode for editing reStructuredText documents.
\\<rst-mode-map>

Turning on `rst-mode' calls the normal hooks `text-mode-hook'
and `rst-mode-hook'.  This mode also supports font-lock
highlighting.

\\{rst-mode-map}"
  :abbrev-table rst-mode-abbrev-table
  :syntax-table rst-mode-syntax-table
  :group 'rst

  ;; Paragraph recognition
  (set (make-local-variable 'paragraph-separate)
       (rst-re '(:alt
		 "\f"
		 lin-end)))
  (set (make-local-variable 'paragraph-start)
       (rst-re '(:alt
		 "\f"
		 lin-end
		 (:seq hws-tag par-tag- bli-sfx))))

  ;; Indenting and filling
  (set (make-local-variable 'indent-line-function) 'rst-indent-line)
  (set (make-local-variable 'adaptive-fill-mode) t)
  (set (make-local-variable 'adaptive-fill-regexp)
       (rst-re 'hws-tag 'par-tag- "?" 'hws-tag))
  (set (make-local-variable 'adaptive-fill-function) 'rst-adaptive-fill)
  (set (make-local-variable 'fill-paragraph-handle-comment) nil)

  ;; Comments
  (set (make-local-variable 'comment-start) ".. ")
  (set (make-local-variable 'comment-start-skip)
       (rst-re 'lin-beg 'exm-tag 'bli-sfx))
  (set (make-local-variable 'comment-continue) "   ")
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-use-syntax) nil)
  ;; reStructuredText has not really a comment ender but nil is not really a
  ;; permissible value
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) nil)

  (set (make-local-variable 'comment-line-break-function)
       'rst-comment-line-break)
  (set (make-local-variable 'comment-indent-function)
       'rst-comment-indent)
  (set (make-local-variable 'comment-insert-comment-function)
       'rst-comment-insert-comment)
  (set (make-local-variable 'comment-region-function)
       'rst-comment-region)
  (set (make-local-variable 'uncomment-region-function)
       'rst-uncomment-region)

  ;; Font lock
  (setq font-lock-defaults
	'(rst-font-lock-keywords
	  t nil nil nil
	  (font-lock-multiline . t)
	  (font-lock-mark-block-function . mark-paragraph)
	  ;; rst-mode does not need font-lock-support-mode because it's fast
	  ;; enough. In fact using `jit-lock-mode` slows things down
	  ;; considerably even if `rst-font-lock-extend-region` is in place and
	  ;; compiled.
	  ;;(font-lock-support-mode . nil)
	  ))
  (add-hook 'font-lock-extend-region-functions 'rst-font-lock-extend-region t)

  ;; Text after a changed line may need new fontification
  (set (make-local-variable 'jit-lock-contextually) t))

;;;###autoload
(define-minor-mode rst-minor-mode
  "ReST Minor Mode.
Toggle ReST minor mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When ReST minor mode is enabled, the ReST mode keybindings
are installed on top of the major mode bindings.  Use this
for modes derived from Text mode, like Mail mode."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 " ReST"
 ;; The minor mode bindings.
 rst-mode-map
 :group 'rst)

;; FIXME: can I somehow install these too?
;;  :abbrev-table rst-mode-abbrev-table
;;  :syntax-table rst-mode-syntax-table


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section Adornment Adjustment
;; ============================
;;
;; The following functions implement a smart automatic title sectioning feature.
;; The idea is that with the cursor sitting on a section title, we try to get as
;; much information from context and try to do the best thing automatically.
;; This function can be invoked many times and/or with prefix argument to rotate
;; between the various sectioning adornments.
;;
;; Definitions: the two forms of sectioning define semantically separate section
;; levels.  A sectioning ADORNMENT consists in:
;;
;;   - a CHARACTER
;;
;;   - a STYLE which can be either of 'simple' or 'over-and-under'.
;;
;;   - an INDENT (meaningful for the over-and-under style only) which determines
;;     how many characters and over-and-under style is hanging outside of the
;;     title at the beginning and ending.
;;
;; Here are two examples of adornments (| represents the window border, column
;; 0):
;;
;;                                  |
;; 1. char: '-'   e                 |Some Title
;;    style: simple                 |----------
;;                                  |
;; 2. char: '='                     |==============
;;    style: over-and-under         |  Some Title
;;    indent: 2                     |==============
;;                                  |
;;
;; Some notes:
;;
;; - The underlining character that is used depends on context. The file is
;;   scanned to find other sections and an appropriate character is selected.
;;   If the function is invoked on a section that is complete, the character is
;;   rotated among the existing section adornments.
;;
;;   Note that when rotating the characters, if we come to the end of the
;;   hierarchy of adornments, the variable rst-preferred-adornments is
;;   consulted to propose a new underline adornment, and if continued, we cycle
;;   the adornments all over again.  Set this variable to nil if you want to
;;   limit the underlining character propositions to the existing adornments in
;;   the file.
;;
;; - An underline/overline that is not extended to the column at which it should
;;   be hanging is dubbed INCOMPLETE.  For example::
;;
;;      |Some Title
;;      |-------
;;
;; Examples of default invocation:
;;
;;   |Some Title       --->    |Some Title
;;   |                         |----------
;;
;;   |Some Title       --->    |Some Title
;;   |-----                    |----------
;;
;;   |                         |------------
;;   | Some Title      --->    | Some Title
;;   |                         |------------
;;
;; In over-and-under style, when alternating the style, a variable is
;; available to select how much default indent to use (it can be zero).  Note
;; that if the current section adornment already has an indent, we don't
;; adjust it to the default, we rather use the current indent that is already
;; there for adjustment (unless we cycle, in which case we use the indent
;; that has been found previously).

(defgroup rst-adjust nil
  "Settings for adjustment and cycling of section title adornments."
  :group 'rst
  :version "21.1")

(define-obsolete-variable-alias
  'rst-preferred-decorations 'rst-preferred-adornments "r6506")
(defcustom rst-preferred-adornments '((?= over-and-under 1)
				      (?= simple 0)
				      (?- simple 0)
				      (?~ simple 0)
				      (?+ simple 0)
				      (?` simple 0)
				      (?# simple 0)
				      (?@ simple 0))
  "Preferred hierarchy of section title adornments.

A list consisting of lists of the form (CHARACTER STYLE INDENT).
CHARACTER is the character used. STYLE is one of the symbols
OVER-AND-UNDER or SIMPLE. INDENT is an integer giving the wanted
indentation for STYLE OVER-AND-UNDER. CHARACTER and STYLE are
always used when a section adornment is described. In other
places t instead of a list stands for a transition.

This sequence is consulted to offer a new adornment suggestion
when we rotate the underlines at the end of the existing
hierarchy of characters, or when there is no existing section
title in the file.

Set this to an empty list to use only the adornment found in the
file."
  :group 'rst-adjust
  :type `(repeat
	  (group :tag "Adornment specification"
		 (choice :tag "Adornment character"
			 ,@(mapcar (lambda (char)
				     (list 'const
					   :tag (char-to-string char) char))
				   rst-adornment-chars))
		 (radio :tag "Adornment type"
			(const :tag "Overline and underline" over-and-under)
			(const :tag "Underline only" simple))
		 (integer :tag "Indentation for overline and underline type"
			  :value 0))))

(defcustom rst-default-indent 1
  "Number of characters to indent the section title.

This is used for when toggling adornment styles, when switching
from a simple adornment style to a over-and-under adornment
style."
  :group 'rst-adjust
  :type '(integer))


(defun rst-compare-adornments (ado1 ado2)
  "Compare adornments.
Return true if both ADO1 and ADO2 adornments are equal,
according to restructured text semantics (only the character and
the style are compared, the indentation does not matter)."
  (and (eq (car ado1) (car ado2))
       (eq (cadr ado1) (cadr ado2))))


(defun rst-get-adornment-match (hier ado)
  "Return the index (level) in hierarchy HIER of adornment ADO.
This basically just searches for the item using the appropriate
comparison and returns the index.  Return nil if the item is
not found."
  (let ((cur hier))
    (while (and cur (not (rst-compare-adornments (car cur) ado)))
      (setq cur (cdr cur)))
    cur))


(defun rst-suggest-new-adornment (allados &optional prev)
  "Suggest a new, different adornment from all that have been seen.

ALLADOS is the set of all adornments, including the line numbers.
PREV is the optional previous adornment, in order to suggest a
better match."

  ;; For all the preferred adornments...
  (let* (
         ;; If 'prev' is given, reorder the list to start searching after the
         ;; match.
         (fplist
          (cdr (rst-get-adornment-match rst-preferred-adornments prev)))

         ;; List of candidates to search.
         (curpotential (append fplist rst-preferred-adornments)))
    (while
        ;; For all the adornments...
        (let ((cur allados)
              found)
          (while (and cur (not found))
            (if (rst-compare-adornments (car cur) (car curpotential))
                ;; Found it!
                (setq found (car curpotential))
              (setq cur (cdr cur))))
          found)

      (setq curpotential (cdr curpotential)))

    (copy-sequence (car curpotential))))

(defun rst-delete-entire-line ()
  "Delete the entire current line without using the `kill-ring'."
  (delete-region (line-beginning-position)
                 (line-beginning-position 2)))

(defun rst-update-section (char style &optional indent)
  "Unconditionally update the style of a section adornment.

Do this using the given character CHAR, with STYLE 'simple
or 'over-and-under, and with indent INDENT.  If the STYLE
is 'simple, whitespace before the title is removed (indent
is always assumed to be 0).

If there are existing overline and/or underline from the
existing adornment, they are removed before adding the
requested adornment."
  (let (marker
        len)

      (end-of-line)
      (setq marker (point-marker))

      ;; Fixup whitespace at the beginning and end of the line
      (if (or (null indent) (eq style 'simple))
          (setq indent 0))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert (make-string indent ? ))

      (end-of-line)
      (delete-horizontal-space)

      ;; Set the current column, we're at the end of the title line
      (setq len (+ (current-column) indent))

      ;; Remove previous line if it is an adornment
      (save-excursion
        (forward-line -1)
	(if (and (looking-at (rst-re 'ado-beg-2-1))
		 ;; Avoid removing the underline of a title right above us.
		 (save-excursion (forward-line -1)
				 (not (looking-at (rst-re 'ttl-beg)))))
	    (rst-delete-entire-line)))

      ;; Remove following line if it is an adornment
      (save-excursion
        (forward-line +1)
        (if (looking-at (rst-re 'ado-beg-2-1))
	    (rst-delete-entire-line))
        ;; Add a newline if we're at the end of the buffer, for the subsequence
        ;; inserting of the underline
        (if (= (point) (buffer-end 1))
            (newline 1)))

      ;; Insert overline
      (if (eq style 'over-and-under)
          (save-excursion
            (beginning-of-line)
            (open-line 1)
            (insert (make-string len char))))

      ;; Insert underline
      (forward-line +1)
      (open-line 1)
      (insert (make-string len char))

      (forward-line +1)
      (goto-char marker)
      ))

(defun rst-classify-adornment (adornment end)
  "Classify adornment for section titles and transitions.
ADORNMENT is the complete adornment string as found in the buffer
with optional trailing whitespace. END is the point after the
last character of ADORNMENT.

Return a list. The first entry is t for a transition or a
cons (CHARACTER . STYLE). Check `rst-preferred-adornments' for
the meaning of CHARACTER and STYLE.

The remaining list forms four match groups as returned by
`match-data'. Match group 0 matches the whole construct. Match
group 1 matches the overline adornment if present. Match group 2
matches the section title text or the transition. Match group 3
matches the underline adornment.

Return nil if no syntactically valid adornment is found."
  (save-excursion
    (save-match-data
      (when (string-match (rst-re 'ado-beg-2-1) adornment)
	(goto-char end)
	(let* ((ado-ch (string-to-char (match-string 2 adornment)))
	       (ado-re (rst-re ado-ch 'adorep3-hlp))
	       (end-pnt (point))
	       (beg-pnt (progn
			  (forward-line 0)
			  (point)))
	       (nxt-emp ; Next line inexistant or empty
		(save-excursion
		  (or (not (zerop (forward-line 1)))
		      (looking-at (rst-re 'lin-end)))))
	       (prv-emp ; Previous line inexistant or empty
		(save-excursion
		  (or (not (zerop (forward-line -1)))
		      (looking-at (rst-re 'lin-end)))))
	       (ttl-blw ; Title found below starting here
		(save-excursion
		  (and
		   (zerop (forward-line 1))
		   (looking-at (rst-re 'ttl-beg))
		   (point))))
	       (ttl-abv ; Title found above starting here
		(save-excursion
		  (and
		   (zerop (forward-line -1))
		   (looking-at (rst-re 'ttl-beg))
		   (point))))
	       (und-fnd ; Matching underline found starting here
		(save-excursion
		  (and ttl-blw
		   (zerop (forward-line 2))
		   (looking-at (rst-re ado-re 'lin-end))
		   (point))))
	       (ovr-fnd ; Matching overline found starting here
		(save-excursion
		  (and ttl-abv
		   (zerop (forward-line -2))
		   (looking-at (rst-re ado-re 'lin-end))
		   (point))))
	       key beg-ovr end-ovr beg-txt end-txt beg-und end-und)
	  (cond
	   ((and nxt-emp prv-emp)
	    ;; A transition
	    (setq key t)
	    (setq beg-txt beg-pnt)
	    (setq end-txt end-pnt))
	   ((or und-fnd ovr-fnd)
	    ;; An overline with an underline
	    (setq key (cons ado-ch 'over-and-under))
	    (let (;; Prefer overline match over underline match
		  (und-pnt (if ovr-fnd beg-pnt und-fnd))
		  (ovr-pnt (if ovr-fnd ovr-fnd beg-pnt))
		  (txt-pnt (if ovr-fnd ttl-abv ttl-blw)))
	      (goto-char ovr-pnt)
	      (setq beg-ovr (point))
	      (setq end-ovr (line-end-position))
	      (goto-char txt-pnt)
	      (setq beg-txt (point))
	      (setq end-txt (line-end-position))
	      (goto-char und-pnt)
	      (setq beg-und (point))
	      (setq end-und (line-end-position))))
	   (ttl-abv
	    ;; An underline
	    (setq key (cons ado-ch 'simple))
	    (setq beg-und beg-pnt)
	    (setq end-und end-pnt)
	    (goto-char ttl-abv)
	    (setq beg-txt (point))
	    (setq end-txt (line-end-position)))
	   (t
	    ;; Invalid adornment
	    (setq key nil)))
	  (if key
	      (list key
		    (or beg-ovr beg-txt beg-und)
		    (or end-und end-txt end-ovr)
		    beg-ovr end-ovr beg-txt end-txt beg-und end-und)))))))

(defun rst-find-title-line ()
  "Find a section title line around point and return its characteristics.
If the point is on an adornment line find the respective title
line. If the point is on an empty line check previous or next
line whether it is a suitable title line and use it if so. If
point is on a suitable title line use it.

If no title line is found return nil.

Otherwise return as `rst-classify-adornment' does. However, if
the title line has no syntactically valid adornment STYLE is nil
in the first element. If there is no adornment around the title
CHARACTER is also nil and match groups for overline and underline
are nil."
  (save-excursion
    (forward-line 0)
    (let ((orig-pnt (point))
	  (orig-end (line-end-position)))
      (cond
       ((looking-at (rst-re 'ado-beg-2-1))
	(let ((char (string-to-char (match-string-no-properties 2)))
	      (r (rst-classify-adornment (match-string-no-properties 0)
					 (match-end 0))))
	  (cond
	   ((not r)
	    ;; Invalid adornment - check whether this is an incomplete overline
	    (if (and
		 (zerop (forward-line 1))
		 (looking-at (rst-re 'ttl-beg)))
		(list (cons char nil) orig-pnt (line-end-position)
		      orig-pnt orig-end (point) (line-end-position) nil nil)))
	   ((consp (car r))
	    ;; A section title - not a transition
	    r))))
       ((looking-at (rst-re 'lin-end))
	(or
	 (save-excursion
	   (if (and (zerop (forward-line -1))
		    (looking-at (rst-re 'ttl-beg)))
	       (list (cons nil nil) (point) (line-end-position)
		     nil nil (point) (line-end-position) nil nil)))
	 (save-excursion
	   (if (and (zerop (forward-line 1))
		    (looking-at (rst-re 'ttl-beg)))
	       (list (cons nil nil) (point) (line-end-position)
		     nil nil (point) (line-end-position) nil nil)))))
       ((looking-at (rst-re 'ttl-beg))
	;; Try to use the underline
	(let ((r (rst-classify-adornment
		  (buffer-substring-no-properties
		   (line-beginning-position 2) (line-end-position 2))
		  (line-end-position 2))))
	  (if r
	      r
	    ;; No valid adornment found
	    (list (cons nil nil) (point) (line-end-position)
		  nil nil (point) (line-end-position) nil nil))))))))

;; The following function and variables are used to maintain information about
;; current section adornment in a buffer local cache. Thus they can be used for
;; font-locking and manipulation commands.

(defun rst-reset-section-caches ()
  "Reset all section cache variables.
Should be called by interactive functions which deal with sections."
  (setq rst-all-sections nil)
  (setq rst-section-hierarchy nil))

(defvar rst-all-sections nil
  "All section adornments in the buffer as found by `rst-find-all-adornments'.
t when no section adornments were found.")
(make-variable-buffer-local 'rst-all-sections)

;; FIXME: If this variable is set to a different value font-locking of section
;; headers is wrong
(defvar rst-section-hierarchy nil
  "Section hierarchy in the buffer as determined by `rst-get-hierarchy'.
t when no section adornments were found. Value depends on
`rst-all-sections'.")
(make-variable-buffer-local 'rst-section-hierarchy)

(defun rst-find-all-adornments ()
  "Return all the section adornments in the current buffer.
Return a list of (LINE . ADORNMENT) with ascending LINE where
LINE is the line containing the section title. ADORNMENT consists
of a (CHARACTER STYLE INDENT) triple as described for
`rst-preferred-adornments'.

Uses and sets `rst-all-sections'."
  (unless rst-all-sections
    (let (positions)
      ;; Iterate over all the section titles/adornments in the file.
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward (rst-re 'ado-beg-2-1) nil t)
	  (let ((ado-data (rst-classify-adornment
			   (match-string-no-properties 0) (point))))
	    (when (and ado-data
		       (consp (car ado-data))) ; Ignore transitions
	      (set-match-data (cdr ado-data))
	      (goto-char (match-beginning 2)) ; Goto the title start
	      (push (cons (1+ (count-lines (point-min) (point)))
			  (list (caar ado-data)
				(cdar ado-data)
				(current-indentation)))
		    positions)
	      (goto-char (match-end 0))))) ; Go beyond the whole thing
	(setq positions (nreverse positions))
	(setq rst-all-sections (or positions t)))))
  (if (eq rst-all-sections t)
      nil
    rst-all-sections))

(defun rst-infer-hierarchy (adornments)
  "Build a hierarchy of adornments using the list of given ADORNMENTS.

ADORNMENTS is a list of (CHARACTER STYLE INDENT) adornment
specifications, in order that they appear in a file, and will
infer a hierarchy of section levels by removing adornments that
have already been seen in a forward traversal of the adornments,
comparing just CHARACTER and STYLE.

Similarly returns a list of (CHARACTER STYLE INDENT), where each
list element should be unique."
  (let (hierarchy-alist)
    (dolist (x adornments)
      (let ((char (car x))
            (style (cadr x)))
        (unless (assoc (cons char style) hierarchy-alist)
	  (push (cons (cons char style) x) hierarchy-alist))))
    (mapcar 'cdr (nreverse hierarchy-alist))))

(defun rst-get-hierarchy (&optional ignore)
  "Return the hierarchy of section titles in the file.

Return a list of adornments that represents the hierarchy of
section titles in the file. Each element consists of (CHARACTER
STYLE INDENT) as described for `rst-find-all-adornments'. If the
line number in IGNORE is specified, a possibly adornment found on
that line is not taken into account when building the hierarchy.

Uses and sets `rst-section-hierarchy' unless IGNORE is given."
  (if (and (not ignore) rst-section-hierarchy)
      (if (eq rst-section-hierarchy t)
	  nil
	rst-section-hierarchy)
    (let ((all (rst-find-all-adornments))
	  r)
      (setq all (assq-delete-all ignore all))
      (setq r (rst-infer-hierarchy (mapcar 'cdr all)))
      (setq rst-section-hierarchy
	    (if ignore
		;; Clear cache reflecting that a possible update is not
		;; reflected
		nil
	      (or r t)))
      r)))

(defun rst-get-adornments-around ()
  "Return the adornments around point.
Return a list of the previous and next adornments."
  (let* ((all (rst-find-all-adornments))
         (curline (line-number-at-pos))
         prev next
         (cur all))

    ;; Search for the adornments around the current line.
    (while (and cur (< (caar cur) curline))
      (setq prev cur
            cur (cdr cur)))
    ;; 'cur' is the following adornment.

    (if (and cur (caar cur))
        (setq next (if (= curline (caar cur)) (cdr cur) cur)))

    (mapcar 'cdar (list prev next))
    ))


(defun rst-adornment-complete-p (ado)
  "Return true if the adornment ADO around point is complete."
  ;; Note: we assume that the detection of the overline as being the underline
  ;; of a preceding title has already been detected, and has been eliminated
  ;; from the adornment that is given to us.

  ;; There is some sectioning already present, so check if the current
  ;; sectioning is complete and correct.
  (let* ((char (car ado))
         (style (cadr ado))
         (indent (caddr ado))
         (endcol (save-excursion (end-of-line) (current-column)))
         )
    (if char
        (let ((exps (rst-re "^" char (format "\\{%d\\}" (+ endcol indent)) "$")))
          (and
           (save-excursion (forward-line +1)
                           (beginning-of-line)
                           (looking-at exps))
           (or (not (eq style 'over-and-under))
               (save-excursion (forward-line -1)
                               (beginning-of-line)
                               (looking-at exps))))
          ))
    ))


(defun rst-get-next-adornment
  (curado hier &optional suggestion reverse-direction)
  "Get the next adornment for CURADO, in given hierarchy HIER.
If suggesting, suggest for new adornment SUGGESTION.
REVERSE-DIRECTION is used to reverse the cycling order."

  (let* (
         (char (car curado))
         (style (cadr curado))

         ;; Build a new list of adornments for the rotation.
         (rotados
          (append hier
                  ;; Suggest a new adornment.
                  (list suggestion
                        ;; If nothing to suggest, use first adornment.
                        (car hier)))) )
    (or
     ;; Search for next adornment.
     (cadr
      (let ((cur (if reverse-direction rotados
                   (reverse rotados))))
        (while (and cur
                    (not (and (eq char (caar cur))
                              (eq style (cadar cur)))))
          (setq cur (cdr cur)))
        cur))

     ;; If not found, take the first of all adornments.
     suggestion
     )))


;; FIXME: A line "``/`` full" is not accepted as a section title
(defun rst-adjust (pfxarg)
  "Auto-adjust the adornment around point.

Adjust/rotate the section adornment for the section title
around point or promote/demote the adornments inside the region,
depending on if the region is active.  This function is meant to
be invoked possibly multiple times, and can vary its behavior
with a positive prefix argument (toggle style), or with a
negative prefix argument (alternate behavior).

This function is a bit of a swiss knife. It is meant to adjust
the adornments of a section title in reStructuredText. It tries
to deal with all the possible cases gracefully and to do `the
right thing' in all cases.

See the documentations of `rst-adjust-adornment-work' and
`rst-promote-region' for full details.

Prefix Arguments
================

The method can take either (but not both) of

a. a (non-negative) prefix argument, which means to toggle the
   adornment style.  Invoke with a prefix arg for example;

b. a negative numerical argument, which generally inverts the
   direction of search in the file or hierarchy.  Invoke with C--
   prefix for example."
  (interactive "P")

  (let* (;; Save our original position on the current line.
	 (origpt (set-marker (make-marker) (point)))

         (reverse-direction (and pfxarg (< (prefix-numeric-value pfxarg) 0)))
         (toggle-style (and pfxarg (not reverse-direction))))

    (if (rst-portable-mark-active-p)
        ;; Adjust adornments within region.
        (rst-promote-region (and pfxarg t))
      ;; Adjust adornment around point.
      (rst-adjust-adornment-work toggle-style reverse-direction))

    ;; Run the hooks to run after adjusting.
    (run-hooks 'rst-adjust-hook)

    ;; Make sure to reset the cursor position properly after we're done.
    (goto-char origpt)

    ))

(defcustom rst-adjust-hook nil
  "Hooks to be run after running `rst-adjust'."
  :group 'rst-adjust
  :type '(hook)
  :package-version '(rst . "1.1.0"))

(defcustom rst-new-adornment-down nil
  "Controls level of new adornment for section headers."
  :group 'rst-adjust
  :type '(choice
	  (const :tag "Same level as previous one" nil)
	  (const :tag "One level down relative to the previous one" t))
  :package-version '(rst . "1.1.0"))

(defun rst-adjust-adornment (pfxarg)
  "Call `rst-adjust-adornment-work' interactively.

Keep this for compatibility for older bindings (are there any?)."
  (interactive "P")

  (let* ((reverse-direction (and pfxarg (< (prefix-numeric-value pfxarg) 0)))
         (toggle-style (and pfxarg (not reverse-direction))))
    (rst-adjust-adornment-work toggle-style reverse-direction)))

(defun rst-adjust-adornment-work (toggle-style reverse-direction)
"Adjust/rotate the section adornment for the section title around point.

This function is meant to be invoked possibly multiple times, and
can vary its behavior with a true TOGGLE-STYLE argument, or with
a REVERSE-DIRECTION argument.

General Behavior
================

The next action it takes depends on context around the point, and
it is meant to be invoked possibly more than once to rotate among
the various possibilities.  Basically, this function deals with:

- adding a adornment if the title does not have one;

- adjusting the length of the underline characters to fit a
  modified title;

- rotating the adornment in the set of already existing
  sectioning adornments used in the file;

- switching between simple and over-and-under styles.

You should normally not have to read all the following, just
invoke the method and it will do the most obvious thing that you
would expect.


Adornment Definitions
=====================

The adornments consist in

1. a CHARACTER

2. a STYLE which can be either of 'simple' or 'over-and-under'.

3. an INDENT (meaningful for the over-and-under style only)
   which determines how many characters and over-and-under
   style is hanging outside of the title at the beginning and
   ending.

See source code for mode details.


Detailed Behavior Description
=============================

Here are the gory details of the algorithm (it seems quite
complicated, but really, it does the most obvious thing in all
the particular cases):

Before applying the adornment change, the cursor is placed on
the closest line that could contain a section title.

Case 1: No Adornment
--------------------

If the current line has no adornment around it,

- search backwards for the last previous adornment, and apply
  the adornment one level lower to the current line.  If there
  is no defined level below this previous adornment, we suggest
  the most appropriate of the `rst-preferred-adornments'.

  If REVERSE-DIRECTION is true, we simply use the previous
  adornment found directly.

- if there is no adornment found in the given direction, we use
  the first of `rst-preferred-adornments'.

TOGGLE-STYLE forces a toggle of the prescribed adornment style.

Case 2: Incomplete Adornment
----------------------------

If the current line does have an existing adornment, but the
adornment is incomplete, that is, the underline/overline does
not extend to exactly the end of the title line (it is either too
short or too long), we simply extend the length of the
underlines/overlines to fit exactly the section title.

If TOGGLE-STYLE we toggle the style of the adornment as well.

REVERSE-DIRECTION has no effect in this case.

Case 3: Complete Existing Adornment
-----------------------------------

If the adornment is complete (i.e. the underline (overline)
length is already adjusted to the end of the title line), we
search/parse the file to establish the hierarchy of all the
adornments (making sure not to include the adornment around
point), and we rotate the current title's adornment from within
that list (by default, going *down* the hierarchy that is present
in the file, i.e. to a lower section level).  This is meant to be
used potentially multiple times, until the desired adornment is
found around the title.

If we hit the boundary of the hierarchy, exactly one choice from
the list of preferred adornments is suggested/chosen, the first
of those adornment that has not been seen in the file yet (and
not including the adornment around point), and the next
invocation rolls over to the other end of the hierarchy (i.e. it
cycles).  This allows you to avoid having to set which character
to use.

If REVERSE-DIRECTION is true, the effect is to change the
direction of rotation in the hierarchy of adornments, thus
instead going *up* the hierarchy.

However, if TOGGLE-STYLE, we do not rotate the adornment, but
instead simply toggle the style of the current adornment (this
should be the most common way to toggle the style of an existing
complete adornment).


Point Location
==============

The invocation of this function can be carried out anywhere
within the section title line, on an existing underline or
overline, as well as on an empty line following a section title.
This is meant to be as convenient as possible.


Indented Sections
=================

Indented section titles such as ::

   My Title
   --------

are invalid in reStructuredText and thus not recognized by the
parser.  This code will thus not work in a way that would support
indented sections (it would be ambiguous anyway).


Joint Sections
==============

Section titles that are right next to each other may not be
treated well.  More work might be needed to support those, and
special conditions on the completeness of existing adornments
might be required to make it non-ambiguous.

For now we assume that the adornments are disjoint, that is,
there is at least a single line between the titles/adornment
lines."
  (rst-reset-section-caches)
  (let ((ttl-fnd (rst-find-title-line))
	(orig-pnt (point)))
    (when ttl-fnd
      (set-match-data (cdr ttl-fnd))
      (goto-char (match-beginning 2))
      (let* ((moved (- (line-number-at-pos) (line-number-at-pos orig-pnt)))
	     (char (caar ttl-fnd))
	     (style (cdar ttl-fnd))
	     (indent (current-indentation))
	     (curado (list char style indent))
	     char-new style-new indent-new)
	(cond
	 ;;-------------------------------------------------------------------
	 ;; Case 1: No valid adornment
	 ((not style)
	  (let ((prev (car (rst-get-adornments-around)))
		cur
		(hier (rst-get-hierarchy)))
	    ;; Advance one level down.
	    (setq cur
		  (if prev
		      (if (or (and rst-new-adornment-down reverse-direction)
			      (and (not rst-new-adornment-down)
				   (not reverse-direction)))
			  prev
			(or (cadr (rst-get-adornment-match hier prev))
			    (rst-suggest-new-adornment hier prev)))
		    (copy-sequence (car rst-preferred-adornments))))
	    ;; Invert the style if requested.
	    (if toggle-style
		(setcar (cdr cur) (if (eq (cadr cur) 'simple)
				      'over-and-under 'simple)) )
	    (setq char-new (car cur)
		  style-new (cadr cur)
		  indent-new (caddr cur))))
	 ;;-------------------------------------------------------------------
	 ;; Case 2: Incomplete Adornment
	 ((not (rst-adornment-complete-p curado))
	  ;; Invert the style if requested.
	  (if toggle-style
	      (setq style (if (eq style 'simple) 'over-and-under 'simple)))
	  (setq char-new char
		style-new style
		indent-new indent))
	 ;;-------------------------------------------------------------------
	 ;; Case 3: Complete Existing Adornment
	 (t
	  (if toggle-style
	      ;; Simply switch the style of the current adornment.
	      (setq char-new char
		    style-new (if (eq style 'simple) 'over-and-under 'simple)
		    indent-new rst-default-indent)
	    ;; Else, we rotate, ignoring the adornment around the current
	    ;; line...
	    (let* ((hier (rst-get-hierarchy (line-number-at-pos)))
		   ;; Suggestion, in case we need to come up with something new
		   (suggestion (rst-suggest-new-adornment
				hier
				(car (rst-get-adornments-around))))
		   (nextado (rst-get-next-adornment
			     curado hier suggestion reverse-direction)))
	      ;; Indent, if present, always overrides the prescribed indent.
	      (setq char-new (car nextado)
		    style-new (cadr nextado)
		    indent-new (caddr nextado))))))
	;; Override indent with present indent!
	(setq indent-new (if (> indent 0) indent indent-new))
	(if (and char-new style-new)
	    (rst-update-section char-new style-new indent-new))
	;; Correct the position of the cursor to more accurately reflect where
	;; it was located when the function was invoked.
	(unless (zerop moved)
	  (forward-line (- moved))
	  (end-of-line))))))

;; Maintain an alias for compatibility.
(defalias 'rst-adjust-section-title 'rst-adjust)


(defun rst-promote-region (demote)
  "Promote the section titles within the region.

With argument DEMOTE or a prefix argument, demote the section
titles instead.  The algorithm used at the boundaries of the
hierarchy is similar to that used by `rst-adjust-adornment-work'."
  (interactive "P")
  (rst-reset-section-caches)
  (let* ((cur (rst-find-all-adornments))
         (hier (rst-get-hierarchy))
         (suggestion (rst-suggest-new-adornment hier))

         (region-begin-line (line-number-at-pos (region-beginning)))
         (region-end-line (line-number-at-pos (region-end)))

         marker-list
         )

    ;; Skip the markers that come before the region beginning
    (while (and cur (< (caar cur) region-begin-line))
      (setq cur (cdr cur)))

    ;; Create a list of markers for all the adornments which are found within
    ;; the region.
    (save-excursion
      (let (m line)
        (while (and cur (< (setq line (caar cur)) region-end-line))
          (setq m (make-marker))
          (goto-char (point-min))
          (forward-line (1- line))
          (push (list (set-marker m (point)) (cdar cur)) marker-list)
          (setq cur (cdr cur)) ))

      ;; Apply modifications.
      (let (nextado)
        (dolist (p marker-list)
          ;; Go to the adornment to promote.
          (goto-char (car p))

          ;; Rotate the next adornment.
          (setq nextado (rst-get-next-adornment
                          (cadr p) hier suggestion demote))

          ;; Update the adornment.
          (apply 'rst-update-section nextado)

          ;; Clear marker to avoid slowing down the editing after we're done.
          (set-marker (car p) nil)
          ))
      (setq deactivate-mark nil)
    )))



(defun rst-display-adornments-hierarchy (&optional adornments)
  "Display the current file's section title adornments hierarchy.
This function expects a list of (CHARACTER STYLE INDENT) triples
in ADORNMENTS."
  (interactive)
  (rst-reset-section-caches)
  (if (not adornments)
      (setq adornments (rst-get-hierarchy)))
  (with-output-to-temp-buffer "*rest section hierarchy*"
    (let ((level 1))
      (with-current-buffer standard-output
        (dolist (x adornments)
          (insert (format "\nSection Level %d" level))
          (apply 'rst-update-section x)
          (goto-char (point-max))
          (insert "\n")
          (incf level)
          ))
    )))

(defun rst-position (elem list)
  "Return position of ELEM in LIST or nil."
  (let ((tail (member elem list)))
    (if tail (- (length list) (length tail)))))

(defun rst-straighten-adornments ()
  "Redo all the adornments in the current buffer.
This is done using our preferred set of adornments.  This can be
used, for example, when using somebody else's copy of a document,
in order to adapt it to our preferred style."
  (interactive)
  (rst-reset-section-caches)
  (save-excursion
    (let (;; Get a list of pairs of (level . marker)
	  (levels-and-markers (mapcar
			       (lambda (ado)
				 (cons (rst-position (cdr ado)
						     (rst-get-hierarchy))
				       (let ((m (make-marker)))
					 (goto-char (point-min))
					 (forward-line (1- (car ado)))
					 (set-marker m (point))
					 m)))
			       (rst-find-all-adornments))))
      (dolist (lm levels-and-markers)
	;; Go to the appropriate position
	(goto-char (cdr lm))

	;; Apply the new styule
	(apply 'rst-update-section (nth (car lm) rst-preferred-adornments))

	;; Reset the market to avoid slowing down editing until it gets GC'ed
	(set-marker (cdr lm) nil)
	)
    )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert list items
;; =================


;=================================================
; Borrowed from a2r.el (version 1.3), by Lawrence Mitchell <wence@gmx.li>
; I needed to make some tiny changes to the functions, so I put it here.
; -- Wei-Wei Guo

(defconst rst-arabic-to-roman
  '((1000 .   "M") (900  .  "CM") (500  .   "D") (400  .  "CD")
    (100  .   "C") (90   .  "XC") (50   .   "L") (40   .  "XL")
    (10   .   "X") (9    .  "IX") (5    .   "V") (4    .  "IV")
    (1    .   "I"))
  "List of maps between Arabic numbers and their Roman numeral equivalents.")

(defun rst-arabic-to-roman (num &optional arg)
  "Convert Arabic number NUM to its Roman numeral representation.

Obviously, NUM must be greater than zero.  Don't blame me, blame the
Romans, I mean \"what have the Romans ever _done_ for /us/?\" (with
apologies to Monty Python).
If optional prefix ARG is non-nil, insert in current buffer."
  (let ((map rst-arabic-to-roman)
        res)
    (while (and map (> num 0))
      (if (or (= num (caar map))
              (> num (caar map)))
          (setq res (concat res (cdar map))
                num (- num (caar map)))
        (setq map (cdr map))))
    res))

(defun rst-roman-to-arabic (string &optional arg)
  "Convert STRING of Roman numerals to an Arabic number.

If STRING contains a letter which isn't a valid Roman numeral, the rest
of the string from that point onwards is ignored.

Hence:
MMD == 2500
and
MMDFLXXVI == 2500.
If optional ARG is non-nil, insert in current buffer."
  (let ((res 0)
        (map rst-arabic-to-roman))
    (while map
      (if (string-match (concat "^" (cdar map)) string)
          (setq res (+ res (caar map))
                string (replace-match "" nil t string))
        (setq map (cdr map))))
    res))
;=================================================

(defun rst-find-pfx-in-region (beg end pfx-re)
  "Find all the positions of prefixes in region between BEG and END.
This is used to find bullets and enumerated list items. PFX-RE is
a regular expression for matching the lines after indentation
with items. Returns a list of cons cells consisting of the point
and the column of the point."
  (let (pfx)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(back-to-indentation)
	(when (and
	       (looking-at pfx-re) ; pfx found and...
	       (let ((pfx-col (current-column)))
		 (save-excursion
		   (forward-line -1) ; ...previous line is...
		   (back-to-indentation)
		   (or (looking-at (rst-re 'lin-end)) ; ...empty,
		       (> (current-column) pfx-col) ; ...deeper level, or
		       (and (= (current-column) pfx-col)
			    (looking-at pfx-re)))))) ; ...pfx at same level
	  (push (cons (point) (current-column))
                pfx))
	(forward-line 1)) )
    (nreverse pfx)))

(defun rst-insert-list-pos (newitem)
  "Arrange relative position of a newly inserted list item.

Adding a new list might consider three situations:

 (a) Current line is a blank line.
 (b) Previous line is a blank line.
 (c) Following line is a blank line.

When (a) and (b), just add the new list at current line.

when (a) and not (b), a blank line is added before adding the new list.

When not (a), first forward point to the end of the line, and add two
blank lines, then add the new list.

Other situations are just ignored and left to users themselves."
  (if (save-excursion
        (beginning-of-line)
        (looking-at (rst-re 'lin-end)))
      (if (save-excursion
            (forward-line -1)
            (looking-at (rst-re 'lin-end)))
          (insert newitem " ")
        (insert "\n" newitem " "))
    (end-of-line)
    (insert "\n\n" newitem " ")))

(defvar rst-initial-enums
  (let (vals)
    (dolist (fmt '("%s." "(%s)" "%s)"))
      (dolist (c '("1" "a" "A" "I" "i"))
        (push (format fmt c) vals)))
    (cons "#." (nreverse vals)))
  "List of initial enumerations.")

(defvar rst-initial-items
  (append (mapcar 'char-to-string rst-bullets) rst-initial-enums)
  "List of initial items.  It's collection of bullets and enumerations.")

(defun rst-insert-list-new-item ()
  "Insert a new list item.

User is asked to select the item style first, for example (a), i), +.  Use TAB
for completition and choices.

If user selects bullets or #, it's just added with position arranged by
`rst-insert-list-pos'.

If user selects enumerations, a further prompt is given. User need to input a
starting item, for example 'e' for 'A)' style.  The position is also arranged by
`rst-insert-list-pos'."
  (interactive)
  ;; FIXME: Make this comply to `interactive' standards
  (let* ((itemstyle (completing-read
		     "Select preferred item style [#.]: "
		     rst-initial-items nil t nil nil "#."))
	 (cnt (if (string-match (rst-re 'cntexp-tag) itemstyle)
		  (match-string 0 itemstyle)))
	 (no
	  (save-match-data
	    ;; FIXME: Make this comply to `interactive' standards
	    (cond
	     ((equal cnt "a")
	      (let ((itemno (read-string "Give starting value [a]: "
					 nil nil "a")))
		(downcase (substring itemno 0 1))))
	     ((equal cnt "A")
	      (let ((itemno (read-string "Give starting value [A]: "
					 nil nil "A")))
		(upcase (substring itemno 0 1))))
	     ((equal cnt "I")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(rst-arabic-to-roman itemno)))
	     ((equal cnt "i")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(downcase (rst-arabic-to-roman itemno))))
	     ((equal cnt "1")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(number-to-string itemno)))))))
    (if no
	(setq itemstyle (replace-match no t t itemstyle)))
    (rst-insert-list-pos itemstyle)))

(defcustom rst-preferred-bullets
  '(?* ?- ?+)
  "List of favorite bullets."
  :group 'rst
  :type `(repeat
	  (choice ,@(mapcar (lambda (char)
			      (list 'const
				    :tag (char-to-string char) char))
			    rst-bullets)))
  :package-version '(rst . "1.1.0"))

(defun rst-insert-list-continue (curitem prefer-roman)
  "Insert a list item with list start CURITEM including its indentation level."
  (end-of-line)
  (insert
   "\n" ; FIXME: Separating lines must be possible
   (cond
    ((string-match (rst-re '(:alt enmaut-tag
				  bul-tag)) curitem)
     curitem)
    ((string-match (rst-re 'num-tag) curitem)
     (replace-match (number-to-string
		     (1+ (string-to-number (match-string 0 curitem))))
		    nil nil curitem))
    ((and (string-match (rst-re 'rom-tag) curitem)
	  (save-match-data
	    (if (string-match (rst-re 'ltr-tag) curitem) ; Also a letter tag
		(save-excursion
		  ;; FIXME: Assumes one line list items without separating
		  ;; empty lines
		  (if (and (zerop (forward-line -1))
			   (looking-at (rst-re 'enmexp-beg)))
		      (string-match
		       (rst-re 'rom-tag)
		       (match-string 0)) ; Previous was a roman tag
		    prefer-roman)) ; Don't know - use flag
	      t))) ; Not a letter tag
     (replace-match
      (let* ((old (match-string 0 curitem))
	     (new (save-match-data
		    (rst-arabic-to-roman
		     (1+ (rst-roman-to-arabic
			  (upcase old)))))))
	(if (equal old (upcase old))
	    (upcase new)
	  (downcase new)))
      t nil curitem))
    ((string-match (rst-re 'ltr-tag) curitem)
     (replace-match (char-to-string
		     (1+ (string-to-char (match-string 0 curitem))))
		    nil nil curitem)))))


(defun rst-insert-list (&optional prefer-roman)
  "Insert a list item at the current point.

The command can insert a new list or a continuing list. When it is called at a
non-list line, it will promote to insert new list. When it is called at a list
line, it will insert a list with the same list style.

1. When inserting a new list:

User is asked to select the item style first, for example (a), i), +. Use TAB
for completition and choices.

 (a) If user selects bullets or #, it's just added.
 (b) If user selects enumerations, a further prompt is given.  User needs to
     input a starting item, for example 'e' for 'A)' style.

The position of the new list is arranged according to whether or not the
current line and the previous line are blank lines.

2. When continuing a list, one thing need to be noticed:

List style alphabetical list, such as 'a.', and roman numerical list, such as
'i.', have some overlapping items, for example 'v.' The function can deal with
the problem elegantly in most situations.  But when those overlapped list are
preceded by a blank line, it is hard to determine which type to use
automatically.  The function uses alphabetical list by default.  If you want
roman numerical list, just use a prefix (\\[universal-argument])."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at (rst-re 'itmany-beg-1))
      (rst-insert-list-continue (match-string 0) prefer-roman)
    (rst-insert-list-new-item)))

(defun rst-straighten-bullets-region (beg end)
  "Make all the bulleted list items in the region consistent.
The region is specified between BEG and END.  You can use this
after you have merged multiple bulleted lists to make them use
the same/correct/consistent bullet characters.

See variable `rst-preferred-bullets' for the list of bullets to
adjust.  If bullets are found on levels beyond the
`rst-preferred-bullets' list, they are not modified."
  (interactive "r")

  (let ((bullets (rst-find-pfx-in-region beg end (rst-re 'bul-sta)))
	(levtable (make-hash-table :size 4)))

    ;; Create a map of levels to list of positions.
    (dolist (x bullets)
      (let ((key (cdr x)))
	(puthash key
		  (append (gethash key levtable (list))
			  (list (car x)))
		  levtable)))

    ;; Sort this map and create a new map of prefix char and list of positions.
    (let ((poslist ()))                 ; List of (indent . positions).
      (maphash (lambda (x y) (push (cons x y) poslist)) levtable)

      (let ((bullets rst-preferred-bullets))
        (dolist (x (sort poslist 'car-less-than-car))
          (when bullets
            ;; Apply the characters.
            (dolist (pos (cdr x))
              (goto-char pos)
              (delete-char 1)
              (insert (string (car bullets))))
            (setq bullets (cdr bullets))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of contents
;; =================

(defun rst-get-stripped-line ()
  "Return the line at cursor, stripped from whitespace."
  (re-search-forward (rst-re "\\S .*\\S ") (line-end-position))
  (buffer-substring-no-properties (match-beginning 0)
                                  (match-end 0)) )

(defun rst-section-tree ()
  "Get the hierarchical tree of section titles.

Returns a hierarchical tree of the sections titles in the
document. This can be used to generate a table of contents for
the document. The top node will always be a nil node, with the
top level titles as children (there may potentially be more than
one).

Each section title consists in a cons of the stripped title
string and a marker to the section in the original text document.

If there are missing section levels, the section titles are
inserted automatically, and the title string is set to nil, and
the marker set to the first non-nil child of itself.
Conceptually, the nil nodes--i.e. those which have no title--are
to be considered as being the same line as their first non-nil
child.  This has advantages later in processing the graph."

  (let ((hier (rst-get-hierarchy))
	(levels (make-hash-table :test 'equal :size 10))
	lines)

    (let ((lev 0))
      (dolist (ado hier)
	;; Compare just the character and indent in the hash table.
        (puthash (cons (car ado) (cadr ado)) lev levels)
        (incf lev)))

    ;; Create a list of lines that contains (text, level, marker) for each
    ;; adornment.
    (save-excursion
      (setq lines
            (mapcar (lambda (ado)
                      (goto-char (point-min))
                      (forward-line (1- (car ado)))
                      (list (gethash (cons (cadr ado) (caddr ado)) levels)
                            (rst-get-stripped-line)
                            (let ((m (make-marker)))
                              (beginning-of-line 1)
                              (set-marker m (point)))
                            ))
                    (rst-find-all-adornments))))
    (let ((lcontnr (cons nil lines)))
      (rst-section-tree-rec lcontnr -1))))


(defun rst-section-tree-rec (ados lev)
  "Recursive guts of the section tree construction.
ADOS is a cons cell whose cdr is the remaining list of
adornments, and we change it as we consume them.  LEV is
the current level of that node.  This function returns a
pair of the subtree that was built.  This treats the ADOS
list destructively."

  (let ((nado (cadr ados))
        node
        children)

    ;; If the next adornment matches our level
    (when (and nado (= (car nado) lev))
      ;; Pop the next adornment and create the current node with it
      (setcdr ados (cddr ados))
      (setq node (cdr nado)) )
    ;; Else we let the node title/marker be unset.

    ;; Build the child nodes
    (while (and (cdr ados) (> (caadr ados) lev))
      (setq children
            (cons (rst-section-tree-rec ados (1+ lev))
                  children)))
    (setq children (reverse children))

    ;; If node is still unset, we use the marker of the first child.
    (when (eq node nil)
      (setq node (cons nil (cdaar children))))

    ;; Return this node with its children.
    (cons node children)
    ))


(defun rst-section-tree-point (node &optional point)
  "Find tree node at point.
Given a computed and valid section tree in NODE and a point
POINT (default being the current point in the current buffer),
find and return the node within the sectree where the cursor
lives.

Return values: a pair of (parent path, container subtree).
The parent path is simply a list of the nodes above the
container subtree node that we're returning."

  (let (path outtree)

    (let* ((curpoint (or point (point))))

      ;; Check if we are before the current node.
      (if (and (cadar node) (>= curpoint (cadar node)))

	  ;; Iterate all the children, looking for one that might contain the
	  ;; current section.
	  (let ((curnode (cdr node))
		last)

	    (while (and curnode (>= curpoint (cadaar curnode)))
	      (setq last curnode
		    curnode (cdr curnode)))

	    (if last
		(let ((sub (rst-section-tree-point (car last) curpoint)))
		  (setq path (car sub)
			outtree (cdr sub)))
	      (setq outtree node))

	    )))
    (cons (cons (car node) path) outtree)
    ))


(defgroup rst-toc nil
  "Settings for reStructuredText table of contents."
  :group 'rst
  :version "21.1")

(defcustom rst-toc-indent 2
  "Indentation for table-of-contents display.
Also used for formatting insertion, when numbering is disabled."
  :group 'rst-toc)

(defcustom rst-toc-insert-style 'fixed
  "Insertion style for table-of-contents.
Set this to one of the following values to determine numbering and
indentation style:
- plain: no numbering (fixed indentation)
- fixed: numbering, but fixed indentation
- aligned: numbering, titles aligned under each other
- listed: numbering, with dashes like list items (EXPERIMENTAL)"
  :group 'rst-toc)

(defcustom rst-toc-insert-number-separator "  "
  "Separator that goes between the TOC number and the title."
  :group 'rst-toc)

;; This is used to avoid having to change the user's mode.
(defvar rst-toc-insert-click-keymap
  (let ((map (make-sparse-keymap)))
       (define-key map [mouse-1] 'rst-toc-mode-mouse-goto)
       map)
  "(Internal) What happens when you click on propertized text in the TOC.")

(defcustom rst-toc-insert-max-level nil
  "If non-nil, maximum depth of the inserted TOC."
  :group 'rst-toc)


(defun rst-toc-insert (&optional pfxarg)
  "Insert a simple text rendering of the table of contents.
By default the top level is ignored if there is only one, because
we assume that the document will have a single title.

If a numeric prefix argument PFXARG is given, insert the TOC up
to the specified level.

The TOC is inserted indented at the current column."
  (interactive "P")
  (rst-reset-section-caches)
  (let* (;; Check maximum level override
         (rst-toc-insert-max-level
          (if (and (integerp pfxarg) (> (prefix-numeric-value pfxarg) 0))
              (prefix-numeric-value pfxarg) rst-toc-insert-max-level))

         ;; Get the section tree for the current cursor point.
         (sectree-pair
	  (rst-section-tree-point
	   (rst-section-tree)))

         ;; Figure out initial indent.
         (initial-indent (make-string (current-column) ? ))
         (init-point (point)))

    (when (cddr sectree-pair)
      (rst-toc-insert-node (cdr sectree-pair) 0 initial-indent "")

      ;; Fixup for the first line.
      (delete-region init-point (+ init-point (length initial-indent)))

      ;; Delete the last newline added.
      (delete-backward-char 1)
    )))

(defun rst-toc-insert-node (node level indent pfx)
  "Insert tree node NODE in table-of-contents.
Recursive function that does printing of the inserted toc.
LEVEL is the depth level of the sections in the tree.
INDENT is the indentation string.  PFX is the prefix numbering,
that includes the alignment necessary for all the children of
level to align."

  ;; Note: we do child numbering from the parent, so we start number the
  ;; children one level before we print them.
  (let ((do-print (> level 0))
        (count 1))
    (when do-print
      (insert indent)
      (let ((b (point)))
	(unless (equal rst-toc-insert-style 'plain)
	  (insert pfx rst-toc-insert-number-separator))
	(insert (or (caar node) "[missing node]"))
	;; Add properties to the text, even though in normal text mode it
	;; won't be doing anything for now.  Not sure that I want to change
	;; mode stuff.  At least the highlighting gives the idea that this
	;; is generated automatically.
	(put-text-property b (point) 'mouse-face 'highlight)
	(put-text-property b (point) 'rst-toc-target (cadar node))
	(put-text-property b (point) 'keymap rst-toc-insert-click-keymap)

	)
      (insert "\n")

      ;; Prepare indent for children.
      (setq indent
	    (cond
	     ((eq rst-toc-insert-style 'plain)
              (concat indent (make-string rst-toc-indent ? )))

	     ((eq rst-toc-insert-style 'fixed)
	      (concat indent (make-string rst-toc-indent ? )))

	     ((eq rst-toc-insert-style 'aligned)
	      (concat indent (make-string (+ (length pfx) 2) ? )))

	     ((eq rst-toc-insert-style 'listed)
	      (concat (substring indent 0 -3)
		      (concat (make-string (+ (length pfx) 2) ? ) " - ")))
	     ))
      )

    (if (or (eq rst-toc-insert-max-level nil)
            (< level rst-toc-insert-max-level))
        (let ((do-child-numbering (>= level 0))
              fmt)
          (if do-child-numbering
              (progn
                ;; Add a separating dot if there is already a prefix
                (when (> (length pfx) 0)
		  (string-match (rst-re "[ \t\n]*\\'") pfx)
		  (setq pfx (concat (replace-match "" t t pfx) ".")))

                ;; Calculate the amount of space that the prefix will require
                ;; for the numbers.
                (if (cdr node)
                    (setq fmt (format "%%-%dd"
                                      (1+ (floor (log10 (length
							 (cdr node))))))))
                ))

          (dolist (child (cdr node))
            (rst-toc-insert-node child
				 (1+ level)
				 indent
				 (if do-child-numbering
				     (concat pfx (format fmt count)) pfx))
            (incf count)))

      )))


(defun rst-toc-update ()
  "Automatically find the contents section of a document and update.
Updates the inserted TOC if present.  You can use this in your
file-write hook to always make it up-to-date automatically."
  (interactive)
  (save-excursion
    ;; Find and delete an existing comment after the first contents directive.
    ;; Delete that region.
    (goto-char (point-min))
    ;; We look for the following and the following only (in other words, if your
    ;; syntax differs, this won't work.).
    ;;
    ;;   .. contents:: [...anything here...]
    ;;      [:field: value]...
    ;;   ..
    ;;      XXXXXXXX
    ;;      XXXXXXXX
    ;;      [more lines]
    (let ((beg (re-search-forward
		(rst-re "^" 'exm-sta "contents" 'dcl-tag ".*\n"
			"\\(?:" 'hws-sta 'fld-tag ".*\n\\)*" 'exm-tag) nil t))
	  last-real)
      (when beg
	;; Look for the first line that starts at the first column.
	(forward-line 1)
	(while (and
		(< (point) (point-max))
		(or (if (looking-at
			 (rst-re 'hws-sta "\\S ")) ; indented content
			(setq last-real (point)))
		    (looking-at (rst-re 'lin-end)))) ; empty line
	  (forward-line 1))
	(if last-real
	    (progn
	      (goto-char last-real)
	      (end-of-line)
	      (delete-region beg (point)))
	  (goto-char beg))
	(insert "\n    ")
	(rst-toc-insert))))
  ;; Note: always return nil, because this may be used as a hook.
  nil)

;; Note: we cannot bind the TOC update on file write because it messes with
;; undo.  If we disable undo, since it adds and removes characters, the
;; positions in the undo list are not making sense anymore.  Dunno what to do
;; with this, it would be nice to update when saving.
;;
;; (add-hook 'write-contents-hooks 'rst-toc-update-fun)
;; (defun rst-toc-update-fun ()
;;   ;; Disable undo for the write file hook.
;;   (let ((buffer-undo-list t)) (rst-toc-update) ))

(defalias 'rst-toc-insert-update 'rst-toc-update) ; backwards compat.

;;------------------------------------------------------------------------------

(defun rst-toc-node (node level)
  "Recursive function that does insert NODE at LEVEL in the table-of-contents."

  (if (> level 0)
      (let ((b (point)))
        ;; Insert line text.
        (insert (make-string (* rst-toc-indent (1- level)) ? ))
        (insert (or (caar node) "[missing node]"))

        ;; Highlight lines.
        (put-text-property b (point) 'mouse-face 'highlight)

        ;; Add link on lines.
        (put-text-property b (point) 'rst-toc-target (cadar node))

        (insert "\n")
	))

  (dolist (child (cdr node))
    (rst-toc-node child (1+ level))))

(defun rst-toc-count-lines (node target-node)
  "Count the number of lines from NODE to the TARGET-NODE node.
This recursive function returns a cons of the number of
additional lines that have been counted for its node and
children, and t if the node has been found."

  (let ((count 1)
	found)
    (if (eq node target-node)
	(setq found t)
      (let ((child (cdr node)))
	(while (and child (not found))
	  (let ((cl (rst-toc-count-lines (car child) target-node)))
	    (setq count (+ count (car cl))
		  found (cdr cl)
		  child (cdr child))))))
    (cons count found)))

(defvar rst-toc-buffer-name "*Table of Contents*"
  "Name of the Table of Contents buffer.")

(defvar rst-toc-return-buffer nil
  "Window configuration to which to return when leaving the TOC.")


(defun rst-toc ()
  "Display a table-of-contents.
Finds all the section titles and their adornments in the
file, and displays a hierarchically-organized list of the
titles, which is essentially a table-of-contents of the
document.

The Emacs buffer can be navigated, and selecting a section
brings the cursor in that section."
  (interactive)
  (rst-reset-section-caches)
  (let* ((curbuf (list (current-window-configuration) (point-marker)))
         (sectree (rst-section-tree))

 	 (our-node (cdr (rst-section-tree-point sectree)))
	 line

         ;; Create a temporary buffer.
         (buf (get-buffer-create rst-toc-buffer-name))
         )

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (rst-toc-mode)
        (delete-region (point-min) (point-max))
        (insert (format "Table of Contents: %s\n" (or (caar sectree) "")))
        (put-text-property (point-min) (point)
                           'face (list '(background-color . "gray")))
        (rst-toc-node sectree 0)

	;; Count the lines to our found node.
	(let ((linefound (rst-toc-count-lines sectree our-node)))
	  (setq line (if (cdr linefound) (car linefound) 0)))
        ))
    (display-buffer buf)
    (pop-to-buffer buf)

    ;; Save the buffer to return to.
    (set (make-local-variable 'rst-toc-return-buffer) curbuf)

    ;; Move the cursor near the right section in the TOC.
    (goto-char (point-min))
    (forward-line (1- line))
    ))


(defun rst-toc-mode-find-section ()
  "Get the section from text property at point."
  (let ((pos (get-text-property (point) 'rst-toc-target)))
    (unless pos
      (error "No section on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this section was killed"))
    pos))

;; FIXME: Cursor before or behind the list must be handled properly; before the
;;        list should jump to the top and behind the list to the last normal
;;        paragraph
(defun rst-goto-section (&optional kill)
  "Go to the section the current line describes."
  (interactive)
  (let ((pos (rst-toc-mode-find-section)))
    (when kill
      (set-window-configuration (car rst-toc-return-buffer))
      (kill-buffer (get-buffer rst-toc-buffer-name)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    ;; FIXME: make the recentering conditional on scroll.
    (recenter 5)))

(defun rst-toc-mode-goto-section ()
  "Go to the section the current line describes and kill the TOC buffer."
  (interactive)
  (rst-goto-section t))

(defun rst-toc-mode-mouse-goto (event)
  "In `rst-toc' mode, go to the occurrence whose line you click on.
EVENT is the input event."
  (interactive "e")
  (let (pos)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq pos (rst-toc-mode-find-section))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (recenter 5)))

(defun rst-toc-mode-mouse-goto-kill (event)
  "Same as `rst-toc-mode-mouse-goto', but kill TOC buffer as well."
  (interactive "e")
  (call-interactively 'rst-toc-mode-mouse-goto event)
  (kill-buffer (get-buffer rst-toc-buffer-name)))

(defun rst-toc-quit-window ()
  "Leave the current TOC buffer."
  (interactive)
  (let ((retbuf rst-toc-return-buffer))
    (set-window-configuration (car retbuf))
    (goto-char (cadr retbuf))))

(defvar rst-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rst-toc-mode-mouse-goto-kill)
    (define-key map [mouse-2] 'rst-toc-mode-mouse-goto)
    (define-key map "\C-m" 'rst-toc-mode-goto-section)
    (define-key map "f" 'rst-toc-mode-goto-section)
    (define-key map "q" 'rst-toc-quit-window)
    (define-key map "z" 'kill-this-buffer)
    map)
  "Keymap for `rst-toc-mode'.")

(put 'rst-toc-mode 'mode-class 'special)

;; Could inherit from the new `special-mode'.
(define-derived-mode rst-toc-mode nil "ReST-TOC"
  "Major mode for output from \\[rst-toc], the table-of-contents for the document."
  (setq buffer-read-only t))

;; Note: use occur-mode (replace.el) as a good example to complete missing
;; features.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section movement commands
;; =========================

(defun rst-forward-section (&optional offset)
  "Skip to the next reStructuredText section title.
OFFSET specifies how many titles to skip.  Use a negative OFFSET to move
backwards in the file (default is to use 1)."
  (interactive)
  (rst-reset-section-caches)
  (let* (;; Default value for offset.
         (offset (or offset 1))

         ;; Get all the adornments in the file, with their line numbers.
         (allados (rst-find-all-adornments))

         ;; Get the current line.
         (curline (line-number-at-pos))

         (cur allados)
         (idx 0)
         )

    ;; Find the index of the "next" adornment w.r.t. to the current line.
    (while (and cur (< (caar cur) curline))
      (setq cur (cdr cur))
      (incf idx))
    ;; 'cur' is the adornment on or following the current line.

    (if (and (> offset 0) cur (= (caar cur) curline))
        (incf idx))

    ;; Find the final index.
    (setq idx (+ idx (if (> offset 0) (- offset 1) offset)))
    (setq cur (nth idx allados))

    ;; If the index is positive, goto the line, otherwise go to the buffer
    ;; boundaries.
    (if (and cur (>= idx 0))
        (progn
          (goto-char (point-min))
          (forward-line (1- (car cur))))
      (if (> offset 0) (goto-char (point-max)) (goto-char (point-min))))
    ))

(defun rst-backward-section ()
  "Like `rst-forward-section', except move back one title."
  (interactive)
  (rst-forward-section -1))

(defun rst-mark-section (&optional arg allow-extend)
  "Select the section that point is currently in."
  ;; Cloned from mark-paragraph.
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero sections"))
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (rst-portable-mark-active-p)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (rst-forward-section arg)
	    (point))))
	(t
	 (rst-forward-section arg)
	 (push-mark nil t t)
	 (rst-forward-section (- arg)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to work on item lists (e.g. indent/dedent, enumerate), which are
;; always 2 or 3 characters apart horizontally with rest.

(defun rst-find-leftmost-column (beg end)
  "Return the leftmost column in region BEG to END."
  (let (mincol)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (looking-at (rst-re 'lin-end))
	  (setq mincol (if mincol
			   (min mincol (current-column))
			 (current-column))))
        (forward-line 1)))
    mincol))

(defmacro rst-iterate-leftmost-paragraphs
  (beg end first-only body-consequent body-alternative)
  "FIXME This definition is old and deprecated / we need to move
to the newer version below:

Call FUN at the beginning of each line, with an argument that
specifies whether we are at the first line of a paragraph that
starts at the leftmost column of the given region BEG and END.
Set FIRST-ONLY to true if you want to callback on the first line
of each paragraph only."
  `(save-excursion
    (let ((leftcol (rst-find-leftmost-column ,beg ,end))
	  (endm (set-marker (make-marker) ,end))
	  )

      (do* (;; Iterate lines
	    (l (progn (goto-char ,beg) (back-to-indentation))
	       (progn (forward-line 1) (back-to-indentation)))

	    (previous nil valid)

 	    (curcol (current-column)
		    (current-column))

	    (valid (and (= curcol leftcol)
			(not (looking-at (rst-re 'lin-end))))
		   (and (= curcol leftcol)
			(not (looking-at (rst-re 'lin-end)))))
	    )
	  ((>= (point) endm))

	(if (if ,first-only
		(and valid (not previous))
	      valid)
	    ,body-consequent
	  ,body-alternative)

	))))

(defmacro rst-iterate-leftmost-paragraphs-2 (spec &rest body)
  "Evaluate BODY for each line in region defined by BEG END.
LEFTMOST is set to true if the line is one of the leftmost of the
entire paragraph.  PARABEGIN is set to true if the line is the
first of a paragraph."
  (declare (indent 1) (debug (sexp body)))
  (destructuring-bind
      (beg end parabegin leftmost isleftmost isempty) spec

  `(save-excursion
     (let ((,leftmost (rst-find-leftmost-column ,beg ,end))
	   (endm (set-marker (make-marker) ,end))
	   )

      (do* (;; Iterate lines
	    (l (progn (goto-char ,beg) (back-to-indentation))
	       (progn (forward-line 1) (back-to-indentation)))

 	    (empty-line-previous nil ,isempty)

	    (,isempty (looking-at (rst-re 'lin-end))
			(looking-at (rst-re 'lin-end)))

	    (,parabegin (not ,isempty)
			(and empty-line-previous
			     (not ,isempty)))

	    (,isleftmost (and (not ,isempty)
			      (= (current-column) ,leftmost))
			 (and (not ,isempty)
			      (= (current-column) ,leftmost)))
	    )
	  ((>= (point) endm))

	(progn ,@body)

	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

;; FIXME: At the moment only block comments with leading empty comment line are
;; supported; comment lines with leading comment markup should be also
;; supported; may be a customizable option could control which style to prefer

(defgroup rst-indent nil "Settings for indendation in reStructuredText.

In reStructuredText indendation points are usually determined by
preceding lines. Sometimes the syntax allows arbitrary
indendation points such as where to start the first line
following a directive. These indentation widths can be customized
here."
  :group 'rst
  :package-version '(rst . "1.1.0"))

(define-obsolete-variable-alias
  'rst-shift-basic-offset 'rst-indent-width "r6713")
(defcustom rst-indent-width 2
  "Indentation when there is no more indentation point given."
  :group 'rst-indent
  :type '(integer))

(defcustom rst-indent-field 3
  "Default indendation for first line after a field or 0 to always indent for
content."
  :group 'rst-indent
  :type '(integer))

(defcustom rst-indent-literal-normal 3
  "Default indendation for literal block after a markup on an own
line."
  :group 'rst-indent
  :type '(integer))

(defcustom rst-indent-literal-minimized 2
  "Default indendation for literal block after a minimized
markup."
  :group 'rst-indent
  :type '(integer))

(defcustom rst-indent-comment 3
  "Default indendation for first line of a comment."
  :group 'rst-indent
  :type '(integer))

;; FIXME: Must consider other tabs:
;; * Line blocks
;; * Definition lists
;; * Option lists
(defun rst-line-tabs ()
  "Return tabs of the current line or nil for no tab.
The list is sorted so the tab where writing continues most likely
is the first one. Each tab is of the form (COLUMN . INNER).
COLUMN is the column of the tab. INNER is non-nil if this is an
inner tab. I.e. a tab which does come from the basic indentation
and not from inner alignment points."
  (save-excursion
    (forward-line 0)
    (save-match-data
      (unless (looking-at (rst-re 'lin-end))
	(back-to-indentation)
	;; Current indendation is always the least likely tab
	(let ((tabs (list (list (point) 0 nil)))) ; (POINT OFFSET INNER)
	  ;; Push inner tabs more likely to continue writing
	  (cond
	   ;; Item
	   ((looking-at (rst-re '(:grp itmany-tag hws-sta) '(:grp "\\S ") "?"))
	    (when (match-string 2)
	      (push (list (match-beginning 2) 0 t) tabs)))
	   ;; Field
	   ((looking-at (rst-re '(:grp fld-tag) '(:grp hws-tag)
				'(:grp "\\S ") "?"))
	    (unless (zerop rst-indent-field)
	      (push (list (match-beginning 1) rst-indent-field t) tabs))
	    (if (match-string 3)
		(push (list (match-beginning 3) 0 t) tabs)
	      (if (zerop rst-indent-field)
		  (push (list (match-end 2)
			      (if (string= (match-string 2) "") 1 0)
			      t) tabs))))
	   ;; Directive
	   ((looking-at (rst-re 'dir-sta-3 '(:grp "\\S ") "?"))
	    (push (list (match-end 1) 0 t) tabs)
	    (unless (string= (match-string 2) "")
	      (push (list (match-end 2) 0 t) tabs))
	    (when (match-string 4)
	      (push (list (match-beginning 4) 0 t) tabs)))
	   ;; Footnote or citation definition
	   ((looking-at (rst-re 'fnc-sta-2 '(:grp "\\S ") "?"))
	    (push (list (match-end 1) 0 t) tabs)
	    (when (match-string 3)
	      (push (list (match-beginning 3) 0 t) tabs)))
	   ;; Comment
	   ((looking-at (rst-re 'cmt-sta-1))
	    (push (list (point) rst-indent-comment t) tabs)))
	  ;; Start of literal block
	  (when (looking-at (rst-re 'lit-sta-2))
	    (let ((tab0 (first tabs)))
	      (push (list (first tab0)
			  (+ (second tab0)
			     (if (match-string 1)
				 rst-indent-literal-minimized
			       rst-indent-literal-normal))
			  t) tabs)))
	  (mapcar (lambda (tab)
		    (goto-char (first tab))
		    (cons (+ (current-column) (second tab)) (third tab)))
		  tabs))))))

(defun rst-compute-tabs (pt)
  "Build the list of possible tabs for all lines above.
Search backwards from point PT to build the list of possible
tabs. Return a list of tabs sorted by likeliness to continue
writing like `rst-line-tabs'. Nearer lines have generally a
higher likeliness than farer lines. Return nil if no tab is found
in the text above."
  (save-excursion
    (goto-char pt)
    (let (leftmost ; Leftmost column found so far
	  innermost ; Leftmost column for inner tab
	  tablist)
      (while (and (zerop (forward-line -1))
		  (or (not leftmost)
		      (> leftmost 0)))
	(let* ((tabs (rst-line-tabs))
	       (leftcol (if tabs (apply 'min (mapcar 'car tabs)))))
	  (when tabs
	    ;; Consider only lines indented less or same if not INNERMOST
	    (when (or (not leftmost)
		      (< leftcol leftmost)
		      (and (not innermost) (= leftcol leftmost)))
	      (dolist (tab tabs)
		(let ((inner (cdr tab))
		      (newcol (car tab)))
		  (when (and
			 (or
			  (and (not inner)
			       (or (not leftmost)
				   (< newcol leftmost)))
			  (and inner
			       (or (not innermost)
				   (< newcol innermost))))
			 (not (memq newcol tablist)))
		    (push newcol tablist))))
	      (setq innermost (if (some 'identity
					(mapcar 'cdr tabs)) ; Has inner
				  leftcol
				innermost))
	      (setq leftmost leftcol)))))
      (nreverse tablist))))

(defun rst-indent-line (&optional dflt)
  "Indent current line to next best reStructuredText tab.
The next best tab is taken from the tab list returned by
`rst-compute-tabs' which is used in a cyclic manner. If the
current indentation does not end on a tab use the first one. If
the current indentation is on a tab use the next tab. This allows
a repeated use of \\[indent-for-tab-command] to cycle through all
possible tabs. If no indentation is possible return `noindent' or
use DFLT. Return the indentation indented to. When point is in
indentation it ends up at its end. Otherwise the point is kept
relative to the content."
  (let* ((pt (point-marker))
	 (cur (current-indentation))
	 (clm (current-column))
	 (tabs (rst-compute-tabs (point)))
	 (fnd (position cur tabs))
	 ind)
    (if (and (not tabs) (not dflt))
	'noindent
      (if (not tabs)
	  (setq ind dflt)
	(if (not fnd)
	    (setq fnd 0)
	  (setq fnd (1+ fnd))
	  (if (>= fnd (length tabs))
	      (setq fnd 0)))
	(setq ind (nth fnd tabs)))
      (indent-line-to ind)
      (if (> clm cur)
	  (goto-char pt))
      (set-marker pt nil)
      ind)))

(defun rst-shift-region (beg end cnt)
  "Shift region BEG to END by CNT tabs.
Shift by one tab to the right (CNT > 0) or left (CNT < 0) or
remove all indentation (CNT = 0). An tab is taken from the text
above. If no suitable tab is found `rst-indent-width' is used."
  (interactive "r\np")
  (let ((tabs (sort (rst-compute-tabs beg) (lambda (x y) (<= x y))))
	(leftmostcol (rst-find-leftmost-column beg end)))
    (when (or (> leftmostcol 0) (> cnt 0))
      ;; Apply the indent
      (indent-rigidly
       beg end
       (if (zerop cnt)
	   (- leftmostcol)
	 ;; Find the next tab after the leftmost column
	 (let* ((cmp (if (> cnt 0) '> '<))
		(tabs (if (> cnt 0) tabs (reverse tabs)))
		(len (length tabs))
		(dir (signum cnt)) ; Direction to take
		(abs (abs cnt)) ; Absolute number of steps to take
		;; Get the position of the first tab beyond leftmostcol
		(fnd (position-if (lambda (elt)
				    (funcall cmp elt leftmostcol))
				  tabs))
		;; Virtual position of tab
		(pos (+ (or fnd len) (1- abs)))
		(tab (if (< pos len)
			 ;; Tab exists - use it
			 (nth pos tabs)
		       ;; Column needs to be computed
		       (let ((col (+ (or (car (last tabs)) leftmostcol)
				     ;; Base on last known column
				     (* (- pos (1- len)) ; Distance left
					dir ; Direction to take
					rst-indent-width))))
			 (if (< col 0) 0 col)))))
	   (- tab leftmostcol)))))))

;; FIXME: A paragraph with an (incorrectly) indented second line is not filled
;; correctly::
;;
;;   Some start
;;     continued wrong
(defun rst-adaptive-fill ()
  "Return fill prefix found at point.
Value for `adaptive-fill-function'."
  (let ((fnd (if (looking-at adaptive-fill-regexp)
		 (match-string-no-properties 0))))
    (if (save-match-data
	  (not (string-match comment-start-skip fnd)))
	;; An non-comment prefix is fine
	fnd
      ;; Matches a comment - return whitespace instead
      (make-string (-
		    (save-excursion
		      (goto-char (match-end 0))
		      (current-column))
		    (save-excursion
		      (goto-char (match-beginning 0))
		      (current-column))) ? ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun rst-comment-line-break (&optional soft)
  "Break line and indent, continuing reStructuredText comment if within one.
Value for `comment-line-break-function'."
  (if soft
      (insert-and-inherit ?\n)
    (newline 1))
  (save-excursion
    (forward-char -1)
    (delete-horizontal-space))
  (delete-horizontal-space)
  (let ((tabs (rst-compute-tabs (point))))
    (when tabs
      (indent-line-to (car tabs)))))

(defun rst-comment-indent ()
  "Return indentation for current comment line."
  (car (rst-compute-tabs (point))))

(defun rst-comment-insert-comment ()
  "Insert a comment in the current line."
  (rst-indent-line 0)
  (insert comment-start))

(defun rst-comment-region (beg end &optional arg)
  "Comment the current region or uncomment it if ARG is \\[universal-argument]."
  (save-excursion
    (if (consp arg)
	(rst-uncomment-region beg end arg)
      (goto-char beg)
      (let ((ind (current-indentation))
	    bol)
	(forward-line 0)
	(setq bol (point))
	(indent-rigidly bol end rst-indent-comment)
	(goto-char bol)
	(open-line 1)
	(indent-line-to ind)
	(insert (comment-string-strip comment-start t t))))))

(defun rst-uncomment-region (beg end &optional arg)
  "Uncomment the current region.
ARG is ignored"
  (save-excursion
    (let (bol eol)
      (goto-char beg)
      (forward-line 0)
      (setq bol (point))
      (forward-line 1)
      (setq eol (point))
      (indent-rigidly eol end (- rst-indent-comment))
      (delete-region bol eol))))

;;------------------------------------------------------------------------------

;; FIXME: these next functions should become part of a larger effort to redo the
;; bullets in bulletted lists.  The enumerate would just be one of the possible
;; outputs.
;;
;; FIXME: We need to do the enumeration removal as well.

(defun rst-enumerate-region (beg end all)
  "Add enumeration to all the leftmost paragraphs in the given region.
The region is specified between BEG and END.  With ALL,
do all lines instead of just paragraphs."
  (interactive "r\nP")
  (let ((count 0)
	(last-insert-len nil))
    (rst-iterate-leftmost-paragraphs
     beg end (not all)
     (let ((ins-string (format "%d. " (incf count))))
       (setq last-insert-len (length ins-string))
       (insert ins-string))
     (insert (make-string last-insert-len ?\ ))
     )))

(defun rst-bullet-list-region (beg end all)
  "Add bullets to all the leftmost paragraphs in the given region.
The region is specified between BEG and END.  With ALL,
do all lines instead of just paragraphs."
  (interactive "r\nP")
  (rst-iterate-leftmost-paragraphs
   beg end (not all)
   (insert (car rst-preferred-bullets) " ")
   (insert "  ")
   ))

;; FIXME: Does not deal with a varying number of digits appropriately
;; FIXME: Does not deal with multiple levels independently
;; FIXME: Does not indent a multiline item correctly
(defun rst-convert-bullets-to-enumeration (beg end)
  "Convert the bulleted and enumerated items in the region to enumerated lists.
Renumber as necessary."
  (interactive "r")
  (let* (;; Find items and convert the positions to markers.
	 (items (mapcar
		 (lambda (x)
		   (cons (let ((m (make-marker)))
			   (set-marker m (car x))
			   m)
			 (cdr x)))
		 (rst-find-pfx-in-region beg end (rst-re 'itmany-sta-1))))
	 (count 1)
	 )
    (save-excursion
      (dolist (x items)
	(goto-char (car x))
	(looking-at (rst-re 'itmany-beg-1))
	(replace-match (format "%d." count) nil nil nil 1)
	(incf count)
	))
    ))



;;------------------------------------------------------------------------------

(defun rst-line-block-region (rbeg rend &optional pfxarg)
  "Toggle line block prefixes for a region.
With prefix argument set the empty lines too."
  (interactive "r\nP")
  (let ((comment-start "| ")
	(comment-end "")
	(comment-start-skip "| ")
	(comment-style 'indent)
	(force (not (not pfxarg))))
    (rst-iterate-leftmost-paragraphs-2
        (rbeg rend parbegin leftmost isleft isempty)
      (when (or force (not isempty))
        (move-to-column leftmost force)
        (delete-region (point) (+ (point) (- (current-indentation) leftmost)))
        (insert "| ")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock
;; =========

(require 'font-lock)

(defgroup rst-faces nil "Faces used in Rst Mode."
  :group 'rst
  :group 'faces
  :version "21.1")

(defcustom rst-block-face 'font-lock-keyword-face
  "All syntax marking up a special block."
  :group 'rst-faces
  :type '(face))

(defcustom rst-external-face 'font-lock-type-face
  "Field names and interpreted text."
  :group 'rst-faces
  :type '(face))

(defcustom rst-definition-face 'font-lock-function-name-face
  "All other defining constructs."
  :group 'rst-faces
  :type '(face))

(defcustom rst-directive-face
  ;; XEmacs compatibility
  (if (boundp 'font-lock-builtin-face)
      'font-lock-builtin-face
    'font-lock-preprocessor-face)
  "Directives and roles."
  :group 'rst-faces
  :type '(face))

(defcustom rst-comment-face 'font-lock-comment-face
  "Comments."
  :group 'rst-faces
  :type '(face))

(defcustom rst-emphasis1-face
  ;; XEmacs compatibility
  (if (facep 'italic)
      ''italic
    'italic)
  "Simple emphasis."
  :group 'rst-faces
  :type '(face))

(defcustom rst-emphasis2-face
  ;; XEmacs compatibility
  (if (facep 'bold)
      ''bold
    'bold)
  "Double emphasis."
  :group 'rst-faces
  :type '(face))

(defcustom rst-literal-face 'font-lock-string-face
  "Literal text."
  :group 'rst-faces
  :type '(face))

(defcustom rst-reference-face 'font-lock-variable-name-face
  "References to a definition."
  :group 'rst-faces
  :type '(face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup rst-faces-defaults nil
  "Values used to generate default faces for section titles on all levels.
Tweak these if you are content with how section title faces are built in
general but you do not like the details."
  :group 'rst-faces
  :version "21.1")

(defun rst-set-level-default (sym val)
  "Set custom var SYM affecting section title text face and recompute the faces."
  (custom-set-default sym val)
  ;; Also defines the faces initially when all values are available
  (and (boundp 'rst-level-face-max)
       (boundp 'rst-level-face-format-light)
       (boundp 'rst-level-face-base-color)
       (boundp 'rst-level-face-step-light)
       (boundp 'rst-level-face-base-light)
       (fboundp 'rst-define-level-faces)
       (rst-define-level-faces)))

;; Faces for displaying items on several levels; these definitions define
;; different shades of grey where the lightest one (i.e. least contrasting) is
;; used for level 1
(defcustom rst-level-face-max 6
  "Maximum depth of levels for which section title faces are defined."
  :group 'rst-faces-defaults
  :type '(integer)
  :set 'rst-set-level-default)
(defcustom rst-level-face-base-color "grey"
  "Base name of the color for creating background colors in section title faces."
  :group 'rst-faces-defaults
  :type '(string)
  :set 'rst-set-level-default)
(defcustom rst-level-face-base-light
  (if (eq frame-background-mode 'dark)
      15
    85)
  "The lightness factor for the base color.  This value is used for level 1.
The default depends on whether the value of `frame-background-mode' is
`dark' or not."
  :group 'rst-faces-defaults
  :type '(integer)
  :set 'rst-set-level-default)
(defcustom rst-level-face-format-light "%2d"
  "The format for the lightness factor appended to the base name of the color.
This value is expanded by `format' with an integer."
  :group 'rst-faces-defaults
  :type '(string)
  :set 'rst-set-level-default)
(defcustom rst-level-face-step-light
  (if (eq frame-background-mode 'dark)
      7
    -7)
  "The step width to use for the next color.
The formula

    `rst-level-face-base-light'
    + (`rst-level-face-max' - 1) * `rst-level-face-step-light'

must result in a color level which appended to `rst-level-face-base-color'
using `rst-level-face-format-light' results in a valid color such as `grey50'.
This color is used as background for section title text on level
`rst-level-face-max'."
  :group 'rst-faces-defaults
  :type '(integer)
  :set 'rst-set-level-default)

(defcustom rst-adornment-faces-alist
  (let ((alist '((t . font-lock-keyword-face)
		 (nil . font-lock-keyword-face)))
	(i 1))
    (while (<= i rst-level-face-max)
      (nconc alist (list (cons i (intern (format "rst-level-%d-face" i)))))
      (setq i (1+ i)))
    alist)
  "Faces for the various adornment types.
Key is a number (for the section title text of that level),
t (for transitions) or nil (for section title adornment).
If you generally do not like how section title text faces are
set up tweak here.  If the general idea is ok for you but you do not like the
details check the Rst Faces Defaults group."
  :group 'rst-faces
  :type '(alist
	  :key-type
	  (choice
	   (integer
	    :tag
	    "Section level (may not be bigger than `rst-level-face-max')")
	   (boolean :tag "transitions (on) / section title adornment (off)"))
	  :value-type (face))
  :set-after '(rst-level-face-max))

;; FIXME: It should be possible to give "#RRGGBB" type of color values
(defun rst-define-level-faces ()
  "Define the faces for the section title text faces from the values."
  ;; All variables used here must be checked in `rst-set-level-default'
  (let ((i 1))
    (while (<= i rst-level-face-max)
      (let ((sym (intern (format "rst-level-%d-face" i)))
	    (doc (format "Face for showing section title text at level %d" i))
	    (col (format (concat "%s" rst-level-face-format-light)
			 rst-level-face-base-color
			 (+ (* (1- i) rst-level-face-step-light)
			    rst-level-face-base-light))))
	(make-empty-face sym)
	(set-face-doc-string sym doc)
	(set-face-background sym col)
	(set sym sym)
	(setq i (1+ i))))))

(rst-define-level-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rst-font-lock-keywords
  ;; The reST-links in the comments below all relate to sections in
  ;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
  `(;; FIXME: Block markup is not recognized in blocks after explicit markup
    ;; start

    ;; Simple `Body Elements`_
    ;; `Bullet Lists`_
    ;; FIXME: A bullet directly after a field name is not recognized
    (,(rst-re 'lin-beg '(:grp bul-sta))
     1 ,rst-block-face)
    ;; `Enumerated Lists`_
    (,(rst-re 'lin-beg '(:grp enmany-sta))
     1 ,rst-block-face)
    ;; `Definition Lists`_ FIXME: missing
    ;; `Field Lists`_
    (,(rst-re 'lin-beg '(:grp fld-tag) 'bli-sfx)
     1 ,rst-external-face)
    ;; `Option Lists`_
    (,(rst-re 'lin-beg '(:grp opt-tag (:shy optsep-tag opt-tag) "*")
	      '(:alt "$" (:seq hws-prt "\\{2\\}")))
     1 ,rst-block-face)
    ;; `Line Blocks`_
    ;; Only for lines containing no more bar - to distinguish from tables
    (,(rst-re 'lin-beg '(:grp "|" bli-sfx) "[^|\n]*$")
     1 ,rst-block-face)

    ;; `Tables`_ FIXME: missing

    ;; All the `Explicit Markup Blocks`_
    ;; `Footnotes`_ / `Citations`_
    (,(rst-re 'lin-beg 'fnc-sta-2)
     (1 ,rst-definition-face)
     (2 ,rst-definition-face))
    ;; `Directives`_ / `Substitution Definitions`_
    (,(rst-re 'lin-beg 'dir-sta-3)
     (1 ,rst-directive-face)
     (2 ,rst-definition-face)
     (3 ,rst-directive-face))
    ;; `Hyperlink Targets`_
    (,(rst-re 'lin-beg
	      '(:grp exm-sta "_" (:alt
				  (:seq "`" ilcbkqdef-tag "`")
				  (:seq (:alt "[^:\\\n]" "\\\\.") "+")) ":")
	      'bli-sfx)
     1 ,rst-definition-face)
    (,(rst-re 'lin-beg '(:grp "__") 'bli-sfx)
     1 ,rst-definition-face)

    ;; All `Inline Markup`_ - most of them may be multiline though this is
    ;; uninteresting

    ;; FIXME: Condition 5 preventing fontification of e.g. "*" not implemented
    ;; `Strong Emphasis`_
    (,(rst-re 'ilm-pfx '(:grp "\\*\\*" ilcast-tag "\\*\\*") 'ilm-sfx)
     1 ,rst-emphasis2-face)
    ;; `Emphasis`_
    (,(rst-re 'ilm-pfx '(:grp "\\*" ilcast-tag "\\*") 'ilm-sfx)
     1 ,rst-emphasis1-face)
    ;; `Inline Literals`_
    (,(rst-re 'ilm-pfx '(:grp "``" ilcbkq-tag "``") 'ilm-sfx)
     1 ,rst-literal-face)
    ;; `Inline Internal Targets`_
    (,(rst-re 'ilm-pfx '(:grp "_`" ilcbkq-tag "`") 'ilm-sfx)
     1 ,rst-definition-face)
    ;; `Hyperlink References`_
    ;; FIXME: `Embedded URIs`_ not considered
    ;; FIXME: Directly adjacing marked up words are not fontified correctly
    ;;        unless they are not separated by two spaces: foo_ bar_
    (,(rst-re 'ilm-pfx '(:grp (:alt (:seq "`" ilcbkq-tag "`")
				    (:seq "\\sw" (:alt "\\sw" "-") "+\\sw"))
			      "__?") 'ilm-sfx)
     1 ,rst-reference-face)
    ;; `Interpreted Text`_
    (,(rst-re 'ilm-pfx '(:grp (:shy ":" sym-tag ":") "?")
	      '(:grp "`" ilcbkq-tag "`")
	      '(:grp (:shy ":" sym-tag ":") "?") 'ilm-sfx)
     (1 ,rst-directive-face)
     (2 ,rst-external-face)
     (3 ,rst-directive-face))
    ;; `Footnote References`_ / `Citation References`_
    (,(rst-re 'ilm-pfx '(:grp fnc-tag "_") 'ilm-sfx)
     1 ,rst-reference-face)
    ;; `Substitution References`_
    ;; FIXME: References substitutions like |this|_ or |this|__ are not
    ;;        fontified correctly
    (,(rst-re 'ilm-pfx '(:grp sub-tag) 'ilm-sfx)
     1 ,rst-reference-face)
    ;; `Standalone Hyperlinks`_
    ;; FIXME: This takes it easy by using a whitespace as delimiter
    (,(rst-re 'ilm-pfx '(:grp uri-tag ":\\S +") 'ilm-sfx)
     1 ,rst-definition-face)
    (,(rst-re 'ilm-pfx '(:grp sym-tag "@" sym-tag ) 'ilm-sfx)
     1 ,rst-definition-face)

    ;; Do all block fontification as late as possible so 'append works

    ;; Sections_ / Transitions_ - for sections this is multiline
    (,(rst-re 'ado-beg-2-1)
     (rst-font-lock-handle-adornment-matcher
      (rst-font-lock-handle-adornment-pre-match-form
       (match-string-no-properties 1) (match-end 1))
      nil
      (1 (cdr (assoc nil rst-adornment-faces-alist)) append t)
      (2 (cdr (assoc rst-font-lock-adornment-level
		     rst-adornment-faces-alist)) append t)
      (3 (cdr (assoc nil rst-adornment-faces-alist)) append t)))

    ;; FIXME: FACESPEC could be used instead of ordinary faces to set
    ;;        properties on comments and literal blocks so they are *not*
    ;;        inline fontified; see (elisp)Search-based Fontification

    ;; `Comments`_ - this is multiline
    (,(rst-re 'lin-beg 'cmt-sta-1)
     (1 ,rst-comment-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit (match-end 1))
      nil
      (0 ,rst-comment-face append)))
    (,(rst-re 'lin-beg '(:grp exm-tag) '(:grp hws-tag) "$")
     (1 ,rst-comment-face)
     (2 ,rst-comment-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit 'next)
      nil
      (0 ,rst-comment-face append)))

    ;; FIXME: This is not rendered as comment::
    ;; .. .. list-table::
    ;;       :stub-columns: 1
    ;;       :header-rows: 1

    ;; FIXME: This is rendered wrong::
    ;;
    ;; 	 xxx yyy::
    ;;
    ;; 	 			----|> KKKKK <|----
    ;; 	 		       /	     	    \
    ;; 	    -|> AAAAAAAAAAPPPPPP <|-   	       	 -|> AAAAAAAAAABBBBBBB <|-
    ;; 	    |			   |	     	 |     	       	       	 |
    ;; 	    |			   |		 |			 |
    ;; 	    PPPPPP     PPPPPPDDDDDDD             BBBBBBB     PPPPPPBBBBBBB
    ;;
    ;; Indentation needs to be taken from the line with the ``::`` and not from
    ;; the first content line.

    ;; `Indented Literal Blocks`_ - this is multiline
    (,(rst-re 'lin-beg 'lit-sta-2)
     (2 ,rst-block-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit t)
      nil
      (0 ,rst-literal-face append)))

    ;; FIXME: `Quoted Literal Blocks`_ missing - this is multiline

    ;; `Doctest Blocks`_
    ;; FIXME: This is wrong according to the specification:
    ;;
    ;;   Doctest blocks are text blocks which begin with ">>> ", the Python
    ;;   interactive interpreter main prompt, and end with a blank line.
    ;;   Doctest blocks are treated as a special case of literal blocks,
    ;;   without requiring the literal block syntax. If both are present, the
    ;;   literal block syntax takes priority over Doctest block syntax:
    ;;
    ;;   This is an ordinary paragraph.
    ;;
    ;;   >>> print 'this is a Doctest block'
    ;;   this is a Doctest block
    ;;
    ;;   The following is a literal block::
    ;;
    ;;       >>> This is not recognized as a doctest block by
    ;;       reStructuredText.  It *will* be recognized by the doctest
    ;;       module, though!
    ;;
    ;;   Indentation is not required for doctest blocks.
    (,(rst-re 'lin-beg '(:grp (:alt ">>>" ell-tag)) '(:grp ".+"))
     (1,rst-block-face)
     (2 ,rst-literal-face))
    )
  "Keywords to highlight in rst mode.")

(defun rst-font-lock-extend-region ()
  "Extend the region `font-lock-beg' / `font-lock-end' iff it may
be in the middle of a multiline construct and return non-nil if so."
  (let ((r (rst-font-lock-extend-region-internal font-lock-beg font-lock-end)))
    (when r
      (setq font-lock-beg (car r))
      (setq font-lock-end (cdr r))
      t)))

(defun rst-font-lock-extend-region-internal (beg end)
  "Check the region BEG / END for being in the middle of a multiline construct.
Return nil if not or a cons with new values for BEG / END"
  ;; There are many potential multiline constructs but really relevant ones are
  ;; comment lines without leading explicit markup tag and literal blocks
  ;; following "::" which are both indented. Thus indendation is what is
  ;; recognized here. The second criteria is an explicit markup tag which may
  ;; be a comment or a double colon at the end of a line.
  (if (not (get-text-property beg 'font-lock-multiline))
      ;; Move only if we don't start inside a multiline construct already
      (save-excursion
	(let (;; non-empty non-indented line, explicit markup tag or literal
	      ;; block tag
	      (stop-re (rst-re '(:alt "[^ \t\n]"
				      (:seq hws-tag exm-tag)
				      (:seq ".*" dcl-tag lin-end)))))
	  (goto-char beg)
	  (forward-line 0)
	  (while (and (not (looking-at stop-re))
		      (zerop (forward-line -1)))) ; try previous line if exists
	  ;; FIXME: Extending the end should also be done
	  (if (not (= (point) beg))
	      (cons (point) end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indented blocks

(defun rst-forward-indented-block (&optional column limit)
  "Move forward across one indented block.
Find the next non-empty line which is not indented at least to COLUMN (defaults
to the column of the point).  Moves point to first character of this line or the
first empty line immediately before it and returns that position.  If there is
no such line before LIMIT (defaults to the end of the buffer) returns nil and
point is not moved."
  (interactive)
  (let ((clm (or column (current-column)))
	(start (point))
	fnd beg cand)
    (if (not limit)
	(setq limit (point-max)))
    (save-match-data
      (while (and (not fnd) (< (point) limit))
	(forward-line 1)
	(when (< (point) limit)
	  (setq beg (point))
	  (if (looking-at (rst-re 'lin-end))
	      (setq cand (or cand beg)) ; An empty line is a candidate
	    (move-to-column clm)
	    ;; FIXME: No indentation [(zerop clm)] must be handled in some
	    ;; useful way - though it is not clear what this should mean at all
	    (if (string-match
		 (rst-re 'linemp-tag)
		 (buffer-substring-no-properties beg (point)))
		(setq cand nil) ; An indented line resets a candidate
	      (setq fnd (or cand beg)))))))
    (goto-char (or fnd start))
    fnd))

(defvar rst-font-lock-find-unindented-line-begin nil
  "Beginning of the match if `rst-font-lock-find-unindented-line-end'")

(defvar rst-font-lock-find-unindented-line-end nil
  "End of the match as determined by `rst-font-lock-find-unindented-line-limit'.
Also used as a trigger for
`rst-font-lock-find-unindented-line-match'.")

(defun rst-font-lock-find-unindented-line-limit (ind-pnt)
  "Find the next unindented line relative to indenation at IND-PNT.
Return this point, the end of the buffer or nil if nothing found.
If IND-PNT is `next' take the indentation from the next line if
this is not empty and indented more than the current one. If
IND-PNT is non-nil but not a number take the indentation from the
next non-empty line if this is indented more than the current
one."
  (setq rst-font-lock-find-unindented-line-begin ind-pnt)
  (setq rst-font-lock-find-unindented-line-end
	(save-excursion
	  (when (not (numberp ind-pnt))
	    ;; Find indentation point in next line if any
	    (setq ind-pnt
		  ;; FIXME: Should be refactored to two different functions
		  ;;        giving their result to this function, may be
		  ;;        integrated in caller
		  (save-match-data
		    (let ((cur-ind (current-indentation)))
		      (if (eq ind-pnt 'next)
			  (when (and (zerop (forward-line 1))
				     (< (point) (point-max)))
			    ;; Not at EOF
			    (setq rst-font-lock-find-unindented-line-begin
				  (point))
			    (when (and (not (looking-at (rst-re 'lin-end)))
				       (> (current-indentation) cur-ind))
			        ;; Use end of indentation if non-empty line
				(looking-at (rst-re 'hws-tag))
				(match-end 0)))
			;; Skip until non-empty line or EOF
			(while (and (zerop (forward-line 1))
				    (< (point) (point-max))
				    (looking-at (rst-re 'lin-end))))
			(when (< (point) (point-max))
			  ;; Not at EOF
			  (setq rst-font-lock-find-unindented-line-begin
				(point))
			  (when (> (current-indentation) cur-ind)
			    ;; Indentation bigger than line of departure
			    (looking-at (rst-re 'hws-tag))
			    (match-end 0))))))))
	  (when ind-pnt
	    (goto-char ind-pnt)
	    (or (rst-forward-indented-block nil (point-max))
		(point-max))))))

(defun rst-font-lock-find-unindented-line-match (limit)
  "Set the match found by
`rst-font-lock-find-unindented-line-limit' the first time called
or nil."
  (when rst-font-lock-find-unindented-line-end
    (set-match-data
     (list rst-font-lock-find-unindented-line-begin
	   rst-font-lock-find-unindented-line-end))
    (put-text-property rst-font-lock-find-unindented-line-begin
		       rst-font-lock-find-unindented-line-end
		       'font-lock-multiline t)
    ;; Make sure this is called only once
    (setq rst-font-lock-find-unindented-line-end nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adornments

(defvar rst-font-lock-adornment-level nil
  "Storage for `rst-font-lock-handle-adornment-matcher'.
Either section level of the current adornment or t for a transition.")

(defun rst-adornment-level (key)
  "Return section level for adornment KEY.
KEY is the first element of the return list of
`rst-classify-adornment'. If KEY is not a cons return it. If KEY is found
in the hierarchy return its level. Otherwise return a level one
beyond the existing hierarchy."
  (if (not (consp key))
      key
    (let* ((hier (rst-get-hierarchy))
	   (char (car key))
	   (style (cdr key)))
      (1+ (or (position-if (lambda (elt)
			     (and (equal (car elt) char)
				  (equal (cadr elt) style))) hier)
	      (length hier))))))

(defvar rst-font-lock-adornment-match nil
  "Storage for match for current adornment.
Set by `rst-font-lock-handle-adornment-pre-match-form'. Also used
as a trigger for `rst-font-lock-handle-adornment-matcher'.")

(defun rst-font-lock-handle-adornment-pre-match-form (ado ado-end)
  "Determine limit for adornments for font-locking section titles and transitions.
In fact determine all things necessary and put the result to
`rst-font-lock-adornment-match' and
`rst-font-lock-adornment-level'. ADO is the complete adornment
matched. ADO-END is the point where ADO ends. Return the point
where the whole adorned construct ends.

Called as a PRE-MATCH-FORM in the sense of `font-lock-keywords'."
  (let ((ado-data (rst-classify-adornment ado ado-end)))
    (if (not ado-data)
	(setq rst-font-lock-adornment-level nil
	      rst-font-lock-adornment-match nil)
      (setq rst-font-lock-adornment-level
	    (rst-adornment-level (car ado-data)))
      (setq rst-font-lock-adornment-match (cdr ado-data))
      (goto-char (nth 1 ado-data)) ; Beginning of construct
      (nth 2 ado-data)))) ; End of construct

(defun rst-font-lock-handle-adornment-matcher (limit)
  "Set the match found by `rst-font-lock-handle-adornment-pre-match-form'
the first time called or nil.

Called as a MATCHER in the sense of `font-lock-keywords'."
  (let ((match rst-font-lock-adornment-match))
    ;; May run only once - enforce this
    (setq rst-font-lock-adornment-match nil)
    (when match
      (set-match-data match)
      (goto-char (match-end 0))
      (put-text-property (match-beginning 0) (match-end 0)
			 'font-lock-multiline t)
      t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(defgroup rst-compile nil
  "Settings for support of conversion of reStructuredText
document with \\[rst-compile]."
  :group 'rst
  :version "21.1")

;; FIXME: Should be `defcustom`
(defvar rst-compile-toolsets
  '((html . ("rst2html.py" ".html" nil))
    (latex . ("rst2latex.py" ".tex" nil))
    (newlatex . ("rst2newlatex.py" ".tex" nil))
    (pseudoxml . ("rst2pseudoxml.py" ".xml" nil))
    (xml . ("rst2xml.py" ".xml" nil))
    (pdf . ("rst2pdf.py" ".pdf" nil))
    (s5 . ("rst2s5.py" ".xml" nil)))
  "Table describing the command to use for each toolset.
An association list of the toolset to a list of the (command to use,
extension of produced filename, options to the tool (nil or a
string)) to be used for converting the document.")

;; FIXME: Should be `defcustom`
(defvar rst-compile-primary-toolset 'html
  "The default toolset for `rst-compile'.")

;; FIXME: Should be `defcustom`
(defvar rst-compile-secondary-toolset 'latex
  "The default toolset for `rst-compile' with a prefix argument.")

(defun rst-compile-find-conf ()
  "Look for the configuration file in the parents of the current path."
  (interactive)
  (let ((file-name "docutils.conf")
        (buffer-file (buffer-file-name)))
    ;; Move up in the dir hierarchy till we find a change log file.
    (let* ((dir (file-name-directory buffer-file))
	   (prevdir nil))
      (while (and (or (not (string= dir prevdir))
		      (setq dir nil)
		      nil)
                  (not (file-exists-p (concat dir file-name))))
        ;; Move up to the parent dir and try again.
	(setq prevdir dir)
        (setq dir (expand-file-name (file-name-directory
                                     (directory-file-name
				      (file-name-directory dir)))))
	)
      (or (and dir (concat dir file-name)) nil)
    )))


(require 'compile)

(defun rst-compile (&optional use-alt)
  "Compile command to convert reST document into some output file.
Attempts to find configuration file, if it can, overrides the
options.  There are two commands to choose from, with USE-ALT,
select the alternative toolset."
  (interactive "P")
  ;; Note: maybe we want to check if there is a Makefile too and not do anything
  ;; if that is the case.  I dunno.
  (let* ((toolset (cdr (assq (if use-alt
				 rst-compile-secondary-toolset
			       rst-compile-primary-toolset)
			rst-compile-toolsets)))
         (command (car toolset))
         (extension (cadr toolset))
         (options (caddr toolset))
         (conffile (rst-compile-find-conf))
         (bufname (file-name-nondirectory buffer-file-name))
         (outname (file-name-sans-extension bufname)))

    ;; Set compile-command before invocation of compile.
    (set (make-local-variable 'compile-command)
         (mapconcat 'identity
                    (list command
                          (or options "")
                          (if conffile
                              (concat "--config=" (shell-quote-argument conffile))
                            "")
                          (shell-quote-argument bufname)
                          (shell-quote-argument (concat outname extension)))
                    " "))

    ;; Invoke the compile command.
    (if (or compilation-read-command use-alt)
        (call-interactively 'compile)
      (compile compile-command))
    ))

(defun rst-compile-alt-toolset ()
  "Compile command with the alternative toolset."
  (interactive)
  (rst-compile t))

(defun rst-compile-pseudo-region ()
  "Show the pseudo-XML rendering of the current active region,
or of the entire buffer, if the region is not selected."
  (interactive)
  (with-output-to-temp-buffer "*pseudoxml*"
    (shell-command-on-region
     (if mark-active (region-beginning) (point-min))
     (if mark-active (region-end) (point-max))
     (cadr (assq 'pseudoxml rst-compile-toolsets))
     standard-output)))

;; FIXME: Should be `defcustom`
(defvar rst-pdf-program "xpdf"
  "Program used to preview PDF files.")

(defun rst-compile-pdf-preview ()
  "Convert the document to a PDF file and launch a preview program."
  (interactive)
  (let* ((tmp-filename (make-temp-file "rst_el" nil ".pdf"))
	 (command (format "%s %s %s && %s %s ; rm %s"
			  (cadr (assq 'pdf rst-compile-toolsets))
			  buffer-file-name tmp-filename
			  rst-pdf-program tmp-filename tmp-filename)))
    (start-process-shell-command "rst-pdf-preview" nil command)
    ;; Note: you could also use (compile command) to view the compilation
    ;; output.
    ))

;; FIXME: Should be `defcustom` or use something like `browse-url`
(defvar rst-slides-program "firefox"
  "Program used to preview S5 slides.")

(defun rst-compile-slides-preview ()
  "Convert the document to an S5 slide presentation and launch a preview program."
  (interactive)
  (let* ((tmp-filename (make-temp-file "rst_el" nil ".html"))
	 (command (format "%s %s %s && %s %s ; rm %s"
			  (cadr (assq 's5 rst-compile-toolsets))
			  buffer-file-name tmp-filename
			  rst-slides-program tmp-filename tmp-filename)))
    (start-process-shell-command "rst-slides-preview" nil command)
    ;; Note: you could also use (compile command) to view the compilation
    ;; output.
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic text functions that are more convenient than the defaults.

;; FIXME: Unbound command - should be bound or removed
(defun rst-replace-lines (fromchar tochar)
  "Replace flush-left lines, consisting of multiple FROMCHAR characters,
with equal-length lines of TOCHAR."
  (interactive "\
cSearch for flush-left lines of char:
cand replace with char: ")
  (save-excursion
    (let ((searchre (rst-re "^" fromchar "+\\( *\\)$"))
          (found 0))
      (while (search-forward-regexp searchre nil t)
        (setq found (1+ found))
        (goto-char (match-beginning 1))
        (let ((width (current-column)))
          (rst-delete-entire-line)
          (insert-char tochar width)))
      (message (format "%d lines replaced." found)))))

;; FIXME: Unbound command - should be bound or removed
(defun rst-join-paragraph ()
  "Join lines in current paragraph into one line, removing end-of-lines."
  (interactive)
  (let ((fill-column 65000)) ; some big number
    (call-interactively 'fill-paragraph)))

;; FIXME: Unbound command - should be bound or removed
(defun rst-force-fill-paragraph ()
  "Fill paragraph at point, first joining the paragraph's lines into one.
This is useful for filling list item paragraphs."
  (interactive)
  (rst-join-paragraph)
  (fill-paragraph nil))


;; FIXME: Unbound command - should be bound or removed
;; Generic character repeater function.
;; For sections, better to use the specialized function above, but this can
;; be useful for creating separators.
(defun rst-repeat-last-character (use-next)
  "Fill the current line up to the length of the preceding line (if not
empty), using the last character on the current line.  If the preceding line is
empty, we use the `fill-column'.

If USE-NEXT, use the next line rather than the preceding line.

If the current line is longer than the desired length, shave the characters off
the current line to fit the desired length.

As an added convenience, if the command is repeated immediately, the alternative
column is used (fill-column vs. end of previous/next line)."
  (interactive "P")
  (let* ((curcol (current-column))
         (curline (+ (count-lines (point-min) (point))
                     (if (zerop curcol) 1 0)))
         (lbp (line-beginning-position 0))
         (prevcol (if (and (= curline 1) (not use-next))
                      fill-column
                    (save-excursion
                      (forward-line (if use-next 1 -1))
                      (end-of-line)
                      (skip-chars-backward " \t" lbp)
                      (let ((cc (current-column)))
                        (if (zerop cc) fill-column cc)))))
         (rightmost-column
          (cond ((equal last-command 'rst-repeat-last-character)
                 (if (= curcol fill-column) prevcol fill-column))
                (t (save-excursion
                     (if (zerop prevcol) fill-column prevcol)))
                )) )
    (end-of-line)
    (if (> (current-column) rightmost-column)
        ;; shave characters off the end
        (delete-region (- (point)
                          (- (current-column) rightmost-column))
                       (point))
      ;; fill with last characters
      (insert-char (preceding-char)
                   (- rightmost-column (current-column))))
    ))


(defun rst-portable-mark-active-p ()
  "A portable function that returns non-nil if the mark is active."
  (cond
   ((fboundp 'region-active-p) (region-active-p))
   ((boundp 'transient-mark-mode) transient-mark-mode mark-active)))



(provide 'rst)
;;; rst.el ends here
