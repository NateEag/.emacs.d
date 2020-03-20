;;; vcard-parse.el --- Library for parsing vCards      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a consumer-agnostic parser for vCard files,
;; aka Virtual Contact Files.  Its entry points parse a file or buffer
;; containing one or more contacts in vCard format, and return the
;; data as a structure meant for use by other programs.  It can parse
;; versions 2.1, 3.0, and 4.0 of the vCard standard, RFC 6350 (see
;; https://tools.ietf.org/html/rfc6350).

;; Parsed vCards are returned as lists containing contact properties.
;; Each property is a list containing the property name, downcased and
;; interned as a symbol, the property value, cast to the most
;; appropriate type, and a further alist of property parameters,
;; values also cast to type where applicable.  For example, this email
;; property:

;; EMAIL;TYPE=work:simon.perreault@viagenie.ca

;; Will be parsed into:

;; (email "simon.perreault@viagenie.ca" ((type . "work")))

;; A contact is a structure containing a list of properties.  As much
;; as possible, the internal implementation of the structure should be
;; ignored, and the properties of a single contact accessed only
;; through the provided getters.  The getters are:

;; `vcard-contact-properties': Return a list of all properties.

;; `vcard-contact-property-types': Return a list of all the different
;; property types this contact has, as symbols.

;; `vcard-contact-property-type': Return all properties of the given
;; type, for this contact.  The return value, if non-nil, is either a
;; single property, or a list of (possibly just one) properties,
;; depending on the cardinality of the property type (see the RFC).

;; `vcard-contact-property-groups': Return a list of all the property
;; groups for the given contact.  A single property's group is found
;; under the 'group key in its parameter list.

;; `vcard-contact-property-group': Return all the properties of the
;; given group, for this contact, or nil.

;; For reference, these are the property types specified for vCard
;; version 4.0:

;; "SOURCE" "KIND" "FN" "N" "NICKNAME" "PHOTO" "BDAY" "ANNIVERSARY"
;; "GENDER" "ADR" "TEL" "EMAIL" "IMPP" "LANG" "TZ" "GEO" "TITLE"
;; "ROLE" "LOGO" "ORG" "MEMBER" "RELATED" "CATEGORIES" "NOTE" "PRODID"
;; "REV" "SOUND" "UID" "CLIENTPIDMAP" "URL" "KEY" "FBURL" "CALADRURI"
;; "CALURI" "XML" iana-token x-name

;; Value types:

;; Booleans, integers, and floats are all cast as expected.  If
;; `vcard-parse-datetime-values' is non-nil, the code will do the best
;; it can to turn a datetime value into a list of integers a-la
;; `parse-time-string'.  This is done either with the built-in
;; `iso8601' library that exists in newer Emacs, or with a local copy
;; that ships with this package, if the built-in version isn't found.

;; While different vCard versions provide slightly different options,
;; the parsing process attempts to normalize property values as much
;; as possible.  Version 4.0 might have more properties available (the
;; KIND property, for instance), but for the most part the parsed data
;; will look the same.

;; TODO:

;; - Go the other direction: produce vCard files from structures.

;;; Code:

(require 'cl-lib)
(require 'iso8601)

(defvar vcard-parse-select-fields nil
  "A list of field types to select.
If this variable is non-nil, only the fields listed will be
parsed, all others will be discarded.  Note that the 'version and
'fn properties are always returned.

Most useful when let-bound around one of the parsing functions.")

(defvar vcard-parse-omit-fields nil
  "A list of field types to omit.
If this variable is non-nil, the fields listed will be discarded.

Most useful when let-bound around one of the parsing functions.")

(defvar vcard-parse-datetime-values t
  "When non-nil, attempt to parse date/time property values.
If successful, the property value will be (usually) converted to
a list of integers, though if the \"type\" parameter of the
property is \"text\", the value will be returned as a string.  It
is also possible that parsing may fail, in which case the
original string value will also be returned.")

(defvar vcard-parse-card-consumer-function nil
  "Custom function for consuming a single contact card.
It is called with a list of properties, as produced by the
built-in code, or by the return value of
`vcard-parse-property-consumer-function'.")

(defvar vcard-parse-property-consumer-function nil
  "Custom function for consuming a single property.
The function is called with four arguments: the property type as
a symbol, the property value (all un-escaping, decoding,
splitting, etc already complete), the property parameters as an
alist with symbol keys, and the vcard version as a float.")

(defvar vcard-parse-overriding-version nil
  "vCard version, as a float, used when no VERSION property is present.
vCard versions are sometimes specified outside of the cards
themselves -- as part of the file media type, for instance.  In
these cases, this variable can be let-bound around the parsing
process to specify the version.

If a card contains its own VERSION property, that property value
cannot be overridden.")

(defvar vcard-compound-properties '(n adr gender org)
  "A list of vcard properties with multi-part values.
Properties are symbols.  Values have several parts, separated by
semicolons.")

(defvar vcard-datetime-properties '(bday anniversary rev)
  "A list of vcard properties representing date or time values.
The parsing process will make some attempt at converting these
values into lisp time values, depending on the value of
`vcard-parse-datetime-values'.")

;;;###autoload
(defun vcard-parse-file (file)
  "Parse FILE containing vCard data into an alist."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (vcard-parse-buffer)))

;;;###autoload
(defun vcard-parse-buffer ()
  "Parse current buffer, containing vCard data.
Returns a list of contact objects."
  (interactive)
  (let ((card-consumer (when (functionp vcard-parse-card-consumer-function)
			 vcard-parse-card-consumer-function))
	(prop-consumer (if (functionp vcard-parse-property-consumer-function)
			   vcard-parse-property-consumer-function
			 #'list))
	(warning-series t)
	card out)
    ;; vCard 4.0 files *must* be utf-8 encoded + CRLF.  But we're only
    ;; parsing this file, we're not responsible for how it's saved to
    ;; disk.  Don't enable this for now.

    ;; (when (and (null (eq buffer-file-coding-system 'utf-8-unix))
    ;; 	       (or (eql
    ;; 		    vard-parse-overriding-version 4.0)
    ;; 		   (save-excursion
    ;; 		     (re-search-forward "VERSION:4\\.0" (point-max) t))))
    ;;   (set-buffer-file-coding-system 'utf-8-unix))
    (goto-char (point-min))
    ;; Unfolding consists of removing any instances of
    ;; newline-plus-space-or-horizontal-tab.  Technically there should
    ;; always be a non-space character following the space, but we
    ;; don't really care.

    ;; From the RFC:

    ;; Note: It is possible for very simple implementations to
    ;; generate improperly folded lines in the middle of a UTF-8
    ;; multi-octet sequence.  For this reason, implementations SHOULD
    ;; unfold lines in such a way as to properly restore the original
    ;; sequence.

    ;; How would we do that?  We could operate on
    ;; `find-file-literally', but then what?

    ;; CR = \015
    ;; LF = \012
    ;; SPC = \040
    ;; TAB = \011
    (while (re-search-forward "\n[ \t]" (point-max) t)
      (replace-match ""))

    (goto-char (point-min))

    ;; This routine assumes no blank lines in the whole file, which is
    ;; the way it's supposed to be, but we could be a little kinder
    ;; with a `skip-syntax-forward' check.

    (while (re-search-forward "^BEGIN:VCARD\n" (line-end-position 2) t)
      (when (setq card (condition-case nil
			   ;; `vcard-parse-card' moves point past the
			   ;; card.
			   (vcard-parse-card prop-consumer card-consumer)
			 (error (lwarn
				   '(vcard) :error
				   "Parsing failed with:\n %s"
				   (buffer-substring-no-properties
				    (point-at-bol)
				    (point-at-eol))))))
	(push card out)))

    (nreverse out)))

(defun vcard-parse-card (&optional prop-consumer card-consumer)
  "Collect properties from a single vCard and return them as an alist.
Point is at bol on the first property.  Collect properties until
the \"END:VCARD\" tag is reached, then move past that tag.

PROP-CONSUMER, if given, should be a function accepting three
arguments -- a property symbol, property value list, and property
parameter list -- and returning a property object.  CARD-CONSUMER
should be a function accepting one argument -- a list of
properties -- and returning a card/contact object."
  (let ((prop-consumer (or prop-consumer #'list))
	(version
	 ;; First line should be the VERSION property.
	 (or (when (re-search-forward
		    "VERSION:\\([[:digit:].]+\\)\n"
		    (line-end-position 2) t)
	       (string-to-number (match-string 1)))
	     vcard-parse-overriding-version
	     (error "Can't determine vCard version")))
	card)
    (push (list 'version version) card)
    (while (and (null (looking-at-p "^END:VCARD$"))
		(re-search-forward
		 "^\\(?:\\(?1:[-[:alnum:]]+\\)\\.\\)?\\(?2:[-[:alnum:]]+\\)"
		 (line-end-position) t))
      (let ((prop (intern (downcase (match-string 2))))
	    anchor sep params value)
	(when (or (eql prop 'fn)
		  (and (or (null vcard-parse-omit-fields)
			   (null (memql prop vcard-parse-omit-fields)))
		       (or (null vcard-parse-select-fields)
			   (memql prop vcard-parse-select-fields))))
	  ;; Pick up the group.
	  (when-let ((group (match-string-no-properties 1)))
	    (push (cons 'group group) params))
	  ;; Pick up parameters.
	  (while (re-search-forward ";\\([^=]+\\)=\\([^;:]+\\)"
				    (line-end-position) t)
	    (push (cons (intern (match-string-no-properties 1))
			(downcase (match-string-no-properties 2)))
		  params))
	  (skip-chars-forward ":")
	  ;; Break value on unescaped commas or semicolons, as
	  ;; appropriate.  Properties may either be compound
	  ;; (eg. addresses), with parts separated by semicolons, or
	  ;; multi-value (eg. categories), with instances separated by
	  ;; commas, but *not both*.
	  (setq sep (if (memq prop vcard-compound-properties) ";" ",")
		anchor (point))
	  (while (re-search-forward sep (line-end-position) t)
	    ;; 92 = backslash.  Having ?\ in the buffer confuses
	    ;; paredit.
	    (unless (eql (char-before (1- (point))) 92)
	      (push (buffer-substring-no-properties anchor (1- (point))) value)
	      (setq anchor (point))))
	  (push (buffer-substring-no-properties
		 anchor (line-end-position))
		value)
	  ;; Unescape all remaining colons, semicolons, commas,
	  ;; backslashes and newlines.
	  (setq value
		(mapcar (lambda (v)
			  (replace-regexp-in-string
			   "\\\\\\([\n:;\\,]\\)" "\\1" v))
			value))
	  ;; Possibly do some parsing of the value(s).
	  (let ((case-fold-search t))
	    (setq value
		  (mapcar
		   (lambda (v)
		     (cond
		      ((string-match-p "false" v)
		       nil)
		      ((string-match-p "true" v)
		       t)
		      ;; What the hell is this, anyway?
		      ((and (eql prop 'x-ablabel)
			    (string-match "_$!<\\([^>]+\\)>!$_" v))
		       (match-string 1 v))
		      ((memql prop vcard-datetime-properties)
		       (if vcard-parse-datetime-values
			   (let ((val-type (cdr-safe (assoc 'value params))))
			     (cond
			      ((and (stringp val-type)
				    (string-equal val-type "text"))
			       v)
			      ((and (stringp val-type)
				    (string-equal val-type "timestamp"))
			       (parse-time-string v))
			      (t
			       (condition-case nil
				   (iso8601-parse v)
				 (error
				  (lwarn
				   '(vcard) :error
				   "Unable to parse date value: \"%s\"" v))))))
			 v))
		      ((string-match-p "\\`[[:digit:].]+\\'" v)
		       (string-to-number v))
		      (t v)))
		   value)))
	  ;; Do we want to normalize this?  This way consumers have to
	  ;; explicitly check if it's a string or a list.
	  (setq value
		(if (= 1 (length value))
		    (car value)
		  (nreverse value)))
	  (push (funcall prop-consumer prop value params)
		card))
	(forward-line)))
    (if card-consumer
	(funcall card-consumer (nreverse card))
      (nreverse card))))

(cl-defmethod vcard-contact-properties ((contact list))
  "Return a list of all properties in CONTACT."
  contact)

(cl-defmethod vcard-contact-property-types ((contact list))
  "Return a list of all property types in CONTACT.
Each type is a symbol representing a downcased property name."
  (let (types)
    (dolist (p (vcard-contact-properties contact) types)
      (cl-pushnew (car p) types))))

(cl-defmethod vcard-contact-property-type ((contact list)
					   (type symbol))
  "Return all properties of TYPE from CONTACT.
TYPE is a symbol, e.g. 'email."
  (let (props)
    (dolist (p (vcard-contact-properties contact) props)
      (when (eql type (car p))
	(push p props)))))

(cl-defmethod vcard-contact-property-groups ((contact list))
  "Return a list of all properties groups in CONTACT.
Each group is a string."
  (let (groups)
    (dolist (p (vcard-contact-properties contact) (nreverse groups))
      (when-let ((g (cdr-safe (assoc 'group (nth 2 p)))))
	(cl-pushnew g groups :test #'equal)))))

(cl-defmethod vcard-contact-property-group ((contact list)
					    (group string))
  "Return all properties belonging to GROUP in CONTACT.
GROUP is a string."
  (let (props)
    (dolist (p (vcard-contact-properties contact) props)
      (when (string-equal (cdr (assq 'group (nth 2 p)))
			  group)
	(push p props)))))

(provide 'vcard-parse)
;;; vcard-parse.el ends here
