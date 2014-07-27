;;; puppet-mode.el --- Major mode for Puppet manifests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013, 2014  Bozhidar Batsov <bozhidar@batsov.com>
;; Copyright (C) 2011  Puppet Labs Inc

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;;     Sebastian Wiesner <swiesner@lunaryorn.com>
;;     Russ Allbery <rra@stanford.edu>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;;     Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/puppet-mode
;; Keywords: languages
;; Version: 20140725.1546
;; X-Original-Version: 0.4-cvs
;; Package-Requires: ((emacs "24.1") (pkg-info "0.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file incorporates work covered by the following copyright and
;; permission notice:

;;   Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;   use this file except in compliance with the License.  You may obtain a copy
;;   of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
;;   License for the specific language governing permissions and limitations
;;   under the License.

;;; Commentary:

;; GNU Emacs 24 major mode for editing Puppet manifests.

;; Provides syntax highlighting, indentation, alignment, movement, Imenu and
;; code checking.

;; Syntax highlighting: Fontification upports all of Puppet 3 syntax, including
;; variable expansion in strings.

;; Indentation: Indent expressions automatically.

;; Alignment: Provide alignment rules for common Puppet expressions, and align
;; the current block with `puppet-align-block' on C-c C-a.

;; Movement: Move to the beginning or end of the current block with
;; `beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.

;; Imenu: Jump to a tag in the current buffer with `imenu' on C-c C-j.  Index
;; variables, resource defaults, classes, nodes, defined types and resource
;; declarations.

;; Code checking: Validate the syntax of the current buffer with
;; `puppet-validate' on C-c C-v.  Lint the current buffer for semantic errors
;; with `puppet-lint' on C-c C-l.  Apply the current buffer with `puppet-apply'
;; on C-c C-c.

;; Flymake: Flymake support is _not_ provided. See Flycheck at
;; http://flycheck.readthedocs.org/en/latest/ for on-the-fly validation and
;; liniting of Puppet manifests.

;;; Code:


;;; Compatibility
(eval-and-compile
  ;; `defvar-local' for Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))


;;; Requirements
(declare-function pkg-info-version-info "pkg-info" (library))

(eval-when-compile
  (require 'rx))

(require 'align)


;;; Customization
(defgroup puppet nil
  "Puppet mastering in Emacs"
  :prefix "puppet-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/lunaryorn/puppet-mode")
  :link '(emacs-commentary-link :tag "Commentary" "puppet-mode"))

(defcustom puppet-indent-level 2
  "Indentation of Puppet statements."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-include-indent 2
  "Indentation of continued Puppet include statements."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :type 'boolean
  :group 'puppet
  :safe 'booleanp)

(defcustom puppet-comment-column 32
  "Indentation column of comments."
  :type 'integer
  :group 'puppet
  :safe 'integerp)

(defcustom puppet-fontify-variables-in-comments nil
  "When non-nil, fontify variable references in comments."
  :type 'boolean
  :group 'puppet
  :safe 'booleanp
  :package-version '(puppet-mode . "0.3"))

(defcustom puppet-validate-command "puppet parser validate --color=false"
  "Command to validate the syntax of a Puppet manifest."
  :type 'string
  :group 'puppet)

(defcustom puppet-lint-command
  (concat
   "puppet-lint --with-context "
   "--log-format \"%{path}:%{linenumber}: %{kind}: %{message} (%{check})\"")
  "Command to lint a Puppet manifest."
  :type 'string
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defcustom puppet-apply-command "puppet apply --verbose --noop"
  "Command to apply a Puppet manifest."
  :type 'string
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defface puppet-regular-expression-literal
  '((t :inherit font-lock-constant-face))
  "Face for regular expression literals in Puppet."
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))

(defface puppet-escape-sequence
  '((t :inherit font-lock-constant-face))
  "Face for escape sequences in double-quoted strings-consed literals in Puppet."
  :group 'puppet
  :package-version '(puppet-mode . "0.3"))


;;; Version information
(defun puppet-version (&optional show-version)
  "Get the Puppet Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'puppet-mode)))
    (when show-version
      (message "Puppet Mode version: %s" version))
    version))


;;; Utilities

(defun puppet-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.

Return nil, if there is no special context at POS, or one of

`comment'
     POS is inside a comment

`single-quoted'
     POS is inside a single-quoted string

`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (`?\' 'single-quoted)
        (`?\" 'double-quoted)))))

(defun puppet-in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or comment."
  (not (null (puppet-syntax-context pos))))


;;; Specialized rx

(eval-when-compile
  (defun puppet-rx-symbol (form)
    "Translate FORM into a regular expression."
    (let ((body (cdr form)))
      (rx-to-string `(and symbol-start ,@body symbol-end) 'no-group)))

  (defconst puppet-rx-constituents
    `((symbol puppet-rx-symbol 0 nil)
      ;; http://docs.puppetlabs.com/puppet/3/reference/lang_datatypes.html#regular-expressions
      (regexp-literal . ,(rx (zero-or-more
                              (or
                               ;; Not the end of a regexp
                               (not (any "/" "\\" "\n"))
                               ;; Any escaped character
                               (and "\\" not-newline)))))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#reserved-words
      (keyword . ,(rx (or "and" "case" "class" "default" "define" "else" "elsif"
                          "false" "if" "in" "import" "inherits" "node" "or"
                          "true" "undef" "unless")))
      ;; http://docs.puppetlabs.com/references/latest/function.html
      (builtin-function . ,(rx (or "alert" "collect" "contain"
                                   "create_resources" "crit" "debug" "defined"
                                   "each" "emerg" "err" "extlookup" "fail"
                                   "file" "filter" "fqdn_rand" "generate"
                                   "hiera" "hiera_array" "hiera_hash"
                                   "hiera_include" "include" "info"
                                   "inline_template" "lookup" "map" "md5"
                                   "notice" "realize" "reduce" "regsubst"
                                   "require" "search" "select" "sha1"
                                   "shellquote" "slice" "split" "sprintf" "tag"
                                   "tagged" "template" "versioncmp" "warning")))
      ;; http://docs.puppetlabs.com/references/latest/type.html
      (builtin-type . ,(rx (or "augeas" "computer" "cron" "exec" "file"
                               "filebucket" "group" "host" "interface" "k5login"
                               "macauthorization" "mailalias" "maillist" "mcx"
                               "mount" "nagios_command" "nagios_contact"
                               "nagios_contactgroup" "nagios_host"
                               "nagios_hostdependency" "nagios_hostescalation"
                               "nagios_hostextinfo" "nagios_hostgroup"
                               "nagios_service" "nagios_servicedependency"
                               "nagios_serviceescalation" "nagios_serviceextinfo"
                               "nagios_servicegroup" "nagios_timeperiod" "notify"
                               "package" "resources" "router" "schedule"
                               "scheduled_task" "selboolean" "selmodule"
                               "service" "ssh_authorized_key" "sshkey" "stage"
                               "tidy" "user" "vlan" "yumrepo" "zfs" "zone"
                               "zpool")))
      ;; http://docs.puppetlabs.com/references/stable/metaparameter.html.
      ;; Strictly speaking, this is no meta parameter, but it's so common that
      ;; it got a mention in the docs, see
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#ensure,
      ;; so we'll consider it as metaparameter anyway
      (builtin-metaparam . ,(rx (or "alias" "audit" "before" "loglevel" "noop"
                                    "notify" "require" "schedule" "stage"
                                    "subscribe" "tag"
                                    ;; Because it's so common and important
                                    "ensure")))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#classes-and-types
      (resource-name . ,(rx
                         ;; Optional top-level scope
                         (optional "::")
                         (zero-or-more (any "a-z")
                                       (zero-or-more (any "a-z" "0-9" "_"))
                                       "::")
                         ;; Nested sub-scopes
                         (any "a-z")
                         (zero-or-more (any "a-z" "0-9" "_"))))
      (cap-resource-name . ,(rx
                             ;; Top-scope indicator
                             (optional "::")
                             (zero-or-more (any "A-Z")
                                           (zero-or-more
                                            (any "a-z" "0-9" "_"))
                                           "::")
                             ;; Nested sub-scopes
                             (any "A-Z")
                             (zero-or-more (any "a-z" "0-9" "_"))))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#nodes
      (node-name . ,(rx (one-or-more (any "a-z" "0-9" ?. ?_ ?-))))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_reserved.html#variables
      (simple-variable-name . ,(rx (one-or-more (any "A-Z" "a-z" "0-9" "_"))))
      (variable-name . ,(rx
                         ;; The optional scope designation
                         (optional "::")
                         (zero-or-more (any "a-z")
                                       (zero-or-more
                                        (any "A-Z" "a-z" "0-9" "_"))
                                       "::")
                         ;; The final variable name
                         (one-or-more (any "A-Z" "a-z" "0-9" "_"))))
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_datatypes.html#double-quoted-strings
      (dq-escape . ,(rx (or line-start (not (any "\\")))
                        (zero-or-more "\\\\")
                        ;; We do not include \n and \', because these are
                        ;; available in single-quoted strings as well
                        (group "\\" (any ?\" ?$ ?n ?r ?t ?s)))))
    "Additional special sexps for `puppet-rx'")

  (defmacro puppet-rx (&rest sexps)
    "Specialized `rx' variant for Puppet Mode.

In addition to the standard forms of `rx', the following forms
are available:

`(symbol SEXP …)'
     Match SEXPs inside symbol boundaries only

`regexp-literal'
     A Puppet regexp literal, *without* surrounding slashes

`keyword'
     Any valid Puppet keyword

`builtin-function'
     Any built-in Puppet function

`builtin-type'
     Any built-in Puppet type

`builtin-metaparam'
     Any built-in meta-parameter, and `ensure'

`resource-name'
     Any valid resource name, including scopes

`cap-resource-name'
     Any capitalized resource name, including capitalized scopes

`node-name'
     Any valid node name

`simple-variable-name'
     Any variable name without scopes, without leading dollar sign

`variable-name'
     Any variable name including scopes, without a leading dollar sign

`dq-escape'
     Special escape sequences for double-quoted strings"
    (let ((rx-constituents (append puppet-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))


;;; Checking

(defvar-local puppet-last-validate-command nil
  "The last command used for validation.")

(defvar-local puppet-last-lint-command nil
  "The last command used for linting.")

(defvar-local puppet-last-apply-command nil
  "The last command used to apply a manifest.")

(defun puppet-run-check-command (command buffer-name-template)
  "Run COMMAND to check the current buffer."
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command nil (lambda (_)
                                   (format buffer-name-template command))))

(defun puppet-read-command (prompt previous-command default-command)
  "Read a command from minibuffer with PROMPT."
  (let* ((buffer-file-name (or (buffer-file-name) ""))
         (filename (or (file-remote-p buffer-file-name 'localname)
                       buffer-file-name)))
    (read-string prompt (or previous-command
                            (concat default-command " "
                                    (shell-quote-argument filename))))))

(defun puppet-validate (command)
  "Validate the syntax of the current buffer with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Validate command: "
                                          puppet-last-validate-command
                                          puppet-validate-command)))
  (setq puppet-last-validate-command command)
  (puppet-run-check-command command "*Puppet Validate: %s*"))

(defun puppet-lint (command)
  "Lint the current buffer with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Lint command: "
                                          puppet-last-lint-command
                                          puppet-lint-command)))
  (setq puppet-last-lint-command command)
  (puppet-run-check-command command "*Puppet Lint: %s*"))

(defun puppet-apply (command)
  "Apply the current manifest with COMMAND.

When called interactively, prompt for COMMAND."
  (interactive (list (puppet-read-command "Apply command: "
                                          puppet-last-apply-command
                                          puppet-apply-command)))
  (setq puppet-last-apply-command command)
  (puppet-run-check-command command "*Puppet Apply: %s*"))


;;; Navigation
;; TODO: Check which of these are still needed for SMIE

(defun puppet-beginning-of-defun-function (&optional arg)
  "Move to the ARG'th beginning of a block."
  (let* ((arg (or arg 1))
         (search (if (< arg 0) #'search-forward #'search-backward))
         (steps (abs arg)))
    (while (> steps 0)
      (let ((pos (funcall search "{" nil 'no-error)))
        ;; Skip over strings and comments
        (while (and pos (puppet-in-string-or-comment-p pos))
          (setq pos (funcall search "{" nil 'no-error)))
        (if pos
            (setq steps (1- steps))
          ;; Drop out of outer loop
          (setq steps 0))))
    (when (< arg 0)
      (backward-char))))


;;; Indentation code
(defun puppet-block-indent ()
  "If point is in a block, return the indentation of the first line of that
block (the line containing the opening brace).  Used to set the indentation
of the closing brace of a block."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "{" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-braces (count-matches "}" apoint opoint))
                (open-braces 0))
            (while (and apoint (> close-braces open-braces))
              (setq apoint (search-backward "{" nil t))
              (when apoint
                (setq close-braces (count-matches "}" apoint opoint))
                (setq open-braces (1+ open-braces)))))
          (if apoint
              (current-indentation)
            nil))))))

(defun puppet-in-array ()
  "If point is in an array, return the position of the opening '[' of
that array, else return nil."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "[" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-brackets (count-matches "]" apoint opoint))
                (open-brackets 0))
            (while (and apoint (> close-brackets open-brackets))
              (setq apoint (search-backward "[" nil t))
              (when apoint
                (setq close-brackets (count-matches "]" apoint opoint))
                (setq open-brackets (1+ open-brackets)))))
          apoint)))))

(defun puppet-in-include ()
  "If point is in a continued list of include statements, return the position
of the initial include plus puppet-include-indent."
  (save-excursion
    (save-match-data
      (let ((include-column nil)
            (not-found t))
        (while not-found
          (forward-line -1)
          (cond
           ((bobp)
            (setq not-found nil))
           ((looking-at "^\\s-*include\\s-+.*,\\s-*$")
            (setq include-column
                  (+ (current-indentation) puppet-include-indent))
            (setq not-found nil))
           ((not (looking-at ".*,\\s-*$"))
            (setq not-found nil))))
        include-column))))

(defun puppet-indent-line ()
  "Indent current line as puppet code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)                ; First line is always non-indented
    (let ((not-indented t)
          (array-start (puppet-in-array))
          (include-start (puppet-in-include))
          (block-indent (puppet-block-indent))
          cur-indent)
      (cond
       (array-start
        ;; This line probably starts with an element from an array.
        ;; Indent the line to the same indentation as the first
        ;; element in that array.  That is, this...
        ;;
        ;;    exec {
        ;;      "add_puppetmaster_mongrel_startup_links":
        ;;      command => "string1",
        ;;      creates => [ "string2", "string3",
        ;;      "string4", "string5",
        ;;      "string6", "string7",
        ;;      "string3" ],
        ;;      refreshonly => true,
        ;;    }
        ;;
        ;; ...should instead look like this:
        ;;
        ;;    exec {
        ;;      "add_puppetmaster_mongrel_startup_links":
        ;;      command => "string1",
        ;;      creates => [ "string2", "string3",
        ;;                   "string4", "string5",
        ;;                   "string6", "string7",
        ;;                   "string8" ],
        ;;      refreshonly => true,
        ;;    }
        (save-excursion
          (goto-char array-start)
          (forward-char 1)
          (re-search-forward "\\S-")
          (forward-char -1)
          (setq cur-indent (current-column))))
       (include-start
        (setq cur-indent include-start))
       ((and (looking-at "^\\s-*},?\\s-*$") block-indent)
        ;; This line contains a closing brace or a closing brace followed by a
        ;; comma and we're at the inner block, so we should indent it matching
        ;; the indentation of the opening brace of the block.
        (setq cur-indent block-indent))
       (t
        ;; Otherwise, we did not start on a block-ending-only line.
        (save-excursion
          ;; Iterate backwards until we find an indentation hint
          (while not-indented
            (forward-line -1)
            (cond
             ;; Comment lines are ignored unless we're at the start of the
             ;; buffer.
             ((eq (puppet-syntax-context) 'comment)
              (if (bobp)
                  (setq not-indented nil)))

             ;; Brace or paren on a line by itself will already be indented to
             ;; the right level, so we can cheat and stop there.
             ((looking-at "^\\s-*[\)}]\\s-*")
              (setq cur-indent (current-indentation))
              (setq not-indented nil))

             ;; Brace (possibly followed by a comma) or paren not on a line by
             ;; itself will be indented one level too much, but don't catch
             ;; cases where the block is started and closed on the same line.
             ((looking-at "^[^\n\({]*[\)}],?\\s-*$")
              (setq cur-indent (- (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent by one level more than the start of our block.  We lose
             ;; if there is more than one block opened and closed on the same
             ;; line but it's still unbalanced; hopefully people don't do that.
             ((looking-at "^.*{[^\n}]*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent by one level if the line ends with an open paren.
             ((looking-at "^.*\(\\s-*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Semicolon ends a block for a resource when multiple resources
             ;; are defined in the same block, but try not to get the case of
             ;; a complete resource on a single line wrong.
             ((looking-at "^\\([^'\":\n]\\|\"[^\n\"]*\"\\|'[^\n']*'\\)*;\\s-*$")
              (setq cur-indent (- (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Indent an extra level after : since it introduces a resource.
             ((looking-at "^.*:\\s-*$")
              (setq cur-indent (+ (current-indentation) puppet-indent-level))
              (setq not-indented nil))

             ;; Start of buffer.
             ((bobp)
              (setq not-indented nil)))))

        ;; If this line contains only a closing paren, we should lose one
        ;; level of indentation.
        (if (looking-at "^\\s-*\)\\s-*$")
            (setq cur-indent (- cur-indent puppet-indent-level)))))

      ;; We've figured out the indentation, so do it.
      (if (and cur-indent (> cur-indent 0))
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


;;; Font locking

(defvar puppet-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Our strings
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    ;; C-style comments.  Yes, Puppet has these!
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; Line comments
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; The backslash is our escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; The dollar sign is an expression prefix for variables
    (modify-syntax-entry ?$ "'" table)
    ;; Fix various operators and punctionation.
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?\; "." table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table in use in `puppet-mode' buffers.")

(defvar puppet-font-lock-keywords
  `(
    ;; Keywords
    (,(puppet-rx (symbol keyword)) 0 font-lock-keyword-face)
    ;; Variables
    (,(puppet-rx "$" (symbol variable-name)) 0 font-lock-variable-name-face)
    ;; Class and type declarations
    (,(puppet-rx (symbol (or "class" "define"))
                 (one-or-more space)
                 (group (symbol resource-name)))
     1 font-lock-type-face)
    ;; Node declarations
    (,(puppet-rx (symbol "node")
                 (one-or-more space)
                 (group node-name))
     1 font-lock-type-face)
    ;; Resource usage, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html
    (,(puppet-rx (group (repeat 0 2 "@") ; Virtual and exported resources
                        (symbol resource-name))
                 (zero-or-more space) "{")
     1 font-lock-type-face)
    ;; Resource defaults, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_defaults.html
    (,(puppet-rx (group (symbol cap-resource-name)) (zero-or-more space) "{")
     1 font-lock-type-face)
    ;; Resource references, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_datatypes.html#resource-references
    (,(puppet-rx (group (symbol cap-resource-name)) (zero-or-more space) "[")
     1 font-lock-type-face)
    ;; Resource collectors, see
    ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_collectors.html
    (,(puppet-rx (group (symbol cap-resource-name)) (zero-or-more space)
                 (optional "<")         ; Exported collector
                 "<|")
     1 font-lock-type-face)
    ;; Negation
    ("!" 0 font-lock-negation-char-face)
    ;; Builtin meta parameters
    (,(puppet-rx (group (symbol builtin-metaparam)) (zero-or-more space) "=>")
     1 font-lock-builtin-face)
    ;; Built-in functions
    (,(puppet-rx (symbol builtin-function)) 0 font-lock-builtin-face)
    ;; Type arguments to some special built-in functions
    (,(puppet-rx (symbol (or "contain" "include" "require"))
                 (one-or-more space)
                 (group (symbol resource-name)))
     1 font-lock-type-face)
    ;; Variable expansions in strings and comments
    (puppet-match-valid-expansion 1 font-lock-variable-name-face t)
    (puppet-match-invalid-expansion 1 font-lock-warning-face t)
    ;; Escape sequences in strings
    (puppet-match-valid-escape 1 'puppet-escape-sequence t)
    ;; Regexp literals
    (puppet-match-regexp-literal (1 'puppet-regular-expression-literal t)
                                 (2 'puppet-regular-expression-literal t)
                                 (3 'puppet-regular-expression-literal t)))
  "Font lock keywords for Puppet Mode.")

(defun puppet-match-property (property context limit)
  "Match a PROPERTY in CONTEXT before LIMIT.

PROPERTY is the text property to look for.  CONTEXT is one of
`single-quoted', `double-quoted', `comment' or nil, or a list
with any of these symbols.  The expansion will only match if it
is in any given CONTEXT.  nil means no specific syntactic context."
  (when (symbolp context)
    (setq context (list context)))
  (let* ((pos (next-single-char-property-change (point) property nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let* ((value (get-text-property pos property)))
        (if (and value (memq (car value) context))
            (progn (set-match-data (cdr value)) t)
          (puppet-match-property property context limit))))))

(defun puppet-match-valid-expansion (limit)
  "Match a valid expansion before LIMIT.

A valid expansion is a variable expansion in a double-quoted
string."
  (let ((valid-contexts '(double-quoted)))
    (when puppet-fontify-variables-in-comments
      (push 'comment valid-contexts))
    (puppet-match-property 'puppet-expansion valid-contexts limit)))

(defun puppet-match-invalid-expansion (limit)
  "Match an invalid expansion before LIMIT.

An invalid expansion is a variable expansion in a single-quoted
string."
  (puppet-match-property 'puppet-expansion 'single-quoted limit))

(defun puppet-match-valid-escape (limit)
  "Match a valid escape sequence before LIMIT."
  (puppet-match-property 'puppet-escape 'double-quoted limit))

(defun puppet-match-regexp-literal (limit)
  "Match a regular expression literal before LIMIT."
  (puppet-match-property 'puppet-regexp-literal nil limit))

(defun puppet-syntax-propertize-match (property &optional group)
  "Propertize a match with PROPERTY at GROUP's beginning.

When in a special syntax context, add PROPERTY to the first
character of the given GROUP of the current `match-data'.  GROUP
defaults to the first group.

The value of PROPERTY is `(CONTEXT . MATCH-DATA)', where CONTEXT
is one of nil, `single-quoted', `double-quoted' or `comment' and
denotes the surrounding context, and MATCH-DATA is the original
match data from propertization."
  (let* ((beg (match-beginning (or group 1)))
         ;; Syntax functions can modify the match data, so we must preserve it
         (context (save-match-data (puppet-syntax-context beg))))
    (put-text-property beg (1+ beg) property
                       (cons context (match-data)))))

(defun puppet-syntax-propertize-scope-operator (beg end)
  "Mark all scope operators between BEG and END as symbols."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (while (search-forward "::" end 'no-error)
        (put-text-property (match-beginning 0) (match-end 0)
                           'syntax-table (string-to-syntax "_"))))))

(defun puppet-syntax-propertize-function (start end)
  "Propertize text between START and END.

Used as `syntax-propertize-function' in Puppet Mode."
  (let ((case-fold-search nil))
    (goto-char start)
    (remove-text-properties start end '(puppet-expansion
                                        puppet-escape
                                        puppet-regexp-literal))
    (funcall
     (syntax-propertize-rules
      ;; Make double colons part of the surrounding symbol.  We can't put the
      ;; colon into symbol syntax, because the colon can appear as non-symbol
      ;; character as well (e.g. "package { $foo:"), but we want the
      ;; double-colon as part of the symbol to make symbol navigation move
      ;; across it, and to make stuff like `thing-at-point' behave reasonably
      ((rx "::" symbol-start) (0 "_"))
      ;; Mark regular expression literals in proper contexts (nodes, cases,
      ;; selectors and match operators) as strings, to make them play nicely
      ;; with sexp navigation and SMIE.  Also propertize them for use in font
      ;; lock keywords, since we want to apply our own
      ;; puppet-regular-expression-literal face instead of the generic string
      ;; face.
      ((puppet-rx (group "/")
                  (group regexp-literal)
                  (group "/")
                  (zero-or-more space)
                  (or ":" "=>"))
       ;; We propertize the body of the regexp literal, not its delimiters, to
       ;; make sure that font lock keywords kick in when the literal gets moved
       ;; with point on separator.  The separator is propertized by syntactic
       ;; font lock (since we marked it as string delimiter), so font lock
       ;; keywords will start in the body of the literal.  If we'd propertize
       ;; the separator, font lock keywords would miss the property
       (0 (ignore (puppet-syntax-propertize-match 'puppet-regexp-literal 2)))
       (1 "|") (3 "|"))
      ((puppet-rx (or "=~" "!~" (symbol "node")) (zero-or-more space)
                  (group "/")
                  (group regexp-literal)
                  (group "/"))
       (0 (ignore (puppet-syntax-propertize-match 'puppet-regexp-literal 2)))
       (1 "|") (3 "|"))
      ;; Find escape sequences and variable expansions.
      ((puppet-rx dq-escape)
       (1 (ignore (puppet-syntax-propertize-match 'puppet-escape))))
      ((puppet-rx (or line-start (not (any "\\")))
                  (zero-or-more "\\\\")
                  ;; We can't use symbol boundaries here, because
                  ;; `syntax-propertize-rules' applies all rules at the same
                  ;; time, so the double-colon scope separator isn't yet part of
                  ;; the symbol at this point.
                  (group "$" (or (and "{" variable-name "}") variable-name)))
       (1 (ignore (progn
                    (puppet-syntax-propertize-match 'puppet-expansion)
                    ;; Propertize all scope operators in the current variable
                    (puppet-syntax-propertize-scope-operator
                     (match-beginning 0) (match-end 0)))))))
     start end)))


;;; Alignment

;; Configure alignment
(add-to-list 'align-sq-string-modes 'puppet-mode)
(add-to-list 'align-dq-string-modes 'puppet-mode)
(add-to-list 'align-open-comment-modes 'puppet-mode)

(defconst puppet-mode-align-rules
  '((puppet-resource-arrow
     (regexp . "\\(\\s-*\\)=>\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(puppet-mode))))
  "Align rules for Puppet Mode.")

(defun puppet-align-block ()
  "Align the current block."
  (interactive)
  (save-excursion
    (save-match-data
      (let ((beg (search-backward "{" nil 'no-error)))
        ;; Skip backwards over strings and comments
        (while (and beg (puppet-in-string-or-comment-p beg))
          (setq beg (search-backward "{" nil 'no-error)))
        (when beg
          (forward-list)
          (align beg (point)))))))


;;; Dealing with strings
(defun puppet-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))

(defun puppet-string-at-point-p ()
  "Check if cursor is at a string or not."
  (puppet-string-region))

(defun puppet-string-region ()
  "Return region for string at point."
  (let ((orig-point (point)) (regex "'\\(\\(\\\\'\\)\\|[^']\\)*'\\|\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"") beg end)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (re-search-forward regex (line-end-position) t) (not (and beg end)))
        (let ((match-beg (match-beginning 0)) (match-end (match-end 0)))
          (when (and
                 (> orig-point match-beg)
                 (< orig-point match-end))
            (setq beg match-beg)
            (setq end match-end))))
      (and beg end (list beg end)))))

(defun puppet-interpolate (suppress)
  "Interpolate with ${} in double quoted strings.

With a prefix argument SUPPRESS it simply inserts $."
  (interactive "P")
  (if (and mark-active (equal (point) (region-end)))
      (exchange-point-and-mark))
  (insert "$")
  (when (and
         (not suppress)
         (or
          (puppet-looking-around "\"[^\"\n]*" "[^\"\n]*\"")
          (puppet-looking-around "`[^`\n]*"   "[^`\n]*`")
          (puppet-looking-around "%([^(\n]*"  "[^)\n]*)")))
    (cond (mark-active
           (goto-char (region-beginning))
           (insert "{")
           (goto-char (region-end))
           (insert "}"))
          (t
           (insert "{}")
           (forward-char -1)))))

(defun puppet-toggle-string-quotes ()
  "Toggle string literal quoting between single and double."
  (interactive)
  (when (puppet-string-at-point-p)
    (let* ((region (puppet-string-region))
           (min (nth 0 region))
           (max (nth 1 region))
           (string-quote (puppet--inverse-string-quote (buffer-substring-no-properties min (1+ min))))
           (content
            (buffer-substring-no-properties (1+ min) (1- max))))
      (setq content
            (if (equal string-quote "\"")
                (replace-regexp-in-string "\\\\\"" "\"" (replace-regexp-in-string "\\([^\\\\]\\)'" "\\1\\\\'" content))
              (replace-regexp-in-string "\\\\\'" "'" (replace-regexp-in-string "\\([^\\\\]\\)\"" "\\1\\\\\"" content))))
      (let ((orig-point (point)))
        (delete-region min max)
        (insert
         (format "%s%s%s" string-quote content string-quote))
        (goto-char orig-point)))))

(defun puppet--inverse-string-quote (string-quote)
  "Get the inverse string quoting for STRING-QUOTE."
  (if (equal string-quote "\"") "'" "\""))

(defun puppet-clear-string ()
  "Clear string at point."
  (interactive)
  (when (puppet-string-at-point-p)
    (let* ((region (puppet-string-region))
           (min (nth 0 region))
           (max (nth 1 region)))
      (delete-region (+ min 1) (- max 1)))))



;;; Imenu

(defun puppet-imenu-collect-entries (pattern)
  "Collect all index entries matching PATTERN.

The first matching group of PATTERN is used as title and position
for each entry."
  (goto-char (point-min))
  (let ((case-fold-search nil)
        entries)
    (while (re-search-forward pattern nil 'no-error)
      (let ((entry (cons (match-string 1) (match-beginning 1))))
        (unless (puppet-in-string-or-comment-p (match-beginning 0))
          ;; Skip this match if it's inside a string or comment
          (push entry entries))))
    (nreverse entries)))

(defun puppet-imenu-create-index ()
  "Create an IMenu index for the current buffer."
  (let ((case-fold-search nil)
        ;; Variable assignments
        (variables (puppet-imenu-collect-entries
                    (puppet-rx (group "$" (symbol simple-variable-name))
                               (zero-or-more space) "=")))
        ;; Resource defaults
        (defaults (puppet-imenu-collect-entries
                   (puppet-rx (group (symbol cap-resource-name))
                              (zero-or-more space) "{")))
        ;; Nodes, classes and defines
        (nodes (puppet-imenu-collect-entries
                (puppet-rx (symbol "node")
                           (one-or-more space)
                           (group (symbol node-name)))))
        (classes (puppet-imenu-collect-entries
                  (puppet-rx (symbol "class")
                             (one-or-more space)
                             (group (symbol resource-name)))))
        (defines (puppet-imenu-collect-entries
                  (puppet-rx (symbol "define")
                             (one-or-more space)
                             (group (symbol resource-name)))))
        resources)
    ;; Resources are a little more complicated since we need to extract the type
    ;; and the name
    (goto-char (point-min))
    (while (re-search-forward
            (puppet-rx (group (repeat 0 2 "@") ; Virtual and exported resources
                              (symbol resource-name))
                       (zero-or-more space) "{"
                       ;; FIXME: Support condensed forms
                       (zero-or-more space)
                       (group (one-or-more not-newline)) ":")
            nil 'no-error)
      ;; FIXME: Doesn't work for any condensed forms, see
      ;; http://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#condensed-forms
      ;; We probably need to be more clever here
      (push (cons (concat (match-string 1) " " (match-string 2))
                  (match-beginning 1))
            resources))
    (let (index
          ;; Keep this in reversed order, for `push'
          (parts (list (cons "Variables" variables)
                       (cons "Defaults" defaults)
                       (cons "Definitions" defines)
                       (cons "Classes" classes)
                       (cons "Nodes" nodes))))
      (dolist (part parts)
        (when (cdr part)
          (push part index)))
      (append index (nreverse resources)))))


;;; Major mode definition

(defvar puppet-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing
    (define-key map (kbd "C-c C-a") #'puppet-align-block)
    (define-key map (kbd "C-c C-'") #'puppet-toggle-string-quotes)
    (define-key map (kbd "C-c C-;") #'puppet-clear-string)
    (define-key map (kbd "$") #'puppet-interpolate)
    ;; Navigation
    (define-key map (kbd "C-c C-j") #'imenu)
    ;; Apply manifests
    (define-key map (kbd "C-c C-c") #'puppet-apply)
    ;; Linting and validation
    (define-key map (kbd "C-c C-v") #'puppet-validate)
    (define-key map (kbd "C-c C-l") #'puppet-lint)
    ;; The menu bar
    (easy-menu-define puppet-menu map "Puppet Mode menu"
      `("Puppet"
        :help "Puppet-specific Features"
        ["Align the current block" puppet-align-block
         :help "Align parameters in the current block"]
        ["Clear string" puppet-clear-string
         :help "Clear the string at point"]
        ["Toggle string quotes" puppet-toggle-string-quotes
         :help "Toggle the string at point quotes between single and double"]
        "-"
        ["Jump to resource/variable" imenu
         :help "Jump to a resource or variable"]
        "-"
        ["Apply manifest" puppet-apply :help "Apply a Puppet manifest"]
        "-"
        ["Validate file syntax" puppet-validate
         :help "Validate the syntax of this file"]
        ["Lint file" puppet-lint
         :help "Check the file for semantic issues"]))
    map)
  "Key map for Puppet Mode buffers.")

;;;###autoload
(define-derived-mode puppet-mode prog-mode "Puppet" ()
  "Major mode for editing Puppet manifests.

\\{puppet-mode-map}"
  ;; Comment setup
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local comment-auto-fill-only-comments t)
  (setq comment-column puppet-comment-column)
  ;; Navigation (TODO: Will we still need this with SMIE?)
  (setq-local beginning-of-defun-function #'puppet-beginning-of-defun-function)
  ;; Indentation
  (setq-local indent-line-function #'puppet-indent-line)
  (setq indent-tabs-mode puppet-indent-tabs-mode)
  ;; Paragaphs
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ \t]*$\\|#$")
  (setq-local paragraph-separate "\\([ \t\f]*\\|#\\)$")
  ;; Font locking
  (setq font-lock-defaults '((puppet-font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'puppet-syntax-propertize-function)
  ;; Alignment
  (setq align-mode-rules-list puppet-mode-align-rules)
  ;; IMenu
  (setq imenu-create-index-function #'puppet-imenu-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(provide 'puppet-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; puppet-mode.el ends here
