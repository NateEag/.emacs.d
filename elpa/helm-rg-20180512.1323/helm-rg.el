;;; helm-rg.el --- a helm interface to ripgrep -*- lexical-binding: t -*-
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author: Danny McClanahan
;; Version: 0.1
;; Package-Version: 20180512.1323
;; URL: https://github.com/cosmicexplorer/helm-rg
;; Package-Requires: ((emacs "25") (helm "2.8.8") (cl-lib "0.5") (dash "2.13.0"))
;; Keywords: find, file, files, helm, fast, rg, ripgrep, grep, search


;;; Commentary:

;; The below is generated from a README at
;; https://github.com/cosmicexplorer/helm-rg.

;; MELPA: https://melpa.org/#/helm-rg

;; Search massive codebases extremely fast, using `ripgrep'
;; (https://github.com/BurntSushi/ripgrep) and `helm'
;; (https://github.com/emacs-helm/helm). Inspired by `helm-ag'
;; (https://github.com/syohex/emacs-helm-ag) and `f3'
;; (https://github.com/cosmicexplorer/f3).

;; Also check out rg.el (https://github.com/dajva/rg.el), which I haven't used
;; much but seems pretty cool.


;; Usage:

;; *See the `ripgrep' whirlwind tour
;; (https://github.com/BurntSushi/ripgrep#whirlwind-tour) for further
;; information on invoking `ripgrep'.*

;; - Invoke the interactive function `helm-rg' to start a search with `ripgrep'
;; in the current directory.
;;     - `helm' is used to browse the results and update the output as you
;; type.
;;     - Each line has the file path, the line number, and the column number of
;; the start of the match, and each part is highlighted differently.
;;     - Use `TAB' to invoke the helm persistent action, which previews the
;; result and highlights the matched text in the preview.
;;     - Use `RET' to visit the file containing the result, move point to the
;; start of the match, and recenter.
;;         - The result's buffer is displayed with
;; `helm-rg-display-buffer-normal-method' (which defaults to
;; `switch-to-buffer').
;;         - Use a prefix argument (`C-u RET') to open the buffer with
;; `helm-rg-display-buffer-alternate-method' (which defaults to
;; `pop-to-buffer').
;; - The text entered into the minibuffer is interpreted into a PCRE
;; (https://pcre.org) regexp to pass to `ripgrep'.
;;     - `helm-rg''s pattern syntax is basically PCRE, but single spaces
;; basically act as a more powerful conjunction operator.
;;         - For example, the pattern `a b' in the minibuffer is transformed
;; into `a.*b|b.*a'.
;;             - The single space can be used to find lines with any
;; permutation of the regexps on either side of the space.
;;             - Two spaces in a row will search for a literal single space.
;;         - `ripgrep''s `--smart-case' option is used so that case-sensitive
;; search is only on if any of the characters in the pattern are capitalized.
;;             - For example, `ab' (conceptually) searches `[Aa][bB]', but `Ab'
;; in the minibuffer will only search for the pattern `Ab' with `ripgrep',
;; because it has at least one uppercase letter.
;; - Use `M-d' to select a new directory to search from.
;; - Use `M-g' to input a glob pattern to filter files by, e.g. `*.py'.
;;     - The glob pattern defaults to the value of
;; `helm-rg-default-glob-string', which is an empty string (matches every file)
;; unless you customize it.
;;     - Pressing `M-g' again shows the same minibuffer prompt for the glob
;; pattern, with the string that was previously input.
;; - Use `<left>' and `<right>' to go up and down by files in the results.
;;     - `<up>' and `<down>' simply go up and down by match result, and there
;; may be many matches for your pattern in a single file, even multiple on a
;; single line (which `ripgrep' reports as multiple separate results).
;;     - The `<left>' and `<right>' keys will move up or down until it lands on
;; a result from a different file than it started on.
;;         - When moving by file, `helm-rg' will cycle around the results list,
;; but it will print a harmless error message instead of looping infinitely if
;; all results are from the same file.
;; - Use the interactive autoloaded function `helm-rg-display-help' to see the
;; ripgrep command's usage info.


;; TODO:

;; - make a keybinding to drop into an edit mode and edit file content inline
;; in results like `helm-ag' (https://github.com/syohex/emacs-helm-ag)
;; - allow (elisp)? regex searching of search results, including file names
;;     - use `helm-swoop' (https://github.com/ShingoFukuyama/helm-swoop)?
;; - publish `update-commentary.el' and the associated machinery as an npm
;; package


;; License:

;; GPL 3.0+ (./LICENSE)

;; End Commentary


;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dash)
(require 'font-lock)
(require 'helm)
(require 'helm-grep)
(require 'helm-lib)
(require 'pcase)
(require 'rx)


;; Customization Helpers
(defun helm-rg--always-safe-local (_)
  "Use as a :safe predicate in a `defcustom' form to accept any local override."
  t)

(defun helm-rg--gen-defcustom-form-from-alist (name alist doc args)
  `(defcustom ,name ',(car (helm-rg--alist-keys (symbol-value alist)))
     ,doc
     :type `(radio ,@(--map `(const ,it) (helm-rg--alist-keys ,alist)))
     :group 'helm-rg
     ,@args))

(defmacro helm-rg--defcustom-from-alist (name alist doc &rest args)
  "Create a `defcustom' named NAME which allows the keys of ALIST as values.

The default value for the `defcustom' is the `car' of the first element of ALIST. ALIST must be the
unquoted name of a variable containing an alist."
  (declare (indent 2))
  (helm-rg--gen-defcustom-form-from-alist name alist doc args))


;; Customization
(defgroup helm-rg nil
  "Group for `helm-rg' customizations."
  :group 'helm-grep)

(defcustom helm-rg-ripgrep-executable (executable-find "rg")
  "The location of the ripgrep binary executable."
  :type 'string
  :group 'helm-rg)

(defcustom helm-rg-default-glob-string ""
  "The glob pattern used for the '-g' argument to ripgrep.
Set to the empty string to match every file."
  :type 'string
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-default-directory 'default
  "Specification for starting directory to invoke ripgrep in.
Used in `helm-rg--interpret-starting-dir'. Possible values:

'default => Use `default-directory'.
'git-root => Use \"git rev-parse --show-toplevel\" (see
             `helm-rg-git-executable').
<string> => Use the directory at path <string>."
  :type '(choice symbol string)
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-git-executable (executable-find "git")
  "Location of git executable."
  :type 'string
  :group 'helm-rg)

(defcustom helm-rg-thing-at-point 'symbol
  "Type of object at point to initialize the `helm-rg' minibuffer input with."
  :type 'symbol
  :group 'helm-rg)

(defcustom helm-rg-input-min-search-chars 3
  "Ripgrep will not be invoked unless the input is at least this many chars.

See `helm-rg--make-process' and `helm-rg--make-dummy-process' if interested."
  :type 'integer
  :safe #'helm-rg--always-safe-local
  :group 'helm-rg)

(defcustom helm-rg-display-buffer-normal-method #'switch-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

The default function to invoke to display a visited buffer in some window in
`helm-rg'."
  :type 'function
  :group 'helm-rg)

(defcustom helm-rg-display-buffer-alternate-method #'pop-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

The function will be invoked if a prefix argument is used when visiting a result
in `helm-rg'."
  :type 'function
  :group 'helm-rg)

(defcustom helm-rg--only-current-line-match-highlight-files-regexp nil
  "By default, `helm-rg' will create overlays to highlight all the matches from ripgrep in a file
when previewing a result. This is done each time a match is selected, even for buffers already
previewed. Creating these overlays can be slow for files with lots of matches in some search. If this
variable is set to an elisp regexp and some file path matches it, `helm-rg' will only highlight the
current line of the file and the matches in that line when previewing that file."
  :type 'regexp
  :group 'helm-rg)


;; Faces
(defface helm-rg-preview-line-highlight
  '((t (:background "green" :foreground "black")))
  "Face for the line of text matched by the ripgrep process."
  :group 'helm-rg)

(defface helm-rg-preview-match-highlight
  '((t (:background "purple" :foreground "white")))
  "Face of the text matched by the pattern given to the ripgrep process."
  :group 'helm-rg)

(defface helm-rg-cmd-arg-face
  '((t (:foreground "green")))
  "Face of arguments in the ripgrep invocation."
  :group 'helm-rg)


;; Constants
(defconst helm-rg--color-format-argument-alist
  '((red :cmd-line "red" :text-property "red3"))
  "Alist mapping (a symbol named after a color) -> (strings to describe that symbol on the ripgrep
command line and in an emacs text property). This allows `helm-rg' to identify matched text using
ripgrep's highlighted output directly instead of doing it ourselves, by telling ripgrep to highlight
matches a specific color, then searching for that specific color as a text property in the output.")

(defconst helm-rg--style-format-argument-alist
  '((bold :cmd-line "bold" :text-property bold))
  "Very similar to `helm-rg--color-format-argument-alist', but for non-color styling.")

(defconst helm-rg--case-sensitive-argument-alist
  '((smart-case "--smart-case")
    (case-sensitive "--case-sensitive")
    (case-insensitive "--ignore-case"))
  "Alist of methods of treating case-sensitivity when invoking ripgrep.

The value is the ripgrep command-line argument which enforces the specified type of
case-sensitivity.")

(defconst helm-rg--buffer-name "*helm-rg*")
(defconst helm-rg--process-name "*helm-rg--rg*")
(defconst helm-rg--process-buffer-name "*helm-rg--rg-output*")

(defconst helm-rg--error-process-name "*helm-rg--error-process*")
(defconst helm-rg--error-buffer-name "*helm-rg--errors*")

(defconst helm-rg--ripgrep-help-buffer-name "helm-rg-usage-help")

(defconst helm-rg--output-new-file-line-regexp
  (rx (: bos (group (+? (not (any 0)))) eos))
  "Regexp for ripgrep output which marks the start of results for a new file.

See `helm-rg--process-transition' for usage.")

(defconst helm-rg--numbered-text-line-regexp
  (rx (: bos
         (: (group (+ digit)) ":"
            (group (*? anything)))
         eos))
  "Regexp for ripgrep output which marks a matched line, with he line number and content.

See `helm-rg--process-transition' for usage.")

(defconst helm-rg--persistent-action-display-buffer-method #'switch-to-buffer
  "A function accepting a single argument BUF and displaying the buffer.

Let-bound to `helm-rg--display-buffer-method' in `helm-rg--async-persistent-action'.")

(defconst helm-rg--loop-input-pattern-regexp
  (rx
   (:
    (* (char ? ))
    ;; group 1 = single entire element
    (group
     (+
      (|
       (not (in ? ))
       (= 2 ? ))))))
  "Regexp applied iteratively to split the input interpreted by `helm-rg'.")

(defconst helm-rg--jump-location-text-property 'helm-rg-jump-to
  "Name of a text property attached to the colorized ripgrep output.
This text property contains location and match info. See `helm-rg--process-transition' for usage.")


;; Variables
(defvar helm-rg--append-persistent-buffers nil
  "Whether to record buffers opened during an `helm-rg' session.")

(defvar helm-rg--cur-persistent-bufs nil
  "List of buffers opened temporarily during an `helm-rg' session.")

(defvar helm-rg--current-overlays nil
  "List of overlays used to highlight matches in `helm-rg'.")

(defvar helm-rg--current-dir nil
  "Working directory for the current `helm-rg' session.")

(defvar helm-rg--glob-string nil
  "Glob string used for the current `helm-rg' session.")

(defvar helm-rg--glob-string-history nil
  "History variable for the selection of `helm-rg--glob-string'.")

(defvar helm-rg--input-history nil
  "History variable for the pattern input to the ripgrep process.")

(defvar helm-rg--display-buffer-method nil
  "The method to use to display a buffer visiting a result.
Should accept one argument BUF, the buffer to display.")

(defvar helm-rg--paths-to-search nil
  "List of paths to use in the ripgrep command.
All paths are interpreted relative to the directory ripgrep is invoked from.
When nil, searches from the directory ripgrep is invoked from.
See the documentation for `helm-rg-default-directory'.")

(defvar helm-rg--case-sensitivity nil
  "Key of `helm-rg--case-sensitive-argument-alist' to use in a `helm-rg' session.")


;; Buffer-local Variables
(defvar-local helm-rg--process-output-parse-state
  (list :cur-file nil)
  "Contains state which is updated as the ripgrep output is processed.

This is buffer-local because it is specific to a single process invocation and is manipulated in
that process's buffer. See `helm-rg--parse-process-output' for usage.")


;; Utilities
(defun helm-rg--alist-get-exhaustive (key alist)
  "Get KEY from ALIST, or throw an error."
  (or (alist-get key alist)
      (error "key '%s' was not found in alist '%S' during exhaustive check"
             key alist)))

(defun helm-rg--alist-keys (alist)
  "Get all keys of ALIST."
  (cl-mapcar #'car alist))

(defmacro helm-rg--get-optional-typed (type-name obj &rest body)
  "If OBJ is non-nil, check its type against TYPE-NAME, then bind it to `it' and execute BODY."
  (declare (indent 2))
  `(let ((it ,obj))
     (when it
       (cl-check-type it ,type-name)
       ,@body)))

(defmacro helm-rg--into-temp-buffer (to-insert &rest body)
  "Execute BODY at the beginning of a `with-temp-buffer' containing TO-INSERT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,to-insert)
     (goto-char (point-min))
     ,@body))


;; Logic
(defun helm-rg--make-dummy-process (input)
  "Make a process that immediately exits to display just a title."
  (let* ((dummy-proc (make-process
                      :name helm-rg--process-name
                      :buffer helm-rg--process-buffer-name
                      :command '("echo")
                      :noquery t))
         (helm-src-name
          (format "no results (input '%s' must be at least %d characters)"
                  input
                  helm-rg-input-min-search-chars)))
    (helm-attrset 'name helm-src-name)
    dummy-proc))

(defun helm-rg--join (sep seq)
  (mapconcat #'identity seq sep))

(defun helm-rg--props (props str)
  (apply #'propertize (append (list str) props)))

(defun helm-rg--make-face (face str)
  (helm-rg--props `(face ,face) str))

(defun helm-rg--propertize-cmd-arg (arg)
  (if (string-match-p (rx space) arg)
      (helm-rg--make-face 'font-lock-string-face (format "'%s'" arg))
    (helm-rg--make-face 'helm-rg-cmd-arg-face arg)))

(defun helm-rg--format-argv-string (argv)
  (->>
   argv
   (-map #'helm-rg--propertize-cmd-arg)
   (helm-rg--join " ")
   (format
    (concat
     (propertize "(" 'face 'highlight)
     "%s"
     (propertize ")" 'face 'highlight)))))

(cl-deftype helm-rg--existing-file ()
  `(and string
        (satisfies file-exists-p)))

(cl-deftype helm-rg--existing-directory ()
  `(and helm-rg--existing-file
        (satisfies file-directory-p)))

(defun helm-rg--process-paths-to-search (paths)
  (cl-check-type helm-rg--current-dir helm-rg--existing-directory)
  (cl-loop
   for path in paths
   for expanded = (expand-file-name path helm-rg--current-dir)
   unless (file-exists-p expanded)
   do (error (concat "Error: expanded path '%s' does not exist. "
                     "The cwd was '%s', and the paths provided were %S.")
             expanded
             helm-rg--current-dir
             paths)
   collect expanded))

(defun helm-rg--empty-glob-p (glob-str)
  (or (null glob-str)
      (string-blank-p glob-str)))

(defun helm-rg--construct-argv (pattern)
  "Create an argument list for the ripgrep command.
Uses `defcustom' values, and `defvar' values bound in other functions."
  (cons
   helm-rg-ripgrep-executable
   (append
    (helm-rg--alist-get-exhaustive helm-rg--case-sensitivity helm-rg--case-sensitive-argument-alist)
    '("--color=ansi")
    (helm-rg--construct-match-color-format-arguments helm-rg-match-color helm-rg-match-style)
    (unless (helm-rg--empty-glob-p helm-rg--glob-string)
      (list "-g" helm-rg--glob-string))
    (list pattern)
    (helm-rg--process-paths-to-search helm-rg--paths-to-search))))

(defun helm-rg--make-process-from-argv (argv)
  (let* ((real-proc (make-process
                     :name helm-rg--process-name
                     :buffer helm-rg--process-buffer-name
                     :command argv
                     :noquery t))
         (helm-src-name
          (format "argv: %s" (helm-rg--format-argv-string argv))))
    (helm-attrset 'name helm-src-name)
    (set-process-query-on-exit-flag real-proc nil)
    real-proc))

(defun helm-rg--make-process ()
  "Invoke ripgrep in `helm-rg--current-dir' with `helm-pattern'.
Make a dummy process if the input is empty with a clear message to the user."
  (let* ((default-directory helm-rg--current-dir)
         (input helm-pattern))
    (if (< (length input) helm-rg-input-min-search-chars)
        (helm-rg--make-dummy-process input)
      (helm-rg--make-process-from-argv
       (helm-rg--construct-argv input)))))

(defun helm-rg--make-overlay-with-face (beg end face)
  "Generate an overlay in region BEG to END with face FACE."
  (let ((olay (make-overlay beg end)))
    (overlay-put olay 'face face)
    olay))

(defun helm-rg--delete-overlays ()
  "Delete all cached overlays in `helm-rg--current-overlays', and clear it."
  (mapc #'delete-overlay helm-rg--current-overlays)
  (setq helm-rg--current-overlays nil))

(defun helm-rg--on-file-line-p (parsed-output)
  (cl-destructuring-bind (&key file line-num match-results) parsed-output
    (null line-num)))

(defun helm-rg--collect-lines-matches-current-file (orig-line-parsed file-abs-path)
  "Collect all matches from ripgrep's highlighted output from from FILE-ABS-PATH."
  ;; If we are on a file's line, stay where we are, otherwise back up to the closest file line above
  ;; the current line (this is the file that "owns" the entry).
  (let ((orig-file (plist-get orig-line-parsed :file)))
    ;; If the file path matches `helm-rg--only-current-line-match-highlight-files-regexp', just
    ;; highlight the matches for the current line, if not on a file line.
    (if (and (stringp helm-rg--only-current-line-match-highlight-files-regexp)
             (string-match-p helm-rg--only-current-line-match-highlight-files-regexp file-abs-path))
        (list (list :match-line-num (plist-get orig-line-parsed :line-num)
                    :line-match-results (plist-get orig-line-parsed :match-results)))
      ;; Otherwise, collect all the results on all matching lines of the file.
      (with-helm-buffer
        (save-excursion
          (if (helm-rg--on-file-line-p orig-line-parsed)
              (beginning-of-line)
            ;; Go back to the owning file's line.
            (re-search-backward (rx (: bol (not digit)))))
          (cl-loop
           do (forward-line 1)
           for cur-line = (helm-rg--current-line-contents)
           for result = (helm-rg--process-transition orig-file cur-line)
           for line-content = (plist-get result :line-content)
           while line-content
           for parsed-line = (helm-rg--get-jump-location-from-line line-content)
           for match-results = (plist-get parsed-line :match-results)
           while match-results
           do (cl-assert (string= orig-file (plist-get result :file-path)))
           for line-num = (plist-get parsed-line :line-num)
           collect (list :match-line-num line-num :line-match-results match-results)))))))

(defun helm-rg--convert-lines-matches-to-overlays (line-match-results)
  (beginning-of-line)
  (--map (cl-destructuring-bind (&key beg end) it
           (helm-rg--make-overlay-with-face
            (+ (point) beg) (+ (point) end)
            'helm-rg-preview-match-highlight))
         line-match-results))

(defun helm-rg--make-match-overlays-for-result (cur-file-matches)
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     with cur-line = 0
     for line-match-set in cur-file-matches
     append (cl-destructuring-bind (&key match-line-num line-match-results)
                line-match-set
              (let ((lines-diff (- match-line-num cur-line)))
                (cl-assert (> lines-diff 0))
                (forward-line lines-diff)
                (incf cur-line lines-diff)
                (cl-assert (not (eobp)))
                (helm-rg--convert-lines-matches-to-overlays line-match-results))))))

(defun helm-rg--async-action (parsed-output)
  "Visit the file at the line and column specified by CAND.
The match is highlighted in its buffer."
  (let ((default-directory helm-rg--current-dir)
        (helm-rg--display-buffer-method
         (or helm-rg--display-buffer-method
             (if helm-current-prefix-arg helm-rg-display-buffer-alternate-method
               helm-rg-display-buffer-normal-method))))
    (helm-rg--delete-overlays)
    (cl-destructuring-bind (&key file line-num match-results) parsed-output
      (let* ((file-abs-path
              (expand-file-name file))
             (buffer-to-display
              (or (when-let ((visiting-buf (find-buffer-visiting file-abs-path)))
                    ;; TODO: prompt to save the buffer if modified? something?
                    visiting-buf)
                  (let ((new-buf (find-file-noselect file-abs-path)))
                    (when helm-rg--append-persistent-buffers
                      (push new-buf helm-rg--cur-persistent-bufs))
                    new-buf)))
             (cur-file-matches
              (helm-rg--collect-lines-matches-current-file parsed-output file-abs-path)))
        (funcall helm-rg--display-buffer-method buffer-to-display)
        (let ((match-olays (helm-rg--make-match-overlays-for-result cur-file-matches)))
          (goto-char (point-min))
          (helm-rg--get-optional-typed natnum line-num
            (forward-line it))
          (let ((line-olay
                 (helm-rg--make-overlay-with-face (line-beginning-position) (line-end-position)
                                                  'helm-rg-preview-line-highlight)))
            (setq helm-rg--current-overlays (cons line-olay match-olays))))
        ;; Move to the first match in the line (all lines have >= 1 match because ripgrep only
        ;; outputs matching lines).
        (let ((first-match-beginning (plist-get (car match-results) :beg)))
          (helm-rg--get-optional-typed natnum first-match-beginning
            (forward-char it)))
        (recenter)))))

(defun helm-rg--async-persistent-action (parsed-output)
  "Visit the file at the line and column specified by CAND.
Call `helm-rg--async-action', but push the buffer corresponding to CAND to
`helm-rg--current-overlays', if there was no buffer visiting it already."
  (let ((helm-rg--append-persistent-buffers t)
        (helm-rg--display-buffer-method helm-rg--persistent-action-display-buffer-method))
    (helm-rg--async-action parsed-output)))

(defun helm-rg--kill-proc-if-live (proc-name)
  "Delete the process named PROC-NAME, if it is alive."
  (let ((proc (get-process proc-name)))
    (when (process-live-p proc)
      (delete-process proc))))

(defun helm-rg--kill-bufs-if-live (&rest bufs)
  "Kill any live buffers in BUFS."
  (mapc
   (lambda (buf)
     (when (buffer-live-p (get-buffer buf))
       (kill-buffer buf)))
   bufs))

(defun helm-rg--unwind-cleanup ()
  "Reset all the temporary state in `defvar's in this package."
  (helm-rg--delete-overlays)
  (cl-loop
   for opened-buf in helm-rg--cur-persistent-bufs
   unless (eq (current-buffer) opened-buf)
   do (kill-buffer opened-buf)
   finally (setq helm-rg--cur-persistent-bufs nil))
  (helm-rg--kill-proc-if-live helm-rg--process-name)
  (helm-rg--kill-bufs-if-live helm-rg--buffer-name
                              helm-rg--process-buffer-name
                              helm-rg--error-buffer-name)
  (setq helm-rg--glob-string nil
        helm-rg--paths-to-search nil
        helm-rg--case-sensitivity nil))

(defun helm-rg--do-helm-rg (rg-pattern)
  "Invoke ripgrep to search for RG-PATTERN, using `helm'."
  (helm :sources '(helm-rg-process-source)
        :buffer helm-rg--buffer-name
        :input rg-pattern
        :prompt "rg pattern: "))

(defun helm-rg--get-thing-at-pt ()
  "Get the object surrounding point, or the empty string."
  (helm-aif (thing-at-point helm-rg-thing-at-point)
      (substring-no-properties it)
    ""))

(defun helm-rg--header-name (src-name)
  (format "%s %s @ %s"
          (propertize "rg" 'face 'bold-italic)
          src-name
          helm-rg--current-dir))

(defun helm-rg--get-jump-location-from-line (line)
  "Get the value of `helm-rg--jump-location-text-property' at the start of LINE."
  ;; When there is an empty pattern, the argument can be nil due to the way helm handles our dummy
  ;; process. There may be a way to avoid having to do this check.
  (when line
    (get-text-property 0 helm-rg--jump-location-text-property line)))

(defun helm-rg--display-to-real (_)
  "Extract the information from the process filter stored in the current entry's text properties.

Note that this doesn't use the argument at all. I don't think you can get the currently selected
line without the text properties scrubbed using helm without doing this."
  (helm-rg--get-jump-location-from-line (helm-get-selection nil 'withprop)))

(defun helm-rg--collect-matches (regexp)
  (cl-loop while (re-search-forward regexp nil t)
           collect (match-string 1)))

(defun helm-rg--pattern-transformer (pattern)
  "Transform PATTERN (the `helm-input') into a Perl-compatible regular expression."
  ;; For example: "a  b c" => "a b.*c|c.*a b".
  (->>
   ;; Split the pattern into our definition of "components". Suppose PATTERN is "a  b c". Then:
   ;; "a  b c" => '("a  b" "c")
   (helm-rg--into-temp-buffer pattern
     (helm-rg--collect-matches helm-rg--loop-input-pattern-regexp))
   ;; Two spaces in a row becomes a single space in the output regexp. Each component is now a
   ;; regexp.
   ;; '("a  b" "c") => '("a b" "c")
   (--map (replace-regexp-in-string (rx (= 2 ? )) " " it))
   ;; All permutations of all component regexps.
   ;; '("a b" "c") => '(("a b" "c") ("c" "a b"))
   (-permutations)
   ;; Each permutation is converted into a regexp which matches a line containing each regexp in
   ;; the permutation in order, each separated by 0 or more non-newline characters.
   ;; '(("a b" "c") ("c" "a b")) => '("a  b.*c" "c.*a  b")
   (--map (helm-rg--join ".*" it))
   ;; '("a  b.*c" "c.*a  b") => "a b.*c|c.*a b"
   (helm-rg--join "|")))

(defun helm-rg--advance-forward ()
  (interactive)
  (if (helm-end-of-source-p)
      (helm-beginning-of-buffer)
    (helm-next-line)))

(defun helm-rg--advance-backward ()
  (interactive)
  (if (helm-beginning-of-source-p)
      (helm-end-of-buffer)
    (helm-previous-line)))

(defun helm-rg--iterate-results (direction success-fn failure-fn)
  (with-helm-window
    (let ((helm-move-to-line-cycle-in-source t)
          (move-fn
           (pcase-exhaustive direction
             (`forward #'helm-rg--advance-forward)
             (`backward #'helm-rg--advance-backward))))
      (call-interactively move-fn)
      (cl-loop
       for cur-line-parsed = (helm-rg--get-jump-location-from-line (helm-rg--current-line-contents))
       until (funcall success-fn cur-line-parsed)
       if (funcall failure-fn cur-line-parsed)
       return (message "%s" "failure: could not cycle to next entry")
       else do (call-interactively move-fn)))))

(defun helm-rg--current-line-contents ()
  "`helm-current-line-contents' doesn't keep text properties."
  (buffer-substring (point-at-bol) (point-at-eol)))

(cl-defun helm-rg--nullable-states-different (a b &key (cmp #'eq))
  "Compare A and B respecting nullability using CMP.

When CMP is `string=', the following results:
(A=nil, B=nil) => nil
(A=\"a\", B=nil) => t
(A=nil, B=\"a\") => t
(A=\"a\", B=\"a\") => nil
(A=\"a\", B=\"b\") => t"
  (if a
      (not (and b (funcall cmp a b)))
    b))

(defun helm-rg--on-same-entry (orig-line-parsed cur-line-parsed)
  (let ((orig-file (plist-get orig-line-parsed :file))
        (cur-file (plist-get cur-line-parsed :file))
        (orig-line-num (plist-get orig-line-parsed :line-num))
        (cur-line-num (plist-get cur-line-parsed :line-num)))
    (and (not (helm-rg--nullable-states-different orig-file cur-file :cmp #'string=))
         (not (helm-rg--nullable-states-different orig-line-num cur-line-num :cmp #'=)))))

(defun helm-rg--move-file (direction)
  "Move through matching lines from ripgrep in the given DIRECTION.

This will loop around the results when advancing past the beginning or end of the results."
  (let* ((orig-line (helm-rg--current-line-contents))
         (orig-line-parsed (helm-rg--get-jump-location-from-line orig-line)))
    (helm-rg--iterate-results
     direction
     #'helm-rg--on-file-line-p
     (lambda (cur-line-parsed)
       (helm-rg--on-same-entry orig-line-parsed cur-line-parsed)))))

(defun helm-rg--file-forward ()
  (interactive)
  (helm-rg--move-file 'forward))

(defun helm-rg--file-backward ()
  (interactive)
  (helm-rg--move-file 'backward))

(defun helm-rg--process-output (exe &rest args)
  "Get output from a process specified by string arguments.
Merges stdout and stderr, and trims whitespace from the result."
  (with-temp-buffer
    (let ((proc (make-process
                 :name "temp-proc"
                 :buffer (current-buffer)
                 :command `(,exe ,@args)
                 :sentinel #'ignore)))
      (while (accept-process-output proc nil nil t)))
    (trim-whitespace (buffer-string))))

(defun helm-rg--check-directory-path (path)
  (if (and path (file-directory-p path)) path
    (error "path '%S' was not a directory." path)))

(defun helm-rg--make-help-buffer (help-buf-name)
  (with-current-buffer (get-buffer-create help-buf-name)
    (read-only-mode -1)
    (erase-buffer)
    (fundamental-mode)
    (insert (helm-rg--process-output helm-rg-ripgrep-executable "--help"))
    (goto-char (point-min))
    (read-only-mode 1)
    (current-buffer)))

(defun helm-rg--lookup-color (color)
  (helm-rg--alist-get-exhaustive color helm-rg--color-format-argument-alist))

(defun helm-rg--lookup-style (style)
  (helm-rg--alist-get-exhaustive style helm-rg--style-format-argument-alist))

(defun helm-rg--construct-match-color-format-arguments (color style)
  (list
   (format "--colors=match:fg:%s"
           (plist-get (helm-rg--lookup-color color) :cmd-line))
   (format "--colors=match:style:%s"
           (plist-get (helm-rg--lookup-style style) :cmd-line))))

(defun helm-rg--construct-match-text-properties (color style)
  `(,(plist-get (helm-rg--lookup-style style) :text-property)
    (foreground-color . ,(plist-get (helm-rg--lookup-color color) :text-property))))

(defun helm-rg--is-match (position object)
  (let ((text-props-for-position (get-text-property position 'font-lock-face object))
        (text-props-for-match
         (helm-rg--construct-match-text-properties helm-rg-match-color helm-rg-match-style)))
    (equal text-props-for-position text-props-for-match)))

(defun helm-rg--first-match-start-ripgrep-output (position match-line &optional find-end)
  (cl-loop
   with line-char-index = position
   for is-match-p = (helm-rg--is-match line-char-index match-line)
   until (if find-end (not is-match-p) is-match-p)
   for next-chg = (next-single-property-change line-char-index 'font-lock-face match-line)
   if next-chg do (setq line-char-index next-chg)
   else return (if find-end
                   ;; char at end of line is end of match
                   (length match-line)
                 nil)
   finally return line-char-index))

(defun helm-rg--parse-match-regions-from-match-line (match-line)
  (cl-loop
   with line-char-index = 0
   for match-beg = (helm-rg--first-match-start-ripgrep-output line-char-index match-line)
   while match-beg
   for match-end = (helm-rg--first-match-start-ripgrep-output match-beg match-line t)
   do (setq line-char-index match-end)
   collect (list :beg match-beg :end match-end)))

(defun helm-rg--process-transition (cur-file line)
  (cond
   ((string= line "") (list :file-path nil))
   ((and cur-file (string-match helm-rg--numbered-text-line-regexp line))
    (let* ((whole-line (match-string 0 line))
           (line-num (string-to-number (match-string 1 line)))
           (content (match-string 2 line))
           (jump-to (list :file cur-file
                          :line-num (1- line-num)
                          :match-results (helm-rg--parse-match-regions-from-match-line content)))
           (output-line
            (propertize whole-line helm-rg--jump-location-text-property jump-to)))
      (list :file-path cur-file
            :line-content output-line)))
   ((string-match helm-rg--output-new-file-line-regexp line)
    (let* ((whole-line (match-string 0 line))
           (file-path (match-string 1 line))
           (jump-to (list :file file-path))
           (output-line
            (propertize whole-line helm-rg--jump-location-text-property jump-to)))
      (list :file-path file-path
            :line-content output-line)))))

(defun helm-rg--maybe-get-line (content)
  (helm-rg--into-temp-buffer content
    (if (re-search-forward (rx (: (group (*? anything)) "\n")) nil t)
        (list :line (match-string 1)
              :rest (buffer-substring (point) (point-max)))
      (list :line nil
            :rest (buffer-string)))))

(defun helm-rg--parse-process-output (input-line)
  (let ((colored-line (ansi-color-apply input-line)))
    (cl-destructuring-bind (&key cur-file) helm-rg--process-output-parse-state
      (if-let ((parsed (helm-rg--process-transition cur-file colored-line)))
          (cl-destructuring-bind (&key file-path line-content) parsed
            (setq helm-rg--process-output-parse-state
                  (list :cur-file file-path))
            (or line-content ""))
        (error "line '%s' could not be parsed! state was: '%S'"
               colored-line helm-rg--process-output-parse-state)))))

(defun helm-rg--edit-results ()
  (interactive)
  ;;
  )


;; Toggles and settings
(defmacro helm-rg--run-after-exit (&rest body)
  "Wrap BODY in `helm-run-after-exit'."
  `(helm-run-after-exit (lambda () ,@body)))

(defun helm-rg--set-glob ()
  "Set the glob string used to invoke ripgrep and search again."
  (interactive)
  (let* ((pat helm-pattern)
         (start-dir helm-rg--current-dir))
    (helm-rg--run-after-exit
     (let ((helm-rg--current-dir start-dir)
           (helm-rg--glob-string
            (read-string
             "rg glob: " helm-rg--glob-string 'helm-rg--glob-string-history)))
       (helm-rg--do-helm-rg pat)))))

(defun helm-rg--set-dir ()
  "Set the directory in which to invoke ripgrep and search again."
  (interactive)
  (let ((pat helm-pattern))
    (helm-rg--run-after-exit
     (let ((helm-rg--current-dir
            (read-directory-name "rg directory: " helm-rg--current-dir nil t)))
       (helm-rg--do-helm-rg pat)))))

(defun helm-rg--is-executable-file (path)
  (and path
       (file-executable-p path)
       (not (file-directory-p path))))

(defun helm-rg--get-git-root ()
  (if (helm-rg--is-executable-file helm-rg-git-executable)
      (helm-rg--process-output helm-rg-git-executable
                               "rev-parse" "--show-toplevel")
    (error "helm-rg-git-executable is not an executable file (was: %S)."
           helm-rg-git-executable)))

(defun helm-rg--interpret-starting-dir (default-directory-spec)
  (pcase-exhaustive default-directory-spec
    ('default default-directory)
    ('git-root (helm-rg--get-git-root))
    ((pred stringp) (helm-rg--check-directory-path))))

(defun helm-rg--set-case-sensitivity ()
  (interactive)
  (let ((pat helm-pattern)
        (start-dir helm-rg--current-dir))
    (helm-rg--run-after-exit
     ;; TODO: see if all of this rebinding of the defvars is necessary, and if it must occur then
     ;; make it part of the `helm-rg--run-after-exit' macro.
     (let* ((helm-rg--current-dir start-dir)
            (all-sensitivity-keys
             (helm-rg--alist-keys helm-rg--case-sensitive-argument-alist))
            (sensitivity-selection
             (completing-read "Choose case sensitivity: " all-sensitivity-keys nil t))
            (helm-rg--case-sensitivity (intern sensitivity-selection)))
       (helm-rg--do-helm-rg pat)))))


;; Keymap
(defconst helm-rg-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g") #'helm-rg--set-glob)
    (define-key map (kbd "M-d") #'helm-rg--set-dir)
    (define-key map (kbd "M-c") #'helm-rg--set-case-sensitivity)
    (define-key map (kbd "M-e") #'helm-rg--edit-results)
    (define-key map (kbd "<right>") #'helm-rg--file-forward)
    (define-key map (kbd "<left>") #'helm-rg--file-backward)
    map)
  "Keymap for `helm-rg'.")


;; Helm sources
(defconst helm-rg-process-source
  ;; `helm-grep-ag-class' is provided by `helm' -- I don't know if that identifier is stable but if
  ;; it's not it will error out very quickly (because `helm-make-source' will fail if that symbol
  ;; is removed).
  (helm-make-source "ripgrep" 'helm-grep-ag-class
    :header-name #'helm-rg--header-name
    :keymap 'helm-rg-map
    :history 'helm-rg--input-history
    :help-message "FIXME: useful help message!!!"
    :candidates-process #'helm-rg--make-process
    :action (helm-make-actions "Visit" #'helm-rg--async-action)
    :filter-one-by-one #'helm-rg--parse-process-output
    :display-to-real #'helm-rg--display-to-real
    :pattern-transformer #'helm-rg--pattern-transformer
    :persistent-action #'helm-rg--async-persistent-action
    :persistent-help "Visit result buffer and highlight matches"
    :requires-pattern nil
    :group 'helm-rg)
  "Helm async source to search files in a directory using ripgrep.")


;; Meta-programmed defcustom forms
(helm-rg--defcustom-from-alist helm-rg-default-case-sensitivity
    helm-rg--case-sensitive-argument-alist
  "Case sensitivity to use in ripgrep searches.

This is the default value for `helm-rg--case-sensitivity', which can be modified with
`helm-rg--set-case-sensitivity' during a `helm-rg' session.

This must be an element of `helm-rg--case-sensitive-argument-alist'.")

(helm-rg--defcustom-from-alist helm-rg-match-color helm-rg--color-format-argument-alist
  "Color to use for match results from ripgrep.

This must be synchronized between ripgrep and elisp so that `helm-rg' can parse the match locations
from the ripgrep highlights, so it must be an element of `helm-rg--color-format-argument-alist'.")

(helm-rg--defcustom-from-alist helm-rg-match-style helm-rg--style-format-argument-alist
  "Style to use for match results from ripgrep.

This must be synchronized between ripgrep and elisp so that `helm-rg' can parse the match locations
from the ripgrep highlights, so it must be an element of `helm-rg--style-format-argument-alist'.")


;; Autoloaded functions
;;;###autoload
(defun helm-rg (rg-pattern &optional pfx paths)
  "Search for the PCRE regexp RG-PATTERN extremely quickly with ripgrep.

When invoked interactively with a prefix argument, or when PFX is non-nil,
set the cwd for the ripgrep process to `default-directory'. Otherwise use the
cwd as described by `helm-rg-default-directory'.

If PATHS is non-nil, ripgrep will search only those paths, relative to the
process's cwd. Otherwise, the process's cwd will be searched.

Note that ripgrep respects glob patterns from .gitignore, .rgignore, and .ignore
files. This composes with the glob defined by `helm-rg-default-glob-string', or
overridden with `helm-rg--set-glob', which is defined in `helm-rg-map'.

The ripgrep command's help output can be printed into its own buffer for reference
with the interactive command `helm-rg-display-help'.

\\{helm-rg-map}"
  (interactive (list (helm-rg--get-thing-at-pt) current-prefix-arg nil))
  (let* ((helm-rg--current-dir
          (or helm-rg--current-dir
              (and pfx default-directory)
              (helm-rg--interpret-starting-dir helm-rg-default-directory)))
         (helm-rg--glob-string
          (or helm-rg--glob-string
              helm-rg-default-glob-string))
         (helm-rg--paths-to-search
          (or helm-rg--paths-to-search
              paths))
         (helm-rg--case-sensitivity
          (or helm-rg--case-sensitivity
              helm-rg-default-case-sensitivity)))
    (unwind-protect (helm-rg--do-helm-rg rg-pattern)
      (helm-rg--unwind-cleanup))))

;;;###autoload
(defun helm-rg-display-help (&optional pfx)
  "Display a buffer with the ripgrep command's usage help.

The help buffer will be reused if it was already created. A prefix argument when
invoked interactively, or a non-nil value for PFX, will display the help buffer
in the current window. Otherwise, if the help buffer is already being displayed
in some window, select that window, or else display the help buffer with
`pop-to-buffer'."
  (interactive "P")
  (let ((filled-out-help-buf
         (or (get-buffer helm-rg--ripgrep-help-buffer-name)
             (helm-rg--make-help-buffer helm-rg--ripgrep-help-buffer-name))))
    (if pfx (switch-to-buffer filled-out-help-buf)
      (-if-let ((buf-win (get-buffer-window filled-out-help-buf t)))
          (select-window buf-win)
        (pop-to-buffer filled-out-help-buf)))))

(provide 'helm-rg)
;;; helm-rg.el ends here
