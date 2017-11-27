;; Copyright (C) 2016  Vibhav Pant <vibhavp@gmail.com>  -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'json)
(require 'xref)
(require 'subr-x)
(require 'widget)
(require 'lsp-receive)
(require 'lsp-common)
(require 'pcase)
(require 'inline)

;;; Code:

(cl-defstruct lsp--client
  (language-id nil :read-only t)
  ;; function to send a message and waits for the next message from the server
  (send-sync nil :read-only t)
  ;; function to send a message and not wait for the next response
  (send-async nil :read-only t)
  (type nil :read-only t)
  (new-connection nil :read-only t)
  (get-root nil :read-only t)
  (ignore-regexps nil :read-only t)

  (notification-handlers (make-hash-table :test 'equal) :read-only t)
  (request-handlers (make-hash-table :test 'equal) :read-only t)
  (response-handlers (make-hash-table :test 'eq) :read-only t)

  (string-renderers '())
  (last-id 0))

(cl-defstruct lsp--workspace
  (parser nil :read-only t)
  ;; file-versions is a hashtable of files "owned" by the workspace
  (file-versions nil)
  (server-capabilities nil)
  (root nil :ready-only t)
  (client nil :read-only t)
  (change-timer-disabled nil)
  (proc nil) ;; the process we communicate with
  (cmd-proc nil) ;; the process we launch initially
  (buffers nil) ;; a list of buffers associated with this workspace
  (highlight-overlays nil) ;; a list of overlays used for highlighting the symbol under point

  ;; Extra client capabilities provided by third-party packages using
  ;; `lsp-register-client-capabilities'. It's value is an alist of (PACKAGE-NAME
  ;; . CAPS), where PACKAGE-NAME is a symbol of the third-party package name,
  ;; and CAPS is either a plist of the client capabilities, or a function that
  ;; takes no argument and returns a plist of the client capabilities or nil.")
  (extra-client-capabilities nil))

(defvar-local lsp--cur-workspace nil)
(defvar lsp--workspaces (make-hash-table :test #'equal)
  "Table of known workspaces, indexed by the project root directory.")

(defvar lsp--ignored-workspace-roots (make-hash-table :test #'equal)
  "Table of project roots which should not have a workspace,
indexed by the project root directory.

This is populated when the user declines to open a workspace
for a file in the workspace.")

(defcustom lsp-before-initialize-hook nil
  "List of functions to be called before a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-initialize-hook nil
  "List of functions to be called after a Language Server has been initialized
for a new workspace."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-before-open-hook nil
  "List of functions to be called before a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defcustom lsp-after-open-hook nil
  "List of functions to be called after a new file with LSP support is opened."
  :type 'hook
  :group 'lsp-mode)

(defvar lsp--sync-methods
  '((0 . none)
    (1 . full)
    (2 . incremental)))
(defvar-local lsp--server-sync-method nil
  "Sync method recommended by the server.")

;;;###autoload
(defgroup lsp-mode nil
  "Customization group for lsp-mode."
  :group 'tools)

;;;###autoload
(defgroup lsp-faces nil
  "Faces for lsp-mode."
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-document-sync-method nil
  "How to sync the document with the language server."
  :type '(choice (const :tag "Documents should not be synced at all." 'none)
                 (const :tag "Documents are synced by always sending the full content of the document." 'full)
                 (const :tag "Documents are synced by always sending incremental changes to the document." 'incremental)
                 (const :tag "Use the method recommended by the language server." nil))
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-project-blacklist nil
  "A list of project directories for which LSP shouldn't be initialized."
  :type '(repeat directory)
  :group 'lsp-mode)

(defcustom lsp-project-whitelist nil
  "A list of project directories for which LSP shouldn be
initialized. When set this turns off use of
`lsp-project-blacklist'"
  :type '(repeat directory)
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-eldoc t
  "Enable `eldoc-mode' integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-highlight-symbol-at-point t
  "Highlight the symbol under the point."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-codeaction t
  "Enable code action processing."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-completion-at-point t
  "Enable `completion-at-point' integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-xref t
  "Enable xref integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-flycheck t
  "Enable flycheck integration."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defcustom lsp-enable-indentation t
  "Indent regions using the file formatting functionality provided by the language server."
  :type 'boolean
  :group 'lsp-mode)

;;;###autoload
(defface lsp-face-highlight-textual
  '((t :background "yellow"))
  "Face used for textual occurances of symbols."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-highlight-read
  '((t :background "red"))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

;;;###autoload
(defface lsp-face-highlight-write
  '((t :background "green"))
  "Face used for highlighting symbols being written to."
  :group 'lsp-faces)

(define-error 'lsp-error "Unkown LSP error")
(define-error 'lsp-empty-response-error
  "Recived empty response from language server" 'lsp-error)
(define-error 'lsp-capability-not-supported
  "Capability is not supported by this language server" 'lsp-error)

(defun lsp-client-on-notification (client method callback)
  (cl-check-type client lsp--client)
  (cl-check-type method string)
  (cl-check-type callback function)
  (puthash method callback (lsp--client-notification-handlers client)))

(defun lsp-client-on-request (client method callback)
  (cl-check-type client lsp--client)
  (cl-check-type method string)
  (cl-check-type callback function)
  (puthash method callback (lsp--client-request-handlers client)))

(define-inline lsp--make-request (method &optional params)
  "Create request body for method METHOD and parameters PARAMS."
  (inline-quote
    (plist-put (lsp--make-notification ,method ,params)
      :id (cl-incf (lsp--client-last-id (lsp--workspace-client lsp--cur-workspace))))))

(defun lsp--make-response-error (code message data)
  (cl-check-type code number)
  (cl-check-type message string)
  `(:code ,code :message ,message :data ,data))

(defun lsp--make-response (id result error)
  (cl-check-type error list)
  `(:jsonrpc "2.0" :id ,id :result ,result :error ,error))

(define-inline lsp--make-notification (method &optional params)
  "Create notification body for method METHOD and parameters PARAMS."
  (inline-quote
    (progn (cl-check-type ,method string)
      (list :jsonrpc "2.0" :method ,method :params ,params))))

(defun lsp--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-false :json-false)
         (body (json-encode params)))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))

(defun lsp--send-notification (body)
  "Send BODY as a notification to the language server."
  (funcall (lsp--client-send-async (lsp--workspace-client lsp--cur-workspace))
           (lsp--make-message body)
           (lsp--workspace-proc lsp--cur-workspace)))

(define-inline lsp--cur-workspace-check ()
  (inline-quote (cl-assert lsp--cur-workspace nil
                  "No language server is associated with this buffer.")))

(define-inline lsp--cur-parser ()
  (inline-quote (lsp--workspace-parser lsp--cur-workspace)))

(defun lsp--send-request (body &optional no-wait)
  "Send BODY as a request to the language server, get the response.
If NO-WAIT is non-nil, don't synchronously wait for a response."
  ;; lsp-send-sync should loop until lsp--from-server returns nil
  ;; in the case of Rust Language Server, this can be done with
  ;; 'accept-process-output`.'
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
         (parser (lsp--cur-parser))
         (send-func (if no-wait
                        (lsp--client-send-async client)
                      (lsp--client-send-sync client))))
    (setf (lsp--parser-waiting-for-response parser) (not no-wait))
    (funcall send-func
             (lsp--make-message body)
             (lsp--workspace-proc lsp--cur-workspace))
    (when (not no-wait)
      (prog1 (lsp--parser-response-result parser)
        (setf (lsp--parser-response-result parser) nil)))))

(defun lsp--send-request-async (body callback)
  "Send BODY as a request to the language server, and call CALLBACK with
the response recevied from the server asynchronously."
  (let ((client (lsp--workspace-client lsp--cur-workspace))
        (id (plist-get body :id)))
    (cl-assert id nil "body missing id field")
    (puthash id callback (lsp--client-response-handlers client))
    (funcall (lsp--client-send-async client) (lsp--make-message body)
      (lsp--workspace-proc lsp--cur-workspace))
    body))

(define-inline lsp--inc-cur-file-version ()
  (inline-quote (cl-incf (gethash buffer-file-name
                           (lsp--workspace-file-versions lsp--cur-workspace)))))

(define-inline lsp--cur-file-version ()
  "Return the file version number.  If INC, increment it before."
  (inline-quote
    (gethash buffer-file-name (lsp--workspace-file-versions lsp--cur-workspace))))

(define-inline lsp--make-text-document-item ()
  "Make TextDocumentItem for the currently opened file.

interface TextDocumentItem {
    uri: string; // The text document's URI
    languageId: string; // The text document's language identifier.
    version: number;
    text: string;
}"
  (inline-quote
    (let ((language-id-fn (lsp--client-language-id (lsp--workspace-client lsp--cur-workspace))))
      (list :uri (concat "file://" buffer-file-name)
	      :languageId (funcall language-id-fn (current-buffer))
	      :version (lsp--cur-file-version)
	      :text (buffer-substring-no-properties (point-min) (point-max))))))

(defun lsp--shutdown-cur-workspace ()
  "Shut down the language server process for ‘lsp--cur-workspace’."
  (with-demoted-errors "LSP error: %S"
    (lsp--send-request (lsp--make-request "shutdown" (make-hash-table)) t)
    (lsp--send-notification (lsp--make-notification "exit" nil)))
  (lsp--uninitialize-workspace))

;; Clean up the entire state of lsp mode when Emacs is killed, to get rid of any
;; pending language servers.
(add-hook 'kill-emacs-hook #'lsp--global-teardown)

(defun lsp--global-teardown ()
  (maphash (lambda (_k value) (lsp--teardown-client value)) lsp--workspaces))

(defun lsp--teardown-client (client)
  (setq lsp--cur-workspace client)
  (lsp--shutdown-cur-workspace))

(defun lsp--uninitialize-workspace ()
  "When a workspace is shut down, by request or from just
disappearing, unset all the variables related to it."
  (remhash (lsp--workspace-root lsp--cur-workspace) lsp--workspaces)
  (let (proc)
    (with-current-buffer (current-buffer)
      (setq proc (lsp--workspace-proc lsp--cur-workspace))
      (unless (eq (process-status proc) 'exit)
        (kill-process (lsp--workspace-proc lsp--cur-workspace)))
      (setq lsp--cur-workspace nil)
      (lsp--unset-variables)
      (kill-local-variable 'lsp--cur-workspace))))

;; NOTE: Possibly make this function subject to a setting, if older LSP servers
;; are unhappy
(defun lsp--client-capabilities ()
  "Return the client capabilites."
  (apply #'lsp--merge-plists
    `(:workspace    ,(lsp--client-workspace-capabilities)
       :textDocument ,(lsp--client-textdocument-capabilities))
    (seq-map (lambda (extra-capabilities-cons)
               (let* ((package-name (car extra-capabilities-cons))
                       (value (cdr extra-capabilities-cons))
                       (capabilities (if (functionp value) (funcall value)
                                       value)))
                 (if (and capabilities (not (listp capabilities)))
                   (progn
                     (message "Capabilities provided by %s are not a plist: %s" package-name value)
                     nil)
                   capabilities)))
      (lsp--workspace-extra-client-capabilities lsp--cur-workspace))))

(defun lsp--merge-plists (first &rest rest)
  "Deeply merge plists.

FIRST is the plist to be merged into. The rest of the arguments
can be either plists or nil. The non-nil plists in the rest of
the arguments will be merged into FIRST.

Return the merged plist."
  (cl-check-type first list)
  (seq-each (lambda (pl)
              (setq first
                (lsp--merge-two-plists first pl)))
    rest)
  first)

(defun lsp--merge-two-plists (first second)
  "Deeply merge two plists.

All values in SECOND are merged into FIRST. FIRST can be nil or a
plist. SECOND must be a plist.

Return the merged plist."
  (when second
    (if (not (listp second))
      (warn "Cannot merge non-list value into a plist. The value is %s" second)
      (cl-loop for (key second-value) on second
        collect (progn
                  (let ((first-value (plist-get first key))
                         merged-value)
                    (cond
                      ((null second-value)) ; do nothing
                      ((null first-value)
                        (if (listp second-value)
                          ;; Deep copy second-value so that the original value won't
                          ;; be modified.
                          (setq merged-value
                            (lsp--merge-two-plists nil second-value)))
                        (setq merged-value second-value))
                      ((and (listp first-value) (listp second-value))
                        (setq merged-value (lsp--merge-two-plists first-value second-value)))
                      ;; Otherwise, the first value is a leaf entry and should
                      ;; not be overridden.
                      )
                    (when merged-value
                      (setq first (plist-put first key merged-value))))))))
  first)

(defun lsp--client-workspace-capabilities ()
  "Client Workspace capabilities according to LSP."
  `(:executeCommand (:dynamicRegistration t)))

(defun lsp--client-textdocument-capabilities ()
  "Client Text document capabilities according to LSP."
  `(:synchronization (:willSave t :didSave t)))

(defun lsp-register-client-capabilities (package-name caps)
  "Register extra client capabilities for the current workspace.

This function must be called before the initialize request is
sent. It's recommended to to call it in the
`lsp-before-initialize-hook'.

PACKAGE name is the symbol of the name of the package that
registers the capabilities. CAPS is either a plist of the
capabilities, or a function that takes no argument and return a
plist of the client capabilties or nil.

Registered capabilities are merged into the default capabilities
before sending to the server via the initialize request. If two
packages provide different values for the same leaf capability
entry, the value is set to the one that registers later. Default
leaf capability entries can not be overwritten."
  (lsp--cur-workspace-check)
  (cl-check-type package-name symbolp)
  (cl-check-type package-name (or list function))
  (let ((extra-client-capabilities
          (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))
    (if (alist-get package-name extra-client-capabilities)
        (message "%s has already registered client capabilities" package-name)
      (push `(,package-name . ,caps)
        (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))))

(defun lsp-unregister-client-capabilities (package-name)
  "Unregister extra capabilities provided by PACKAGE-NAME for the current workspace.

PACKAGE-NAME is a symbol of the name of the package that has
registered client capabilities by calling
`lsp-register-client-capabilities'."
  (lsp--cur-workspace-check)
  (cl-check-type package-name symbol)
  (let ((extra-client-capabilities
          (lsp--workspace-extra-client-capabilities lsp--cur-workspace)))
    (setf (lsp--workspace-extra-client-capabilities lsp--cur-workspace)
      (assq-delete-all package-name extra-client-capabilities))))

(define-inline lsp--server-capabilities ()
  "Return the capabilities of the language server associated with the buffer."
  (inline-quote (lsp--workspace-server-capabilities lsp--cur-workspace)))

(defun lsp--set-sync-method ()
  (let* ((sync (gethash "textDocumentSync" (lsp--server-capabilities)))
         (kind (if (hash-table-p sync) (gethash "change" sync) sync))
         (method (alist-get kind lsp--sync-methods))
         )
    (setq lsp--server-sync-method (or lsp-document-sync-method
                                      method))))

(defun lsp--workspace-apply-edit-handler (_workspace params)
  (lsp--apply-workspace-edits (gethash "edit" params))
  ;; TODO: send reply
  )

(defun lsp--make-sentinel (buffer)
  (lambda (_p exit-str)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (buf (lsp--workspace-buffers lsp--cur-workspace))
          (with-current-buffer buf
            (message "%s: %s has exited (%s)"
                     (lsp--workspace-root lsp--cur-workspace)
                     (process-name (lsp--workspace-proc lsp--cur-workspace))
                     exit-str)
            (lsp--uninitialize-workspace)))))))

(defun lsp--should-start-p (root)
  "Consult `lsp-project-blacklist' and `lsp-project-whitelist' to
determine if a server should be started for the given ROOT
directory."
  (if lsp-project-whitelist
      (member root lsp-project-whitelist)
    (not (member root lsp-project-blacklist))))

(defun lsp--start (client &optional extra-init-params)
  (when lsp--cur-workspace
    (user-error "LSP mode is already enabled for this buffer"))
  (let* ((root (funcall (lsp--client-get-root client)))
         (workspace (gethash root lsp--workspaces))
         new-conn response init-params
         parser proc cmd-proc)
    (if workspace
        (setq lsp--cur-workspace workspace)

      (setf
       parser (make-lsp--parser)
       lsp--cur-workspace (make-lsp--workspace
                           :parser parser
                           :file-versions (make-hash-table :test 'equal)
                           :root root
                           :client client)
       (lsp--parser-workspace parser) lsp--cur-workspace
       new-conn (funcall
                 (lsp--client-new-connection client)
                 (lsp--parser-make-filter parser (lsp--client-ignore-regexps client))
                 (lsp--make-sentinel (current-buffer)))
       ;; the command line process invoked
       cmd-proc (if (consp new-conn) (car new-conn) new-conn)
       ;; the process we actually communicate with
       proc (if (consp new-conn) (cdr new-conn) new-conn)

       (lsp--workspace-proc lsp--cur-workspace) proc
       (lsp--workspace-cmd-proc lsp--cur-workspace) cmd-proc)

      (puthash root lsp--cur-workspace lsp--workspaces)
      (run-hooks 'lsp-before-initialize-hook)
      (setq init-params
            `(:processId ,(emacs-pid)
                         :rootPath ,root
                         :rootUri ,(concat "file://" root)
                         :capabilities ,(lsp--client-capabilities)
                         :initializationOptions ,(if (functionp extra-init-params)
                                                     (funcall extra-init-params lsp--cur-workspace)
                                                   extra-init-params)))
      (setf response (lsp--send-request
                      (lsp--make-request "initialize" init-params)))
      (unless response
        (signal 'lsp-empty-response-error (list "initialize")))
      (setf (lsp--workspace-server-capabilities lsp--cur-workspace)
            (gethash "capabilities" response))
      ;; Version 3.0 now sends an "initialized" notification to allow registration
      ;; of server capabilities
      (lsp--send-notification (lsp--make-notification "initialized" nil))
      (run-hooks 'lsp-after-initialize-hook))
    (lsp--text-document-did-open)))

;; Forward-declare variables defined in flycheck.el.
(defvar flycheck-mode)
(defvar flycheck-checkers)
(defvar flycheck-checker)
(defvar flycheck-check-syntax-automatically)

(defun lsp--text-document-did-open ()
  (run-hooks 'lsp-before-open-hook)
  (puthash buffer-file-name 0 (lsp--workspace-file-versions lsp--cur-workspace))
  (push (current-buffer) (lsp--workspace-buffers lsp--cur-workspace))
  (lsp--send-notification (lsp--make-notification
                           "textDocument/didOpen"
                           `(:textDocument ,(lsp--make-text-document-item))))

  (add-hook 'after-save-hook #'lsp-on-save nil t)
  (add-hook 'kill-buffer-hook #'lsp--text-document-did-close nil t)

  (setq-local eldoc-documentation-function #'lsp--on-hover)
  (when lsp-enable-eldoc
    (eldoc-mode 1))

  (when (and lsp-enable-flycheck (featurep 'lsp-flycheck))
    (setq-local flycheck-check-syntax-automatically nil)
    (setq-local flycheck-checker 'lsp)
    (lsp-flycheck-add-mode major-mode)
    (add-to-list 'flycheck-checkers 'lsp)
    (add-hook 'lsp-after-diagnostics-hook (lambda ()
                                            (when flycheck-mode
                                              (flycheck-buffer)))))

  (when (and lsp-enable-indentation
             (lsp--capability "documentRangeFormattingProvider"))
    (setq-local indent-region-function #'lsp-format-region))

  (when lsp-enable-xref
    (setq-local xref-backend-functions #'lsp--xref-backend))

  (when (and lsp-enable-completion-at-point (lsp--capability "completionProvider"))
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t))

  ;; Make sure the hook is local (last param) otherwise we see all changes for all buffers
  (add-hook 'before-change-functions #'lsp-before-change nil t)
  (add-hook 'after-change-functions #'lsp-on-change nil t)
  (add-hook 'before-save-hook #'lsp--before-save nil t)
  (add-hook 'auto-save-hook #'lsp--on-auto-save nil t)
  (lsp--set-sync-method)
  (run-hooks 'lsp-after-open-hook))

(define-inline lsp--text-document-identifier ()
  "Make TextDocumentIdentifier.

interface TextDocumentIdentifier {
    uri: string;
}"
  (inline-quote (list :uri (concat "file://" buffer-file-name))))

(define-inline lsp--versioned-text-document-identifier ()
  "Make VersionedTextDocumentIdentifier.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: number;
}"
  (inline-quote (plist-put (lsp--text-document-identifier)
                  :version (lsp--cur-file-version))))

(define-inline lsp--position (line char)
  "Make a Position object for the given LINE and CHAR.
interface Position {
    line: number;
    character: number;
}"
  (inline-quote (list :line ,line :character ,char)))

(define-inline lsp--cur-line ()
  (inline-quote (1- (line-number-at-pos))))

(define-inline lsp--cur-column ()
  (inline-quote (- (point) (line-beginning-position))))

(define-inline lsp--cur-position ()
  "Make a Position object for the current point."
  (inline-quote (lsp--position (lsp--cur-line) (lsp--cur-column))))

(defun lsp--point-to-position (point)
  "Convert POINT to Position."
  (save-excursion
    (goto-char point)
    (save-restriction
      (widen) ;; May be in a narrowed region
      (lsp--cur-position))))

(define-inline lsp--position-p (p)
  (inline-quote
    (and (numberp (plist-get ,p :line)) (numberp (plist-get ,p :character)))))

(define-inline lsp--range (start end)
  "Make Range body from START and END.

interface Range {
     start: Position;
     end: Position;
 }"
  ;; make sure start and end are Position objects
  (inline-quote
    (progn
      (cl-check-type ,start (satisfies lsp--position-p))
      (cl-check-type ,end (satisfies lsp--position-p))
      (list :start ,start :end ,end))))

(define-inline lsp--region-to-range (start end)
  "Make Range object for the current region."
  (inline-quote (lsp--range (lsp--point-to-position ,start)
                  (lsp--point-to-position ,end))))

(defun lsp--current-region-or-pos ()
  "If the region is active return that, else get the point."
  (if (use-region-p)
      (lsp--region-to-range (region-beginning) (region-end))
    (lsp--region-to-range (point) (point))))

(defun lsp--get-start-position ()
  "Get the start of the region if active, else current point."
  (let ((pos (if (use-region-p)
                 (region-beginning)
               (point))))
    (lsp-point-to-position pos)))

(defun lsp--get-end-position ()
  "Get the end of the region if active, else current point."
  (let ((pos (if (use-region-p)
                 (region-end)
               (point))))
    (lsp-point-to-position pos)))

(define-inline lsp--range-start-line (range)
  "Return the start line for a given LSP range, in LSP coordinates"
  (inline-quote (plist-get (plist-get ,range :start) :line)))

(define-inline lsp--range-end-line (range)
  "Return the end line for a given LSP range, in LSP coordinates"
  (inline-quote (plist-get (plist-get ,range :end) :line)))

(defun lsp--apply-workspace-edits (edits)
  (cl-letf (((lsp--workspace-change-timer-disabled lsp--cur-workspace) t))
    (maphash (lambda (key value)
               (lsp--apply-workspace-edit key value))
             (gethash "changes" edits))))

(defun lsp--apply-workspace-edit (uri edits)
  (let ((filename (string-remove-prefix "file://" uri)))
    ;; TODO: What if the buffer has been modified?
    ;;       Although, for incremental sync that should be fine
    (when (not (find-buffer-visiting filename))
      (progn (find-file filename)
             (lsp--text-document-did-open)))
    (lsp--apply-text-edits edits)))

(defun lsp--apply-text-edits (edits)
  "Apply the edits described in the TextEdit[] object in EDITS."
  (dolist (edit edits)
    (lsp--apply-text-edit edit)))

(defun lsp--apply-text-edit (text-edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT."
  (let* ((range (gethash "range" text-edit))
         (start-point (lsp--position-to-point (gethash "start" range)))
         (end-point (lsp--position-to-point (gethash "end" range))))
    (save-excursion
      (goto-char start-point)
      (delete-region start-point end-point)
      (insert (gethash "newText" text-edit)))))

(define-inline lsp--capability (cap &optional capabilities)
  "Get the value of capability CAP.  If CAPABILITIES is non-nil, use them instead."
  (inline-quote (gethash ,cap (or ,capabilities (lsp--server-capabilities)))))

(defvar-local lsp--before-change-vals nil
  "Store the positions from the `lsp-before-change' function
  call, for validation and use in the `lsp-on-change' function.")

(defun lsp--text-document-content-change-event (start end length)
  "Make a TextDocumentContentChangeEvent body for START to END, of length LENGTH."
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":5,"character":6}
  ;;             ,"end" :{"line":5,"character":6}}
  ;;             ,"rangeLength":0
  ;;             ,"text":"\nbb = 5"}
  ;;
  ;; And (47 47 7) means delete 7 chars starting at pos 47
  ;; must become
  ;;   {"range":{"start":{"line":6,"character":0}
  ;;            ,"end"  :{"line":7,"character":0}}
  ;;            ,"rangeLength":7
  ;;            ,"text":""}
  ;;
  ;; (208 221 3) means delete 3 chars starting at pos 208, and replace them with
  ;; 13 chars. So it must become
  ;;   {"range":{"start":{"line":5,"character":8}
  ;;             ,"end" :{"line":5,"character":11}}
  ;;             ,"rangeLength":3
  ;;             ,"text":"new-chars-xxx"}
  ;;

  ;; Adding text:
  ;;   lsp-before-change:(start,end)=(33,33)
  ;;   lsp-on-change:(start,end,length)=(33,34,0)
  ;;
  ;; Changing text:
  ;;   lsp-before-change:(start,end)=(208,211)
  ;;   lsp-on-change:(start,end,length)=(208,221,3)
  ;;
  ;; Deleting text:
  ;;   lsp-before-change:(start,end)=(19,27)
  ;;   lsp-on-change:(start,end,length)=(19,19,8)

  (if (eq length 0)
      ;; Adding something only, work from start only
      `(:range ,(lsp--range (lsp--point-to-position start)
                            (lsp--point-to-position start))
               :rangeLength 0
               :text ,(buffer-substring-no-properties start end))

    (if (eq start end)
        ;; Deleting something only
        (if (lsp--bracketed-change-p start end length)
            ;; (if (and (eq start (plist-get lsp--before-change-vals :start) )
            ;;          (eq length (- (plist-get lsp--before-change-vals :end)
            ;;                        (plist-get lsp--before-change-vals :start))))
            ;; The before-change value is valid, use it
            `(:range ,(lsp--range (lsp--point-to-position start)
                                  (plist-get lsp--before-change-vals :end-pos))
                     :rangeLength ,length
                     :text "")
          (lsp--change-for-mismatch start end length))
      ;; Deleting some things, adding others
      (if (lsp--bracketed-change-p start end length)
          ;; The before-change value is valid, use it
          `(:range ,(lsp--range (lsp--point-to-position start)
                                (plist-get lsp--before-change-vals :end-pos))
                   :rangeLength ,length
                   :text ,(buffer-substring-no-properties start end))
        (lsp--change-for-mismatch start end length)))))


(defun lsp--change-for-mismatch (_start _end _length)
  "If the current change is not fully bracketed, report it and
return the full contents of the buffer as the change."
  (lsp--full-change-event))

;; TODO: Add tests for this function.
(defun lsp--bracketed-change-p (start _end length)
  "If the before and after positions are the same, and the length
is the size of the start range, we are probably good."
  (and (eq start (plist-get lsp--before-change-vals :start) )
       (eq length (- (plist-get lsp--before-change-vals :end)
                     (plist-get lsp--before-change-vals :start)))))

;; Observed from vscode for applying a diff replacing one line with
;; another. Emacs on-change shows this as a delete followed by an
;; add.

;; 2017-04-22 17:43:59 [ThreadId 11] DEBUG haskell-lsp - ---> {"jsonrpc":"2.0","method":"textDocument/didChange","params":
;; {"textDocument":{"uri":"file:///home/alanz/tmp/haskell-hie-test-project/src/Foo.hs","version":2}
;; ,"contentChanges":[{"range":{"start":{"line":7,"character":0}
;;                             ,"end":  {"line":7,"character":8}}
;;                     ,"rangeLength":8
;;                     ,"text":"baz ="}]}}


(defun lsp--full-change-event ()
  (save-restriction
    (widen)
    `(:text ,(buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defcustom lsp-change-idle-delay 0.5
  "Number of seconds of idle timer to wait before sending file changes to the server."
  :group 'lsp-mode
  :type 'number)

(defvar lsp--change-idle-timer nil
  "Idle timer which sends changes to the language server.")
(defvar lsp--last-workspace-timer nil
  "The last workspace for which the onChange idle timer was set.")

(defvar-local lsp--changes [])
(defvar-local lsp--has-changes []
  "non-nil if the current buffer has any changes yet to be sent.")

(defun lsp--rem-idle-timer ()
  (when lsp--change-idle-timer
    (cancel-timer lsp--change-idle-timer)
    (setq
     lsp--change-idle-timer nil
     lsp--last-workspace-timer nil)))

(defun lsp--set-idle-timer (workspace)
  (setq lsp--change-idle-timer (run-at-time lsp-change-idle-delay nil
                                            #'(lambda ()
                                                (cl-assert workspace)
                                                (lsp--send-changes workspace)))
        lsp--last-workspace-timer lsp--cur-workspace))

(defun lsp--flush-other-workspace-changes ()
  "Flush changes for any other workspace."
  (when (and lsp--last-workspace-timer
             (not (eq lsp--last-workspace-timer lsp--cur-workspace)))
    ;; A timer for a different workspace was set, flush those
    ;; changes first.
    (lsp--send-changes lsp--last-workspace-timer t)))

(defun lsp--send-changes (workspace &optional no-flush-other)
  "Sends any queued changes for this WORKSPACE.
Called before any function that performs any query to the languge server related
to a text document."
  (unless no-flush-other
    (lsp--flush-other-workspace-changes))
  (lsp--rem-idle-timer)
  (dolist (buffer (lsp--workspace-buffers workspace))
    (with-current-buffer buffer
      (when (and lsp--has-changes
                 (not (eq lsp--server-sync-method 'none)))
        (lsp--inc-cur-file-version)
        (lsp--send-notification
         (lsp--make-notification
          "textDocument/didChange"
          `(:textDocument
            ,(lsp--versioned-text-document-identifier)
            :contentChanges
            ,(pcase lsp--server-sync-method
               ('incremental lsp--changes)
               ('full `[,(lsp--full-change-event)])
               ('none `[])))))
        (setq lsp--changes []
              lsp--has-changes nil)))))

(defun lsp--push-change (change-event)
  "Push CHANGE-EVENT to the buffer change vector."
  ;; (message "lsp--push-change entered")
  (setq lsp--changes (vconcat lsp--changes `(,change-event))))

(defun lsp-before-change (start end)
  "Executed before a file is changed.
Added to `before-change-functions'."
  ;; Note:
  ;;
  ;; This variable holds a list of functions to call when Emacs is about to
  ;; modify a buffer. Each function gets two arguments, the beginning and end of
  ;; the region that is about to change, represented as integers. The buffer
  ;; that is about to change is always the current buffer when the function is
  ;; called.
  ;;
  ;; WARNING:
  ;;
  ;; Do not expect the before-change hooks and the after-change hooks be called
  ;; in balanced pairs around each buffer change. Also don't expect the
  ;; before-change hooks to be called for every chunk of text Emacs is about to
  ;; delete. These hooks are provided on the assumption that Lisp programs will
  ;; use either before- or the after-change hooks, but not both, and the
  ;; boundaries of the region where the changes happen might include more than
  ;; just the actual changed text, or even lump together several changes done
  ;; piecemeal.
  ;; (message "lsp-before-change:(start,end)=(%s,%s)" start end)
  (setq lsp--before-change-vals
        `(:start ,start
                 :end ,end
                 :start-pos ,(lsp--point-to-position start)
                 :end-pos   ,(lsp--point-to-position end))))

(defun lsp-on-change (start end length)
  "Executed when a file is changed.
Added to `after-change-functions'."
  ;; Note:
  ;;
  ;; Each function receives three arguments: the beginning and end of the region
  ;; just changed, and the length of the text that existed before the change.
  ;; All three arguments are integers. The buffer that has been changed is
  ;; always the current buffer when the function is called.
  ;;
  ;; The length of the old text is the difference between the buffer positions
  ;; before and after that text as it was before the change. As for the
  ;; changed text, its length is simply the difference between the first two
  ;; arguments.
  ;;
  ;; So (47 54 0) means add    7 chars starting at pos 47
  ;; So (47 47 7) means delete 7 chars starting at pos 47
  ;; (message "lsp-on-change:(start,end,length)=(%s,%s,%s)" start end length)
  ;; (message "lsp-on-change:(lsp--before-change-vals)=%s" lsp--before-change-vals)
  (lsp--flush-other-workspace-changes)
  (when (and lsp--cur-workspace
             (not (or (eq lsp--server-sync-method 'none)
                      (eq lsp--server-sync-method nil))))
    (setq lsp--has-changes t)
    (lsp--rem-idle-timer)
    (when (eq lsp--server-sync-method 'incremental)
      ;; (lsp--push-change (lsp--text-document-content-change-event start end length)))

      ;; Each change needs to be wrt to the current doc, so send immediately.
      ;; Otherwise we need to adjust the coordinates of the new change according
      ;; to the cumulative changes already queued.
      (progn
        (lsp--push-change (lsp--text-document-content-change-event start end length))
        (lsp--send-changes lsp--cur-workspace)))
    (if (lsp--workspace-change-timer-disabled lsp--cur-workspace)
        (lsp--send-changes lsp--cur-workspace)
      (lsp--set-idle-timer lsp--cur-workspace))))

(defun lsp--text-document-did-close ()
  "Executed when the file is closed, added to `kill-buffer-hook'."
  (lsp--send-changes lsp--cur-workspace)
  (let ((file-versions (lsp--workspace-file-versions lsp--cur-workspace))
        (old-buffers (lsp--workspace-buffers lsp--cur-workspace)))
    ;; remove buffer from the current workspace's list of buffers
    ;; do a sanity check first
    (when (memq (current-buffer) old-buffers)
      (setf (lsp--workspace-buffers lsp--cur-workspace)
            (delq (current-buffer) old-buffers))

      (remhash buffer-file-name file-versions)
      (lsp--send-notification
       (lsp--make-notification
        "textDocument/didClose"
        `(:textDocument ,(lsp--versioned-text-document-identifier))))
      (when (= 0 (hash-table-count file-versions))
        (lsp--shutdown-cur-workspace)))))

(defun lsp--before-save ()
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace)
    (lsp--send-notification
      (lsp--make-notification "textDocument/willSave"
        `(:textDocument ,(lsp--text-document-identifier)
           :reason 1)))))

(defun lsp--on-auto-save ()
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace)
    (lsp--send-notification
      (lsp--make-notification "textDocument/willSave"
        `(:textDocument ,(lsp--text-document-identifier)
           :reason 2)))))

(defun lsp--text-document-did-save ()
  "Executed when the file is closed, added to `after-save-hook''."
  (when lsp--cur-workspace
    (lsp--send-changes lsp--cur-workspace)
    (lsp--send-notification
     (lsp--make-notification
      "textDocument/didSave"
      `(:textDocument ,(lsp--versioned-text-document-identifier))))))

(define-inline lsp--text-document-position-params ()
  "Make TextDocumentPositionParams for the current point in the current document."
  (inline-quote (list :textDocument (lsp--text-document-identifier)
                  :position (lsp--position (lsp--cur-line) (lsp--cur-column)))))

(define-inline lsp--text-document-code-action-params ()
  "Make CodeActionParams for the current region in the current document."
  (inline-quote (list :textDocument (lsp--text-document-identifier)
                  :range (lsp--current-region-or-pos)
                  :context (list :diagnostics (lsp--cur-line-diagnotics)))))

(defun lsp--cur-line-diagnotics ()
  "Return any diagnostics that apply to the current line."
  (let* ((diags (gethash buffer-file-name lsp--diagnostics nil))
         (range (lsp--current-region-or-pos))
         (start-line (lsp--range-start-line range))
         (end-line (lsp--range-end-line range))
         (diags-in-range (cl-remove-if-not
                          (lambda (diag)
                            (let ((line (lsp-diagnostic-line diag)))
                              (and (>= line start-line) (<= line end-line))))
                          diags)))
    (cl-coerce (mapcar #'lsp-diagnostic-original diags-in-range) 'vector)))


(defconst lsp--completion-item-kind
  `(
    (1 . "Text")
    (2 . "Method")
    (3 . "Function")
    (4 . "Constructor")
    (5 . "Field")
    (6 . "Variable")
    (7 . "Class")
    (8 . "Interface")
    (9 . "Module")
    (10 . "Property")
    (11 . "Unit")
    (12 . "Value")
    (13 . "Enum")
    (14 . "Keyword")
    (15 . "Snippet")
    (16 . "Color")
    (17 . "File")
    (18 . "Reference")))

(defun lsp--gethash (key table &optional dflt)
  "Look up KEY in TABLE and return its associated value,
unless KEY not found or its value is falsy, when it returns DFLT.
DFLT defaults to nil.

Needed for completion request fallback behavior for the fields
'sortText', 'filterText', and 'insertText' as described here:

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#completion-request"

  (let ((result (gethash key table dflt)))
    (when (member result '(nil "" 0 :json-false))
      (setq result dflt))
    result))

(defun lsp--make-completion-item (item)
  (propertize (lsp--gethash "insertText" item (gethash "label" item ""))
              'lsp-completion-item
              item))

(defun lsp--annotate (item)
  (let* ((table (plist-get (text-properties-at 0 item) 'lsp-completion-item))
         (detail (gethash "detail" table nil))
         (kind (alist-get (gethash "kind" table nil) lsp--completion-item-kind)))
    (concat
     " "
     detail
     (when kind " ")
     (when kind (format "(%s)" kind)))))

(defun lsp--sort-string (c)
  (lsp--gethash "sortText" c (gethash "label" c "")))

(defun lsp--sort-completions (completions)
  (sort completions #'(lambda (c1 c2)
                        (when (string-lessp
                               (lsp--sort-string c1)
                               (lsp--sort-string c2))
                          t))))

(defun lsp--get-completions ()
  (lsp--send-changes lsp--cur-workspace)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list
     (if bounds (car bounds) (point))
     (if bounds (cdr bounds) (point))
     (completion-table-dynamic
      #'(lambda (_)
          ;; *we* don't need to know the string being completed
          ;; the language server does all the work by itself
          (let* ((resp (lsp--send-request (lsp--make-request
                                           "textDocument/completion"
                                           (lsp--text-document-position-params))))
                 (items (cond
                         ((null resp) nil)
                         ((hash-table-p resp) (gethash "items" resp nil))
                         ((sequencep resp) resp))))
            (mapcar #'lsp--make-completion-item items))))
     :annotation-function #'lsp--annotate
     :display-sort-function #'lsp--sort-completions)))

(defun lsp--resolve-completion (item)
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (cl-assert item nil "Completion item must not be nil")
  (if (gethash "resolveProvider" (lsp--capability "completionProvider"))
      (lsp--send-request
       (lsp--make-request
	"completionItem/resolve"
	item))
    item))

(defun lsp--location-to-xref (location)
  "Convert Location object LOCATION to an ‘xref-item’.
interface Location {
    uri: string;
    range: Range;
}"
  (lsp--send-changes lsp--cur-workspace)
  (let ((uri (string-remove-prefix "file://" (gethash "uri" location)))
        (ref-pos (gethash "start" (gethash "range" location))))
    (xref-make uri
               (xref-make-file-location uri
                                        (1+ (gethash "line" ref-pos))
                                        (gethash "character" ref-pos)))))

(defun lsp--get-defitions ()
  "Get definition of the current symbol under point.
Returns xref-item(s)."
  (lsp--send-changes lsp--cur-workspace)
  (let ((location (lsp--send-request (lsp--make-request
                                      "textDocument/definition"
                                      (lsp--text-document-position-params)))))
    (if (consp location) ;;multiple definitions
        (mapcar 'lsp--location-to-xref location)
      (lsp--location-to-xref location))))

(defun lsp--make-reference-params (&optional td-position)
  "Make a ReferenceParam object.
If TD-POSITION is non-nil, use it as TextDocumentPositionParams object instead."
  (let ((json-false :json-false))
    (plist-put (or td-position (lsp--text-document-position-params))
               :context `(:includeDeclaration ,json-false))))

(defun lsp--get-references ()
  "Get all references for the symbol under point.
Returns xref-item(s)."
  (lsp--send-changes lsp--cur-workspace)
  (let ((location  (lsp--send-request (lsp--make-request
                                       "textDocument/references"
                                       (lsp--make-reference-params)))))
    (if (consp location)
        (mapcar 'lsp--location-to-xref location)
      (and location (lsp--location-to-xref location)))))

(defun lsp--cancel-request (id)
  (lsp--cur-workspace-check)
  (cl-check-type id (or number string))
  (let ((response-handlers (lsp--client-response-handlers (lsp--workspace-client
                                                            lsp--cur-workspace))))
    (remhash id response-handlers)
    (lsp--send-notification (lsp--make-notification "$/cancelRequest"
                              `(:id ,id)))))

(defun lsp--on-hover ()
  (when (and (lsp--capability "documentHighlightProvider")
             lsp-highlight-symbol-at-point)
    (lsp-symbol-highlight))
  (when (and (lsp--capability "codeActionProvider") lsp-enable-codeaction)
    (lsp--text-document-code-action))
  (when lsp-enable-eldoc
    (lsp--text-document-hover-string)))

(defvar-local lsp--cur-hover-request-id nil)

(defun lsp--text-document-hover-string ()
  "interface Hover {
    contents: MarkedString | MarkedString[];
    range?: Range;
}

type MarkedString = string | { language: string; value: string };"
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (when lsp--cur-hover-request-id
    (lsp--cancel-request lsp--cur-hover-request-id))
  (let* ((client (lsp--workspace-client lsp--cur-workspace))
          (renderers (lsp--client-string-renderers client))
          bounds body)
    (when (symbol-at-point)
      (setq bounds (bounds-of-thing-at-point 'symbol)
        body (lsp--send-request-async (lsp--make-request "textDocument/hover"
                                        (lsp--text-document-position-params))
               (lsp--make-hover-callback renderers (car bounds) (cdr bounds)
                 (current-buffer)))
        lsp--cur-hover-request-id (plist-get body :id))
      (cl-assert (integerp lsp--cur-hover-request-id)))))

(define-inline lsp--point-is-within-bounds-p (start end)
  "Return whether the current point is within START and END."
  (inline-quote
    (let ((p (point)))
      (and (>= p ,start) (<= p ,end)))))

;; start and end are the bounds of the symbol at point
(defun lsp--make-hover-callback (renderers start end buffer)
  (lambda (hover)
    (setq lsp--cur-hover-request-id nil)
    (when (and (lsp--point-is-within-bounds-p start end)
            (eq (current-buffer) buffer) (eldoc-display-message-p))
      (let ((contents (gethash "contents" hover)))
        (eldoc-message
          (mapconcat (lambda (e)
                       (let (renderer)
                         (if (hash-table-p e)
                           (if (setq renderer (cdr (assoc-string
                                                     (gethash "language" e) renderers)))
                             (funcall renderer (gethash "value" e))
                             (gethash "value" e))
                           e)))
            (if (listp contents) contents (list contents)) "\n"))))))

(defun lsp-provide-marked-string-renderer (client language renderer)
  (cl-check-type language string)
  (cl-check-type renderer function)
  (setf (alist-get language (lsp--client-string-renderers client)) renderer))

(defun lsp-info-under-point ()
  "Show relevant documentation for the thing under point."
  (interactive)
  (lsp--text-document-hover-string))

;; NOTE: the code actions cannot currently be applied. There is some non-GNU
;; code to do this in the lsp-haskell module. We still need a GNU version, here.
;; PRs accepted.
(defvar lsp-code-actions nil
  "Code actions for the buffer.")

(defun lsp--text-document-code-action ()
  "Request code action to automatically fix issues reported by
the diagnostics"
  (lsp--cur-workspace-check)
  (lsp--send-request-async (lsp--make-request
                            "textDocument/codeAction"
                            (lsp--text-document-code-action-params))
                           #'lsp--text-document-code-action-callback))

(defun lsp--text-document-code-action-callback (actions)
  "Callback to process the reply to a 'textDocument/codeAction' request."
  (setq lsp-code-actions (cl-union actions lsp-code-actions)))

(defun lsp--make-document-formatting-options ()
  (let ((json-false :json-false))
    `(:tabSize ,tab-width :insertSpaces
               ,(if indent-tabs-mode json-false t))))

(defun lsp--make-document-formatting-params ()
  `(:textDocument ,(lsp--text-document-identifier)
                  :options ,(lsp--make-document-formatting-options)))

(defun lsp-format-buffer ()
  "Ask the server to format this document."
  (interactive)
  (lsp--send-changes lsp--cur-workspace)
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/formatting"
                                   (lsp--make-document-formatting-params)))))
    (dolist (edit edits)
      (lsp--apply-text-edit edit))))

(defun lsp--make-document-range-formatting-params (start end)
  "Make DocumentRangeFormattingParams for selected region.
interface DocumentRangeFormattingParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}"
  (plist-put (lsp--make-document-formatting-params)
             :range (lsp--region-to-range start end)))

(defconst lsp--highlight-kind-face
  '((1 . lsp-face-highlight-textual)
    (2 . lsp-face-highlight-read)
    (3 . lsp-face-highlight-write)))

(defun lsp--remove-cur-overlays ()
  (dolist (overlay (lsp--workspace-highlight-overlays lsp--cur-workspace))
    (delete-overlay overlay)))

(defun lsp-symbol-highlight ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (lsp--send-changes lsp--cur-workspace)
  (lsp--send-request-async (lsp--make-request
                            "textDocument/documentHighlight"
                            (lsp--text-document-position-params))
                           #'lsp--symbol-highlight-callback))

(defun lsp--symbol-highlight-callback (highlights)
  "Callback function to process the reply of a
'textDocument/documentHightlight' message.
A reference is highlighted only if it is visible in a window."
  (lsp--remove-cur-overlays)
  (let ((windows-on-buffer (get-buffer-window-list nil nil 'visible)))
    (dolist (highlight highlights)
      (let* ((range (gethash "range" highlight nil))
             (kind (gethash "kind" highlight 1))
             (start-point (lsp--position-to-point (gethash "start" range)))
             (end-point (lsp--position-to-point (gethash "end" range)))
             overlay)
        (dolist (win windows-on-buffer)
          (when (or (pos-visible-in-window-group-p start-point win t)
                    (pos-visible-in-window-group-p end-point win t))
            (setq overlay (make-overlay start-point end-point))
            (overlay-put overlay 'face
                         (cdr (assq kind lsp--highlight-kind-face)))
            (push overlay (lsp--workspace-highlight-overlays lsp--cur-workspace))))))))

(defconst lsp--symbol-kind
  '((1 . "File")
    (2 . "Module")
    (3 . "Namespace")
    (4 . "Package")
    (5 . "Class")
    (6 . "Method")
    (7 . "Property")
    (8 . "Field")
    (9 . "Constructor"),
    (10 . "Enum")
    (11 . "Interface")
    (12 . "Function")
    (13 . "Variable")
    (14 . "Constant")
    (15 . "String")
    (16 . "Number")
    (17 . "Boolean")
    (18 . "Array")))

(defun lsp--symbol-information-to-xref (symbol)
  (xref-make (format "%s %s"
                     (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                     (gethash "name" symbol))
             (lsp--location-to-xref (gethash "location" symbol))))

(defun lsp-format-region (s e)
  (lsp--send-changes lsp--cur-workspace)
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/rangeFormatting"
                                   (lsp--make-document-range-formatting-params s e)))))
    (lsp--apply-text-edits edits)))

(defun lsp--location-to-td-position (location)
  "Convert LOCATION to a TextDocumentPositionParams object."
  `(:textDocument (:uri ,(gethash "uri" location))
                  :position ,(gethash "start" (gethash "range" location))))

(defun lsp--symbol-info-to-identifier (symbol)
  (propertize (gethash "name" symbol)
              'ref-params (lsp--make-reference-params
                           (lsp--location-to-td-position (gethash "location" symbol)))))

(defun lsp--xref-backend () 'xref-lsp)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp)))
  (propertize (symbol-name (symbol-at-point))
              'def-params (lsp--text-document-position-params)
              'ref-params (lsp--make-reference-params)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
  (let ((json-false :json-false)
        (symbols (lsp--send-request (lsp--make-request
                                     "textDocument/documentSymbol"
                                     `(:textDocument ,(lsp--text-document-identifier))))))
    (mapcar #'lsp--symbol-info-to-identifier symbols)))

;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp)))
;;   nil)

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp)) identifier)
  (let* ((maybeparams (get-text-property 0 'def-params identifier))
         ;; In some modes (such as haskell-mode), xref-find-definitions gets
         ;; called directly without applying the properties expected here. So we
         ;; must test if the properties are present, and if not use the current
         ;; point location.
         (params (if (null maybeparams)
                     (lsp--text-document-position-params)
                   maybeparams))
         (def (lsp--send-request (lsp--make-request
                                  "textDocument/definition"
                                  params))))
    (if (consp def)
        (mapcar 'lsp--location-to-xref def)
      (and def `(,(lsp--location-to-xref def))))))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp)) identifier)
  (let* ((properties (text-properties-at 0 identifier))
         (params (plist-get properties 'ref-params))
         (ref (lsp--send-request (lsp--make-request
                                  "textDocument/references"
                                  (or params (lsp--make-reference-params))))))
    (if (consp ref)
        (mapcar 'lsp--location-to-xref ref)
      (and ref `(,(lsp--location-to-xref ref))))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
  (let ((symbols (lsp--send-request (lsp--make-request
                                     "workspace/symbol"
                                     `(:query ,pattern)))))
    (mapcar 'lsp--symbol-information-to-xref symbols)))

(defun lsp--make-document-rename-params (newname)
  "Make DocumentRangeFormattingParams for selected region.
interface RenameParams {
    textDocument: TextDocumentIdentifier;
    position: Position;
    newName: string;
}"
  `(:position ,(lsp--cur-position)
              :textDocument ,(lsp--text-document-identifier)
              :newName ,newname))

(defun lsp-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive "sRename to: ")
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (unless (lsp--capability "renameProvider")
    (signal 'lsp-capability-not-supported (list "renameProvider")))
  (let ((edits (lsp--send-request (lsp--make-request
                                   "textDocument/rename"
                                   (lsp--make-document-rename-params newname)))))
    (lsp--apply-workspace-edits edits)))

(defun lsp--execute-lsp-server-command (command)
  "Given a COMMAND returned from the server via e.g.
'textDocument/codeAction' ceate and send a
'workspace/executeCommand' message"

  (lsp--send-execute-command (gethash "command" command) (gethash "arguments" command nil))
  )

(defun lsp--send-execute-command (command &optional args)
  "Create and send a 'workspace/executeCommand' message having
command COMMAND and optionsl ARGS"
  (lsp--cur-workspace-check)
  (lsp--send-changes lsp--cur-workspace)
  (lsp--send-request
   (lsp--make-request
    "workspace/executeCommand"
    (lsp--make-execute-command-params command args))))

(defun lsp--make-execute-command-params (cmd &optional args)
  (if args
      (list :command cmd :arguments args)
    (list :command cmd)))

(defalias 'lsp-point-to-position #'lsp--point-to-position)
(defalias 'lsp-get-start-position #'lsp--get-start-position)
(defalias 'lsp-get-end-position #'lsp--get-end-position)
(defalias 'lsp-text-document-identifier #'lsp--text-document-identifier)
(defalias 'lsp-send-execute-command #'lsp--send-execute-command)
(defalias 'lsp-on-open #'lsp--text-document-did-open)
(defalias 'lsp-on-save #'lsp--text-document-did-save)
;; (defalias 'lsp-on-change #'lsp--text-document-did-change)
(defalias 'lsp-completion-at-point #'lsp--get-completions)
(defalias 'lsp-error-explainer #'lsp--error-explainer)

(defun lsp--unset-variables ()
  (when lsp-enable-eldoc
    (setq-local eldoc-documentation-function 'ignore))
  (when lsp-enable-xref
    (setq-local xref-backend-functions nil))
  (when lsp-enable-completion-at-point
    (remove-hook 'completion-at-point-functions #'lsp-completion-at-point t))
  (remove-hook 'after-change-functions #'lsp-on-change t)
  (remove-hook 'before-change-functions #'lsp-before-change t))

(defun lsp--error-explainer (fc-error)
  "Proof of concept to use this flycheck function to apply a codeAction.
This should eventually make use of the completion of
https://github.com/flycheck/flycheck/pull/1022 and
https://github.com/flycheck/flycheck/issues/530#issuecomment-235224763."
  (message "lsp--error-explainer: got %s" fc-error))

(defun lsp--set-configuration (settings)
  "Set the configuration for the lsp server."
  (lsp--send-notification (lsp--make-notification
                           "workspace/didChangeConfiguration"
                           `(:settings , settings))))

(declare-function lsp-flycheck-add-mode "lsp-flycheck" (mode))
(declare-function flycheck-buffer "flycheck" ())

(provide 'lsp-methods)
;;; lsp-methods.el ends here
