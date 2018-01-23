;; Copyright (C) 2016-2017  Vibhav Pant <vibhavp@gmail.com> -*- lexical-binding: t -*-

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

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'lsp-common)
(require 'lsp-notifications)
(require 'pcase)
(require 'subr-x)

;; vibhavp: Should we use a lower value (5)?
(defcustom lsp-response-timeout 10
  "Number of seconds to wait for a response from the language server before timing out."
  :type 'number
  :group 'lsp-mode)

(defvar lsp--no-response)

(defun lsp--send-wait (message proc)
  "Send MESSAGE to PROC and wait for output from the process."
  (when lsp-print-io
    (message "lsp--stdio-wait: %s" message))
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message)
  (setq lsp--no-response t)
  (with-local-quit
    (accept-process-output proc lsp-response-timeout))
  (when lsp--no-response
    (signal 'lsp-timed-out-error nil)))

(defun lsp--send-no-wait (message proc)
  "Send MESSAGE to PROC without waiting for further output."
  (when lsp-print-io
    (message "lsp--send-no-wait: %s" message))
  (when (memq (process-status proc) '(stop exit closed failed nil))
    (error "%s: Cannot communicate with the process (%s)" (process-name proc)
           (process-status proc)))
  (process-send-string proc message))

(cl-defstruct lsp--parser
  (waiting-for-response nil)
  (response-result nil)
  (headers '()) ;; alist of headers
  (body nil) ;; message body
  (reading-body nil) ;; If non-nil, reading body
  (body-length nil) ;; length of current message body
  (body-received 0) ;; amount of current message body currently stored in 'body'
  (leftovers nil) ;; Leftover data from previous chunk; to be processed

  (queued-notifications nil) ;; Unused field
  (queued-requests nil)

  (workspace nil) ;; the workspace
  )

(define-error 'lsp-parse-error
  "Error parsing message from language server" 'lsp-error)
(define-error 'lsp-unknown-message-type
  "Unknown message type" '(lsp-error lsp-parse-error))
(define-error 'lsp-unknown-json-rpc-version
  "Unknown JSON-RPC protocol version" '(lsp-error lsp-parse-error))
(define-error 'lsp-no-content-length
  "Content-Length header missing in message" '(lsp-error lsp-parse-error))
(define-error 'lsp-invalid-header-name
  "Invalid header name" '(lsp-error lsp-parse-error))

;;  id  method
;;   x    x     request
;;   x    .     response
;;   .    x     notification

(defun lsp--get-message-type (json-data)
  "Get the message type from JSON-DATA."
  (when (not (string= (gethash "jsonrpc" json-data "") "2.0"))
    (signal 'lsp-unknown-json-rpc-version (list (gethash "jsonrpc" json-data))))
  (if (gethash "id" json-data nil)
      (if (gethash "error" json-data nil)
          'response-error
        (if (gethash "method" json-data nil)
            'request
          'response))
    (if (gethash "method" json-data nil)
      'notification
      (signal 'lsp-unknown-message-type (list json-data)))))

(defun lsp--on-notification (p notification)
  "Call the appropriate handler for NOTIFICATION."
  (let ((params (gethash "params" notification))
        (client (lsp--workspace-client (lsp--parser-workspace p)))
        handler)
    (pcase (gethash "method" notification)
      ("window/showMessage" (lsp--window-show-message params))
      ("window/logMessage" (lsp--window-show-message params)) ;; Treat as showMessage for now
      ("textDocument/publishDiagnostics" (lsp--on-diagnostics params
                                           (lsp--parser-workspace p)))
      ("textDocument/diagnosticsEnd")
      ("textDocument/diagnosticsBegin")
      (other
        (setq handler (gethash other (lsp--client-notification-handlers client) nil))
        (if (not handler)
          (message "Unknown method: %s" other)
          (funcall handler (lsp--parser-workspace p) params))))))

(defun lsp--on-request (p request)
  "Call the appropriate handler for REQUEST, and send the return value to the server."
  (let ((params (gethash "params" request))
         (client (lsp--workspace-client (lsp--parser-workspace p)))
         (process (lsp--workspace-proc (lsp--parser-workspace p)))
         (empty-response (lsp--make-response (gethash "id" request) nil nil))
         handler response)
    (setq response
      (pcase (gethash "method" request)
        ("client/registerCapability"
          (dolist (reg (gethash "registrations" params))
            (lsp--server-register-capability reg))
          empty-response)
        ("client/unregisterCapability"
          (dolist (unreg (gethash "unregisterations" params))
            (lsp--server-unregister-capability unreg))
          empty-response)
        ("workspace/applyEdit"
          (lsp--workspace-apply-edit-handler
            (lsp--parser-workspace p) params)
          empty-response)
        (other
          (setq handler (gethash other (lsp--client-request-handlers client) nil))
          (if (not handler)
            (progn
              (message "Unknown request method: %s" other)
              empty-response)
            (lsp--make-response (gethash "id" request)
              (funcall handler (lsp--parser-workspace p) params) nil)))))
    ;; Send response to the server.
    (lsp--send-no-wait (lsp--make-message response) process)))

(defconst lsp--errors
  '((-32700 "Parse Error")
    (-32600 "Invalid Request")
    (-32601 "Method not Found")
    (-32602 "Invalid Parameters")
    (-32603 "Internal Error")
    (-32099 "Server Start Error")
    (-32000 "Server End Error")))

(defun lsp--error-string (err)
  "Format ERR as a user friendly string."
  (let ((code (gethash "code" err))
        (message (gethash "message" err)))
    (format "Error from the Language Server: %s (%s)"
            message
            (or (car (alist-get code lsp--errors)) "Unknown error"))))

(defun lsp--get-body-length (headers)
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server our our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun lsp--parse-header (s)
  "Parse string S as a LSP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (substring s (+ 2 pos)))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c being the elements of val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun lsp--parser-reset (p)
  (setf
   (lsp--parser-leftovers p) ""
   (lsp--parser-body-length p) nil
   (lsp--parser-body-received p) nil
   (lsp--parser-headers p) '()
   (lsp--parser-body p) nil
   (lsp--parser-reading-body p) nil))

(defun lsp--parser-on-message (p msg)
  "Called when the parser reads a complete message from the server."
  (let* ((json-array-type 'list)
          (json-object-type 'hash-table)
          (json-false nil)
          (json-data (json-read-from-string msg))
          (id (gethash "id" json-data nil))
          (client (lsp--workspace-client (lsp--parser-workspace p)))
          callback)
    (pcase (lsp--get-message-type json-data)
      ('response
        (cl-assert id)
        (setq callback (gethash id (lsp--client-response-handlers client) nil))
        (if callback
          (progn (funcall callback (gethash "result" json-data nil))
            (remhash id (lsp--client-response-handlers client)))
          (setf (lsp--parser-response-result p)
            (and json-data (gethash "result" json-data nil))
            (lsp--parser-waiting-for-response p) nil)))
      ('response-error
        (setf (lsp--parser-response-result p) nil)
        (when json-data
          (message (lsp--error-string (gethash "error" json-data nil))))
        (setf (lsp--parser-response-result p) nil
          (lsp--parser-waiting-for-response p) nil))
      ('notification (lsp--on-notification p json-data))
      ('request      (lsp--on-request p json-data)))))

(defun lsp--parser-read (p output)
  (cl-assert (lsp--parser-workspace p) nil "Parser workspace cannot be nil.")
  (let ((messages '())
        (chunk (concat (lsp--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (lsp--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'lsp--parse-header
                                (split-string header-raw "\r\n")))
                       (body-length (lsp--get-body-length headers)))
                  (setf
                   (lsp--parser-headers p) headers
                   (lsp--parser-reading-body p) t
                   (lsp--parser-body-length p) body-length
                   (lsp--parser-body-received p) 0
                   (lsp--parser-body p) (make-string body-length ?\0)
                   (lsp--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (lsp--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (lsp--parser-body-length p))
               (received-body-length (lsp--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body
                (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (lsp--parser-body p) received-body-length this-body)
          (setf (lsp--parser-body-received p) (+ (lsp--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            ;; TODO: keep track of the Content-Type header, if
            ;; present, and use its value instead of just defaulting
            ;; to utf-8
            (push (decode-coding-string (lsp--parser-body p) 'utf-8) messages)
            (lsp--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun lsp--parser-make-filter (p ignore-regexps)
  #'(lambda (proc output)
      (setq lsp--no-response nil)
      (when (cl-loop for r in ignore-regexps
                     ;; check if the output is to be ignored or not
                     ;; TODO: Would this ever result in false positives?
                     when (string-match r output) return nil
                     finally return t)
        (let ((messages
               (condition-case err
                   (lsp--parser-read p output)
                 (error
                  (progn
                    (lsp--parser-reset p)
                    (setf (lsp--parser-response-result p) nil
                          (lsp--parser-waiting-for-response p) nil)
                    (error "Error parsing language server output: %s" err))))))

          (dolist (m messages)
            (when lsp-print-io (message "Output from language server: %s" m))
            (lsp--parser-on-message p m))))
      (when (lsp--parser-waiting-for-response p)
        (with-local-quit (accept-process-output proc)))))

(declare-function lsp--client-notification-handlers "lsp-methods" (client))
(declare-function lsp--client-request-handlers "lsp-methods" (client))
(declare-function lsp--workspace-client "lsp-methods" (workspace))
(declare-function lsp--workspace-apply-edit-handler "lsp-methods" (workspace params))

(provide 'lsp-io)
;;; lsp-io.el ends here
