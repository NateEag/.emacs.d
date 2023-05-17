;;; url-http-oauth.el --- OAuth 2.0 for URL library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 0.8.3
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; url-http-oauth adds OAuth 2.0 support to Emacs's URL library.
;;
;; Installation:
;;
;; M-x package-install RET url-http-oauth
;;
;; Usage:
;;
;; See url-http-oauth-demo.el, which is installed alongside
;; url-http-oauth.el.

;;; Code:
(require 'url-auth)
(require 'url-http)
(require 'url-util)
(require 'json)

(defvar url-http-oauth--interposed nil
  "A list of OAuth 2.0 settings association lists.")

(defvar url-http-oauth--interposed-regexp nil
  "A regular expression matching URLs.
If a URL matches this regular expression, `url' will use this
`url-http-oauth' to access resources at the URL via OAuth 2.0.")

(defun url-http-oauth--url-string (url)
  "Return URL as a string.
URL is string or an object."
  (if (stringp url) url (url-recreate-url url)))

(defun url-http-oauth--url-object (url)
  "Return URL as a parsed URL object.
URL is a string or an object."
  (if (stringp url) (url-generic-parse-url url) url))

(defun url-http-oauth--url-no-query (url)
  "Return an object representing URL with no query components.
URL is a string or an object."
  (let ((url (url-http-oauth--url-object url)))
    (url-parse-make-urlobj
     (url-type url)
     nil nil
     (url-host url)
     (url-portspec url)
     (car (url-path-and-query url))
     nil nil t)))

(defun url-http-oauth--settings (url)
  "Return a settings list if URL needs OAuth 2.0, nil otherwise.
URL is an object or a string."
  (let* ((url (url-http-oauth--url-string url)))
    (catch 'found
      (dolist (settings url-http-oauth--interposed)
        (when (or (string-prefix-p (cdr (assoc "resource-url" settings)) url)
                  (catch 'match
                    (dolist (prefix (cdr (assoc "resource-url-prefixes"
                                                settings)))
                      (when (string-prefix-p prefix url)
                        (throw 'match t)))))
          (throw 'found settings))))))

(defun url-http-oauth--update-regexp ()
  "Update `url-http-oauth--interposed-regexp'."
  (let (all-urls)
    (dolist (settings url-http-oauth--interposed)
      (let ((resource-url (cdr (assoc "resource-url" settings)))
            (resource-url-prefixes
             (cdr (assoc "resource-url-prefixes" settings))))
        (when resource-url
          (push resource-url all-urls))
        (when resource-url-prefixes
          (dolist (prefix resource-url-prefixes)
            (push prefix all-urls)))))
    (setq url-http-oauth--interposed-regexp (regexp-opt all-urls))))

;; Maybe if RFC 8414, "OAuth 2.0 Authorization Server Metadata",
;; catches on, authorization-url and access-token-url can be made
;; optional and their values retrieved automatically.  As of early
;; 2023, RFC 8414 is not consistently implemented yet.
;;;###autoload
(defun url-http-oauth-interpose (url-settings)
  "Arrange for Emacs to use OAuth 2.0 to access a URL using URL-SETTINGS.
URL-SETTINGS is an association list (alist) with fields whose
descriptions follow.  URL will be accessed by Emacs's `url'
library with a suitable \"Authorization\" header containing
\"Bearer <token>\".

RESOURCE-URL is a string representing the main URL at which
resources will be accessed.  RESOURCE-URL-PREFIXES is a list of
strings.  The same bearer token that is used to access resources
at RESOURCE-URL will be used for URLs that match a prefix string
in RESOURCE-URL-PREFIXES.

AUTHORIZATION-ENDPOINT and ACCESS-TOKEN-ENDPOINT are strings
representing URLs that will be used to acquire <token>.
Retrieved tokens will be saved it to the user's `auth-sources'
file.

CLIENT-IDENTIFIER is a string identifying an Emacs library or
mode to the server.  SCOPE is a string defining the -permissions
that the Emacs library or mode is requesting.

CLIENT-SECRET-METHOD is the symbol `prompt' if a client secret is
required, nil otherwise.  The client secret will be saved to the
user's `auth-sources' file.

SCOPE is a string, a space delimited list of requested permission
scopes.  These scopes are not standardized, but they may be
required or recommended by the OAuth 2.0 provider.

AUTHORIZATION-EXTRA-ARGUMENTS is an alist of URL query key/value
pairs that will be appended to the authorization URL.  Specific
pairs in this list are not standardized but may be required or
recommended by the OAuth 2.0 provider.  Examples of string types
include RESOURCE, RESPONSE_MODE, LOGIN_HINT, PROMPT and
REDIRECT_URI.

AUTHORIZATION-CODE-FUNCTION is an elisp function that takes an
authorization URL as a string argument, and returns, as a string,
a full URL containing a code value in its query string."
  (let ((client-identifier (cdr (assoc "client-identifier" url-settings)))
        (client-secret-method
         (cdr (assoc "client-secret-method" url-settings))))
    (unless (and (stringp client-identifier) (> (length client-identifier) 0))
      (error "url-http-oauth: Unset client-identifier value"))
    (unless (or (eq client-secret-method 'prompt) (eq client-secret-method nil))
      (error "url-http-oauth: Unrecognized client-secret-method value"))
    (prog1
        (add-to-list 'url-http-oauth--interposed url-settings)
      (url-http-oauth--update-regexp))))

;;;###autoload
(defun url-http-oauth-uninterpose (url-settings)
  "Arrange for Emacs not to use OAuth 2.0 when accessing URL in URL-SETTINGS.
This function does the opposite of `url-http-oauth-interpose'."
  (prog1
      (setq url-http-oauth--interposed
            (delete url-settings url-http-oauth--interposed))
    (url-http-oauth--update-regexp)))

;;;###autoload
(defun url-http-oauth-interposed-p (url)
  "Return non-nil if `url' will use OAuth 2.0 to access URL.
URL is an object."
  (when url-http-oauth--interposed-regexp
    (string-match-p url-http-oauth--interposed-regexp
                    (url-http-oauth--url-string url))))

(defvar url-http-response-status)
(defvar auth-source-creation-prompts)

(defun url-http-oauth--port (url)
  "Return port of URL.
Assume an HTTPS URL that does not specify a port uses 443.  URL
is a string or an object."
  (let ((port-number (url-port (url-http-oauth--url-object url))))
    (if port-number
        (number-to-string port-number)
      (when (string= "https" (url-type url)) "443"))))

;; Backport of `auth-info-password'.
(defun url-http-oauth--auth-info-password (auth-info)
  "Return the :secret password from the AUTH-INFO."
  (let ((secret (plist-get auth-info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

;; Backport (roughly) of `json-parse-buffer'.
(defun url-http-oauth--json-parse-buffer ()
  "See `json-parse-buffer'."
  (let ((json-object-type 'hash-table))
    (json-read-from-string
     (buffer-substring (point) (point-max)))))

(defun url-http-oauth--auth-source-search (url &optional user secret prompt
                                               expiry refresh-token)
  "Find the `auth-source' entry for USER and URL.
Arrange for the entry to be created if it is not already saved in
on of `auth-sources'.  URL is a string or an object.  USER is a
string.  SECRET is a string if the password is already known and
needs to be saved, or nil meaning to prompt for the password.  If
SECRET is nil, PROMPT should be a string with which the user will
be prompted to enter the password.  EXPIRY is a string
representing the epoch-time at which SECRET becomes invalid.
REFRESH-TOKEN is a string that can be sent to the authorization
server to receive a new access token."
  (let* ((auth-source-creation-prompts (when prompt `((secret . ,prompt))))
         (create (when (or secret prompt)
                   (if (or expiry refresh-token)
                       `(,@(when expiry (list 'expiry))
                         ,@(when refresh-token (list 'refresh-token)))
                     t)))
         (spec `(:user ,(or user "") ; "" => omit "user" field from authinfo.
                       ;; Misuse the host field: insert the full URL.
                       ;; This allows different authentication for
                       ;; different URL paths on the same host.  The
                       ;; `auth-source' netrc backend does not have
                       ;; search support for arbitrary fields, like a
                       ;; hypothetical :path that would be desirable
                       ;; in this case.  Introducing support for
                       ;; arbitrary fields would have too many forward
                       ;; and backward compatibility implications for
                       ;; netrc-formatted authinfo files.
                       :host ,(url-http-oauth--url-string
                               (url-http-oauth--url-no-query url))
                       :port ,(url-http-oauth--port url)
                       ,@(when secret (list :secret secret))
                       ,@(when expiry (list :expiry expiry))
                       ,@(when refresh-token
                           (list :refresh-token refresh-token))
                       ,@(when create (list :create create)))))
    (car ; First result always wins.
     (let ((auth-source-do-cache nil)) ; Do not cache nil result.
       (apply #'auth-source-search spec)))))

(defun url-http-oauth--parse-grant ()
  "Parse the JSON grant structure in the current buffer.
Return the parsed JSON object."
  ;; (message "url-http-oauth: grant: %s" (buffer-string))
  (progn
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (let* ((grant (url-http-oauth--json-parse-buffer))
           (type (gethash "token_type" grant)))
      (unless (equal (downcase type) "bearer")
        (error "url-http-oauth: Unrecognized token type %s" type))
      ;; Return grant object.
      grant)))

(defun url-http-oauth--get-access-token-grant (url-settings code)
  "Get an access token for using CODE.
URL-SETTINGS contain the client identifier and access token
endpoint."
  (let* ((url-request-method "POST")
         (access-token-url (cdr (assoc "access-token-endpoint" url-settings)))
         (client-identifier (cdr (assoc "client-identifier" url-settings)))
         (client-secret-method (cdr (assoc "client-secret-method"
                                           url-settings)))
         (auth-result
          (when client-secret-method
            (url-http-oauth--auth-source-search
             access-token-url client-identifier nil
             "Client secret for %u at %h: ")))
         (client-secret (url-http-oauth--auth-info-password auth-result))
         (save-function (plist-get auth-result :save-function))
         (authorization (when client-secret
                          (concat
                           "Basic "
                           (base64-encode-string
                            (format "%s:%s" client-identifier client-secret)
                            t))))
         (url-request-extra-headers
          (apply #'list
                 (cons "Content-Type" "application/x-www-form-urlencoded")
                 (when authorization
                   (list (cons "Authorization" authorization)))))
         (redirect-uri
          (cdr (assoc "redirect_uri" (cdr (assoc "authorization-extra-arguments"
                                                 url-settings)))))
         (url-request-data
          (url-build-query-string
           (apply #'list (list "code" code)
                  (when (not authorization)
                    ;; Client identifier is included in authorization
                    ;; string.  Some services object to it also being
                    ;; in the body.
                    (list "client_id" client-identifier))
                  (list "grant_type" "authorization_code")
                  (when redirect-uri
                    (list (list "redirect_uri" redirect-uri)))))))
    (with-current-buffer (url-retrieve-synchronously access-token-url)
      (if (eq 'OK (car (alist-get url-http-response-status url-http-codes)))
          (prog1
              (url-http-oauth--parse-grant)
            ;; Success, so save client secret, if necessary.
            (when (functionp save-function)
              (funcall save-function)))
        (error "url-http-oauth: Failed to get access token with %s"
               (buffer-string))))))

(defun url-http-oauth--expiry-string (grant)
  "Return as a string a number representing the expiry time of GRANT.
The time is in seconds since the epoch."
  (let ((expiry (gethash "expires_in" grant)))
    (unless expiry (error "url-http-oauth: Did not find expiry time in grant"))
    (format-time-string "%s" (time-add nil (if (stringp expiry)
                                               (string-to-number expiry)
                                             expiry)))))

(defun url-http-oauth--refresh-token-string (grant)
  "Return the refresh token from GRANT.
The refresh token is an opaque string."
  (gethash "refresh_token" grant))

(defun url-http-oauth--extract-authorization-code (url)
  "Extract the value of the code parameter in URL."
  (let ((query (cdr (url-path-and-query (url-generic-parse-url url)))))
    (unless query
      (error "url-http-oauth: Expected URL with query component"))
    (let ((code (cadr (assoc "code" (url-parse-query-string query)))))
      (unless code
        (error "url-http-oauth: Failed to find code in query component"))
      code)))

(defun url-http-oauth--authorization-url (url-settings)
  "Return the authorization URL for URL-SETTINGS."
  (let ((base (cdr (assoc "authorization-endpoint" url-settings)))
        (client
         (list "client_id" (cdr (assoc "client-identifier" url-settings))))
        (response-type (list "response_type" "code"))
        (scope (list "scope" (cdr (assoc "scope" url-settings))))
        (extra (mapcar (lambda (entry)
                         (list (car entry) (cdr entry)))
                       (cdr (assoc "authorization-extra-arguments"
                                   url-settings)))))
    (concat base "?" (url-build-query-string
                      (apply #'list client response-type scope extra)))))

(defun url-http-oauth--url-build-refresh (url-settings)
  "Build a refresh token URL query string from URL-SETTINGS."
  (let* ((client-identifier (cdr (assoc "client-identifier" url-settings)))
         (authorization-extra-arguments
          (cdr (assoc "authorization-extra-arguments" url-settings)))
         (resource (cdr (assoc "resource" authorization-extra-arguments)))
         (redirect-uri
          (cdr (assoc "redirect_uri" authorization-extra-arguments))))
    (url-build-query-string
     (apply #'list
            (let ((resource-url
                   (cdr (assoc "resource-url" url-settings)))
                  (error-message
                   "url-http-oauth: Failed to retrieve refresh token for %s"))
              (list "refresh_token"
                    (or (plist-get
                         (url-http-oauth--auth-source-search resource-url)
                         :refresh-token)
                        (error error-message resource-url))))
            (list "client_id" client-identifier)
            (list "grant_type" "refresh_token")
            (list "resource" resource)
            (when redirect-uri
              (list (list "redirect_uri" redirect-uri)))))))

;; This monstrosity is required because the `auth-source' netrc
;; backend does not support deletion, yet we need to refresh the
;; bearer token.
(defun url-http-oauth--netrc-delete (host &optional user port)
  "Delete a netrc entry matching HOST, USER and PORT.
Delete the first matching line from any `auth-source' backend.
The entry is cleared from the `password-data' cache after the
`auth-source' file is saved.  Respects
`auth-source-save-behavior'."
  (dolist (backend (mapcar #'auth-source-backend-parse auth-sources))
    (when (eq (slot-value backend 'type) 'netrc)
      (let* ((file (oref backend source))
             (results (auth-source-netrc-normalize
                       (auth-source-netrc-parse
                        :max 1
                        :file (oref backend source)
                        :host (or host t)
                        :user (or user t)
                        :port (or port t))
                       file)))
        (when results
          (with-temp-buffer
            (when (file-exists-p file)
              (insert-file-contents file))
            (when auth-source-gpg-encrypt-to
              ;; (see bug#7487) making `epa-file-encrypt-to' local to
              ;; this buffer lets epa-file skip the key selection query
              ;; (see the `local-variable-p' check in
              ;; `epa-file-write-region').
              (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
                (make-local-variable 'epa-file-encrypt-to))
              (if (listp auth-source-gpg-encrypt-to)
                  (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))
            ;; we want the new data to be found first, so insert at beginning
            (goto-char (point-min))
            ;; Ask AFTER we've successfully opened the file.
            (let* ((allow-null t)
                   (start-point (point-min))
                   (prior-start-point
                    (catch 'point
                      (auth-source-netrc-parse-entries
                       (lambda (alist)
                         (let ((end-point (point)))
                           (if (and alist
                                    (or
                                     (and allow-null (null host))
                                     (auth-source-search-collection
                                      host
                                      (or
                                       (auth-source--aget alist "machine")
                                       (auth-source--aget alist "host")
                                       t)))
                                    (or
                                     (and allow-null (null user))
                                     (auth-source-search-collection
                                      user
                                      (or
                                       (auth-source--aget alist "login")
                                       (auth-source--aget alist "account")
                                       (auth-source--aget alist "user")
                                       t)))
                                    (or
                                     (and allow-null (null port))
                                     (auth-source-search-collection
                                      port
                                      (or
                                       (auth-source--aget alist "port")
                                       (auth-source--aget alist "protocol")
                                       t))))
                               (throw 'point start-point)
                             (progn
                               (setq start-point end-point)
                               nil))))
                       1))))
              (when prior-start-point
                (goto-char prior-start-point)
                (auth-source-netrc-parse-next-interesting)
                (goto-char (line-beginning-position))
                (let ((extents
                       (if (bobp)
                           (progn
                             (goto-char (line-end-position))
                             (if (eobp)
                                 (cons (line-beginning-position)
                                       (line-end-position))
                               (cons (line-beginning-position)
                                     (1+ (line-end-position)))))
                         (progn
                           (goto-char (line-end-position))
                           (cons (1- (line-beginning-position))
                                 (line-end-position))))))
                  (let ((region-to-delete (buffer-substring (car extents)
                                                            (cdr extents))))
                    (when (or (not (eq auth-source-save-behavior 'ask))
                              (y-or-n-p (format "Delete region %S and save? "
                                                region-to-delete)))
                      (delete-region (car extents) (cdr extents))
                      (write-region (point-min) (point-max) file nil 'silent)
                      ;; Make the .authinfo file non-world-readable.
                      (set-file-modes file #o600)
                      (auth-source-do-debug
                       "auth-source-netrc-create: deleted region %S from %s"
                       region-to-delete file)
                      (auth-source-forget+ (list :host (or host t)
                                                 :user (or user t)
                                                 :port (or port t)))
                      nil)))))))))))

(defun url-http-oauth--save-bearer (url grant)
  "Save bearer access token for URL from GRANT.
URL is a string or an object.  GRANT is a parsed JSON object.
Save the bearer token to `auth-sources' then return it."
  (url-http-oauth--netrc-delete url)
  (let* ((bearer-retrieved (gethash "access_token" grant))
         (auth-result
          (url-http-oauth--auth-source-search
           url nil bearer-retrieved nil
           (url-http-oauth--expiry-string grant)
           (url-http-oauth--refresh-token-string grant)))
         (save-function (plist-get auth-result :save-function)))
    (when (functionp save-function)
      (funcall save-function))
    bearer-retrieved))

;; FIXME: If a refresh token fails then maybe look for status = 401
;; response with: WWW-Authenticate: Bearer
;; client_id="00000000-0000-0000-0000-000000000000",
;; trusted_issuers="00000000-0000-0000-0000-000000000000@*",
;; token_types="app_asserted_user_v1 service_asserted_app_v1",
;; authorization_uri=
;; "https://login.microsoftonline.com/common/oauth2/authorize",
;; error="invalid_token",Basic Realm="" in which case, call refresh on
;; URL before proceeding.
(defun url-http-oauth--refresh-access-token-grant (url-settings)
  "Refresh access token using URL-SETTINGS."
  (let* ((url-request-method "POST")
         (access-token-url (cdr (assoc "access-token-endpoint" url-settings)))
         (url-request-data (url-http-oauth--url-build-refresh url-settings)))
    (with-current-buffer (url-retrieve-synchronously access-token-url)
      (if (eq 'OK (car (alist-get url-http-response-status url-http-codes)))
          (url-http-oauth--parse-grant)
        (error "url-http-oauth: Failed to get access token with %s"
               (buffer-string))))))

(defun url-http-oauth--retrieve-and-save-bearer (url url-settings)
  "Retrieve the bearer token required for URL, using URL-SETTINGS.
Save the bearer token to `auth-sources' upon success."
  (let* ((authorization-code-function
          (cdr (assoc "authorization-code-function" url-settings)))
         (authorization-url (url-http-oauth--authorization-url url-settings))
         (response-url
          (if (functionp authorization-code-function)
              (funcall authorization-code-function authorization-url)
            (read-from-minibuffer
             (format "Browse to %s and paste the redirected code URL: "
                     authorization-url))))
         (code
          (url-http-oauth--extract-authorization-code response-url))
         (grant (url-http-oauth--get-access-token-grant url-settings code)))
    (url-http-oauth--save-bearer url grant)))

;; FIXME: if anything goes wrong during the authentication steps,
;; `url-http-end-of-document-sentinel' calls back into
;; `url-oauth-auth' somehow.  Maybe `url-http-no-retry' can help here?
(defun url-http-oauth--get-bearer (url)
  "Prompt the user with the authorization endpoint for URL.
URL is a parsed object."
  (let* ((url-settings (url-http-oauth--settings url))
         (resource-url (cdr (assoc "resource-url" url-settings))))
    (let ((expiry (plist-get (url-http-oauth--auth-source-search resource-url)
                             :expiry)))
      (when (and expiry (> (time-to-seconds) (string-to-number expiry)))
        (url-http-oauth--save-bearer
         resource-url
         (url-http-oauth--refresh-access-token-grant url-settings))))
    (let ((bearer-current (url-http-oauth--auth-info-password
                           (url-http-oauth--auth-source-search resource-url))))
      (or bearer-current
          (url-http-oauth--retrieve-and-save-bearer resource-url
                                                    url-settings)))))

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is an object representing a parsed URL.  It should specify a
user, and contain a \"scope\" query argument representing the
permissions that the caller is requesting."
  (when (url-http-oauth-interposed-p url)
    (let ((bearer (url-http-oauth--get-bearer url)))
      (if bearer
          (concat "Bearer " bearer)
        (error "url-http-oauth: Bearer retrieval failed for %s" url)))))

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
