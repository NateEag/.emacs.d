;;; url-http-oauth-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "url-http-oauth" "url-http-oauth.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from url-http-oauth.el

(autoload 'url-http-oauth-interpose "url-http-oauth" "\
Arrange for Emacs to use OAuth 2.0 to access a URL using URL-SETTINGS.
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
a full URL containing a code value in its query string.

\(fn URL-SETTINGS)" nil nil)

(autoload 'url-http-oauth-uninterpose "url-http-oauth" "\
Arrange for Emacs not to use OAuth 2.0 when accessing URL in URL-SETTINGS.
This function does the opposite of `url-http-oauth-interpose'.

\(fn URL-SETTINGS)" nil nil)

(autoload 'url-http-oauth-interposed-p "url-http-oauth" "\
Return non-nil if `url' will use OAuth 2.0 to access URL.
URL is an object.

\(fn URL)" nil nil)

(autoload 'url-oauth-auth "url-http-oauth" "\
Return an OAuth 2.0 HTTP authorization header.
URL is an object representing a parsed URL.  It should specify a
user, and contain a \"scope\" query argument representing the
permissions that the caller is requesting.

\(fn URL &optional PROMPT OVERWRITE REALM ARGS)" nil nil)

(url-register-auth-scheme "oauth" nil 9)

(register-definition-prefixes "url-http-oauth" '("url-http-oauth--"))

;;;***

;;;### (autoloads nil "url-http-oauth-demo" "url-http-oauth-demo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from url-http-oauth-demo.el

(register-definition-prefixes "url-http-oauth-demo" '("url-http-oauth-demo-get-profile-name"))

;;;***

;;;### (autoloads nil nil ("url-http-oauth-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; url-http-oauth-autoloads.el ends here
