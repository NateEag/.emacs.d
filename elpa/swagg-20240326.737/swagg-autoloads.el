;;; swagg-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "swagg" "swagg.el" (0 0 0 0))
;;; Generated autoloads from swagg.el

(autoload 'swagg-request "swagg" "\
Select an endpoint from Swagger DEFINITION and make a request.
When called interactively, you'll be prompted to select a
definition from `swagg-definitions' first.

After selecting a definition and an endpoint, you'll be prompted
to enter all path parameters and the request will be made.  The
result will be displayed in a separate buffer.

This function does not support requests with bodies.  For that,
see `swagg-request-with-rest-block'.

Also see `swagg-use-unique-buffer-per-request'.

\(fn DEFINITION)" t nil)

(autoload 'swagg-request-with-rest-block "swagg" "\
Select an endpoint from Swagger DEFINITION and make a request.
When called interactively, you'll be prompted to select a
definition from `swagg-definitions' first.

After selecting a definition and an endpoint, you'll be prompted
to enter all path parameters.  After that, a new (or already
existing) `org-mode' buffer will open and your request will be
inserted (with generated request body, if there is one) into this
buffer.  Now you can utilize any rest client to send your
request, like one of the following: verb, restclient.el, ob-http.
See this project's README to learn more about the rest clients.

If ARG is non-nil, then instead of inserting the request to a new
buffer, simply insert it to current buffer.

Also see `swagg-rest-block-prelude' and
`swagg-rest-block-postlude' to control the inserted rest block
surroundings and `swagg-rest-block-org-header-tags' to
automatically tag request's org-header.

\(fn DEFINITION &optional ARG)" t nil)

(autoload 'swagg-invalidate-cache "swagg" "\
Invalidate swagger definition JSON cache.
Useful if your swagger JSON/YAML has been changed.

If SELECT? is non-nil, instead of invalidating all definitions,
prompt user to select a definition to invalidate.

\(fn &optional SELECT\\=\\?)" t nil)

(register-definition-prefixes "swagg" '("swagg-"))

;;;***

;;;### (autoloads nil nil ("swagg-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; swagg-autoloads.el ends here
