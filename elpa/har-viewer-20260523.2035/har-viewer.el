;;; har-viewer.el --- Major mode for viewing HTTP Archive (HAR) files -*- lexical-binding: t -*-

;; Copyright (C) 2026 Gregory Newman

;; Author: Gregory Newman <bozoslivehere@protonmail.com>
;; Assisted-by: Claude:claude-sonnet-4.6
;; Package-Version: 20260523.2035
;; Package-Revision: 0b36dd0ef674
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, http, network
;; URL: https://github.com/bozoslivehere/har-viewer.el

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for viewing HTTP Archive (HAR) files.  Provides a
;; tabulated list of all captured requests and responses with commands
;; to inspect headers, request/response bodies, and copy entries as
;; cURL commands.
;;
;; Usage:
;; Enable `har-viewer-global-minor-mode' so that visiting any .har file
;; automatically binds C-c C-v to `har-viewer-view'.  Alternatively, call
;; `har-viewer-view' directly from a buffer containing HAR JSON.
;;
;; Keybindings in `har-viewer-mode':
;;
;;   RET        Display request and response headers
;;   C-c C-r    Display response body
;;   C-c C-p    Display request body
;;   C-c C-c    Copy entry as a cURL command (also: yc in evil normal state)
;;   C-c C-n    Narrow list by URL regex
;;
;; Optional integrations:
;;
;;   restclient  -- header buffers use restclient-mode when available
;;   web-beautify -- body buffers are auto-formatted when available
;;   evil         -- adds RET and yc normal-state bindings when available

;;; Code:

(require 'json)
(require 'tabulated-list)
(require 'url-parse)

(declare-function evil-define-key     "evil-core")
(declare-function restclient-mode     "restclient")
(declare-function web-beautify-js-buffer  "web-beautify")
(declare-function web-beautify-html-buffer "web-beautify")
(declare-function web-beautify-css-buffer  "web-beautify")

(defvar har-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'har-viewer-display-headers)
    (define-key map (kbd "C-c C-p") #'har-viewer-display-request-body)
    (define-key map (kbd "C-c C-c") #'har-viewer-copy-as-curl)
    (define-key map (kbd "C-c C-n") #'har-viewer-narrow-to-regex)
    (define-key map (kbd "C-c C-r") #'har-viewer-display-response-body)
    map)
  "Keymap for `har-viewer-mode'.")

;;;###autoload
(define-derived-mode har-viewer-mode tabulated-list-mode "HAR Viewer"
  "Major mode for viewing HTTP Archive (HAR) files."
  (setq tabulated-list-format [("Protocol" 8 t)
                               ("Method" 6 t)
                               ("Domain" 25 t)
                               ("Path" 65 t)
                               ("Status" 7 t)
                               ("Content-Type" 20 t)
                               ("Size" 5 t)
                               ("Time" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (when (featurep 'evil)
    (evil-define-key 'normal har-viewer-mode-map
      (kbd "RET") #'har-viewer-display-headers
      (kbd "yc")  #'har-viewer-copy-as-curl)))

;;; Buffer-local state

(defvar-local har-viewer--source-buffer nil
  "The buffer from which this HAR viewer was opened.")

(defvar-local har-viewer--entries-headers nil
  "List of (REQUEST-HEADERS RESPONSE-HEADERS) string lists, one per entry.")

(defvar-local har-viewer--response-bodies nil
  "List of (MIME-TYPE . TEXT) cons cells, one per entry.")

(defvar-local har-viewer--request-bodies nil
  "List of (MIME-TYPE . TEXT) cons cells, one per entry.")

;;; Configuration

(defgroup har-viewer nil
  "Major mode for viewing HTTP Archive (HAR) files."
  :group 'tools
  :prefix "har-viewer-")

(defvar har-viewer-beautify-bodies nil
  "When non-nil, auto-format body buffers using `web-beautify' if available.")

;;; Internal helpers

(defvar cl-struct-url-tags)

(defun har-viewer--parse-url (url)
  "Parse URL, handling blob: URLs by preserving the blob marker in the type."
  (if (string-prefix-p "blob:" url)
      (let* ((inner-url   (substring url 5))
             (parsed-url  (url-generic-parse-url inner-url))
             (protocol    (concat (url-type parsed-url) ":blob")))
        (setf (url-type parsed-url) protocol)
        parsed-url)
    (url-generic-parse-url url)))

(defun har-viewer--parse-file ()
  "Parse the HAR JSON in the current buffer.
Returns a plist with keys :entries, :headers, :request-bodies,
and :response-bodies."
  (save-excursion
    (goto-char (point-min))
    (let* ((json-object-type 'alist)
           (json    (json-read))
           (entries (alist-get 'entries (alist-get 'log json)))
           (entry-id 0)
           all-headers all-responses all-requests
           (parsed-entries
            (mapcar
             (lambda (entry)
               (setq entry-id (1+ entry-id))
               (let* ((request      (alist-get 'request entry))
                      (response     (alist-get 'response entry))
                      (url          (har-viewer--parse-url (alist-get 'url request)))
                      (status       (number-to-string (alist-get 'status response)))
                      (method       (alist-get 'method request))
                      (protocol     (url-type url))
                      (domain       (url-host url))
                      (path         (url-filename url))
                      (content-type (or (alist-get 'mimeType (alist-get 'content response))
                                        "[no content]"))
                      (size         (alist-get 'size (alist-get 'content response)))
                      (time         (alist-get 'time entry))
                      (req-headers  (alist-get 'headers request))
                      (res-headers  (alist-get 'headers response))
                      (req-headers-str
                       (if req-headers
                           (mapcar (lambda (h)
                                     (format "%s: %s"
                                             (alist-get 'name h)
                                             (alist-get 'value h)))
                                   req-headers)
                         '("No request headers.")))
                      (res-headers-str
                       (if res-headers
                           (mapcar (lambda (h)
                                     (format "%s: %s"
                                             (alist-get 'name h)
                                             (alist-get 'value h)))
                                   res-headers)
                         '("No response headers.")))
                      (post-mime (if (assoc 'postData request)
                                     (alist-get 'mimeType (alist-get 'postData request))
                                   ""))
                      (request-body
                       (if (assoc 'postData request)
                           (if (> size 0)
                               (if (assoc 'text (alist-get 'postData request))
                                   (cons post-mime
                                         (alist-get 'text (alist-get 'postData request)))
                                 (cons post-mime "[no data]"))
                             (cons "Empty" "[Empty request]"))
                         (cons "" "Nothing to show here...")))
                      (response-body
                       (if (and (> size 0) (assoc 'text (alist-get 'content response)))
                           (cons content-type
                                 (alist-get 'text (alist-get 'content response)))
                         (cons "Empty" "[empty response]"))))
                 (push (list req-headers-str res-headers-str) all-headers)
                 (push response-body all-responses)
                 (push request-body all-requests)
                 (list entry-id (vector protocol method domain path status
                                        content-type
                                        (format "%s" size)
                                        (format "%.0f ms" time)))))
             entries)))
      (list :entries        (nreverse parsed-entries)
            :headers        (nreverse all-headers)
            :response-bodies (nreverse all-responses)
            :request-bodies  (nreverse all-requests)))))

(defun har-viewer--mime-type-to-mode (mime-type)
  "Return the major mode symbol appropriate for MIME-TYPE."
  (cond ((string-match "javascript" mime-type) 'js-mode)
        ((string-match "json"       mime-type) 'json-mode)
        ((string-match "html"       mime-type) 'html-mode)
        ((string-match "xml"        mime-type) 'xml-mode)
        ((string-match "css"        mime-type) 'css-mode)
        (t 'text-mode)))

(defun har-viewer--beautify (mime-type)
  "Format the current buffer according to MIME-TYPE using web-beautify.
Does nothing unless `har-viewer-beautify-bodies' is non-nil and
the `web-beautify' package is loaded."
  (when (and (featurep 'web-beautify) har-viewer-beautify-bodies)
    (cond ((string-match "javascript" mime-type) (web-beautify-js-buffer))
          ((string-match "json"       mime-type) (json-pretty-print-buffer))
          ((string-match "html"       mime-type) (web-beautify-html-buffer))
          ((string-match "xml"        mime-type) (web-beautify-html-buffer))
          ((string-match "css"        mime-type) (web-beautify-css-buffer)))))

(defun har-viewer--open-details-window ()
  "Switch to the *HAR Details* window, creating it below if needed."
  (let ((details-window (get-buffer-window "*HAR Details*")))
    (if details-window
        (select-window details-window)
      (display-buffer-below-selected
       (get-buffer-create "*HAR Details*")
       '((window-height . 0.3))))))


;;; Commands

;;;###autoload
(defun har-viewer-view ()
  "Open a HAR viewer for the current buffer."
  (interactive)
  (let* ((source-buf (current-buffer))
         (data       (har-viewer--parse-file))
         (buf        (get-buffer-create "*HAR Viewer*")))
    (with-current-buffer buf
      (har-viewer-mode)
      (setq har-viewer--source-buffer  source-buf)
      (setq har-viewer--entries-headers  (plist-get data :headers))
      (setq har-viewer--response-bodies  (plist-get data :response-bodies))
      (setq har-viewer--request-bodies   (plist-get data :request-bodies))
      (setq tabulated-list-entries       (plist-get data :entries))
      (tabulated-list-print t)
      (switch-to-buffer buf))))

(defun har-viewer-display-headers ()
  "Display the headers for the HAR entry at point."
  (interactive)
  (let* ((entry-id        (1- (tabulated-list-get-id)))
         (headers-pair    (nth entry-id har-viewer--entries-headers))
         (request-headers (car headers-pair))
         (response-headers (cadr headers-pair))
         (current-window  (selected-window)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert "# Request Headers:\n")
      (if (and (listp request-headers) (stringp (car request-headers)))
          (dolist (header request-headers) (insert header "\n"))
        (insert "# No request headers.\n"))
      (insert "\n# Response Headers:\n")
      (if (and (listp response-headers) (stringp (car response-headers)))
          (dolist (header response-headers) (insert header "\n"))
        (insert "# No response headers.\n"))
      (goto-char (point-min))
      (when (fboundp 'restclient-mode) (restclient-mode))
      (select-window current-window))))

(defun har-viewer-display-response-body ()
  "Display the response body for the HAR entry at point."
  (interactive)
  (let* ((entry-id     (1- (tabulated-list-get-id)))
         (response     (nth entry-id har-viewer--response-bodies))
         (mime-type    (car response))
         (body-text    (cdr response))
         (current-window (selected-window))
         (mode         (har-viewer--mime-type-to-mode mime-type)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (har-viewer--beautify mime-type)
      (select-window current-window))))

(defun har-viewer-display-request-body ()
  "Display the request body for the HAR entry at point."
  (interactive)
  (let* ((entry-id     (1- (tabulated-list-get-id)))
         (request      (nth entry-id har-viewer--request-bodies))
         (mime-type    (car request))
         (body-text    (cdr request))
         (current-window (selected-window))
         (mode         (har-viewer--mime-type-to-mode mime-type)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (har-viewer--beautify mime-type)
      (select-window current-window))))

(defun har-viewer-copy-as-curl ()
  "Copy the HAR entry at point as a cURL command to the kill ring."
  (interactive)
  (let* ((entry-id      (1- (tabulated-list-get-id)))
         (entry-data    (tabulated-list-get-entry))
         (protocol      (replace-regexp-in-string ":blob$" "" (aref entry-data 0)))
         (method        (aref entry-data 1))
         (domain        (aref entry-data 2))
         (path          (aref entry-data 3))
         (headers-pair  (nth entry-id har-viewer--entries-headers))
         (req-headers   (car headers-pair))
         (request       (nth entry-id har-viewer--request-bodies))
         (body-text     (cdr request))
         (url           (format "%s://%s%s" protocol domain path))
         (parts         (list (format "curl -X '%s' \\\n  '%s'" method url))))
    (when (and (listp req-headers) (stringp (car req-headers)))
      (dolist (header req-headers)
        (setq parts (append parts
                            (list (format "  -H '%s'"
                                          (replace-regexp-in-string
                                           "'" "'\\''" header t t)))))))
    (when (and body-text
               (not (string= body-text "Nothing to show here..."))
               (not (string= body-text "[Empty request]"))
               (not (string= body-text "[no data]")))
      (setq parts (append parts
                          (list (format "  --data-raw '%s'"
                                        (replace-regexp-in-string
                                         "'" "'\\''" body-text t t))))))
    (kill-new (mapconcat #'identity parts " \\\n"))
    (message "Copied cURL command to kill ring.")))

(defun har-viewer-narrow-to-regex (regex)
  "Filter the HAR viewer to entries whose URL matches REGEX.
The full entry list is re-read from the source buffer so the
filter can be changed or cleared by calling this command again."
  (interactive "sNarrow to URL regex: ")
  (let* ((data    (with-current-buffer har-viewer--source-buffer
                    (har-viewer--parse-file)))
         (all     (plist-get data :entries))
         (filtered (seq-filter
                    (lambda (entry)
                      (let* ((v           (cadr entry))
                             (domain-path (concat (aref v 2) (aref v 3))))
                        (string-match-p regex domain-path)))
                    all)))
    (setq har-viewer--entries-headers  (plist-get data :headers))
    (setq har-viewer--response-bodies  (plist-get data :response-bodies))
    (setq har-viewer--request-bodies   (plist-get data :request-bodies))
    (setq tabulated-list-entries filtered)
    (tabulated-list-print t)))

;;; Integration hooks

(defvar har-viewer-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'har-viewer-view)
    map)
  "Keymap for `har-viewer-minor-mode'.")

;;;###autoload
(define-minor-mode har-viewer-minor-mode
  "Minor mode that provides \\[har-viewer-view] in .har file buffers."
  :lighter nil
  :group 'har-viewer
  :keymap har-viewer-minor-mode-map)

(defun har-viewer-minor-mode--maybe-enable ()
  "Enable `har-viewer-minor-mode' when visiting a .har file."
  (when (and buffer-file-name
             (string-match-p "\\.har\\'" buffer-file-name))
    (when (boundp 'so-long-predicate)
      (setq-local so-long-predicate #'ignore))
    (har-viewer-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode har-viewer-global-minor-mode
  har-viewer-minor-mode
  har-viewer-minor-mode--maybe-enable
  :group 'har-viewer)

(provide 'har-viewer)
;;; har-viewer.el ends here
