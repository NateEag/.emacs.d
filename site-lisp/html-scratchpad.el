;;; html-scratchpad.el --- a local, Emacs-driven JSFiddle-alike.

;;; Package-Requires: ((simple-httpd "1.4.6"))

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:
;;;
;;; Thanks to simple-httpd.el, you too can try out quick experiments locally
;;; with just about no friction, and even send people on your network links to
;;; them.
;;;
;;; As with all things Emacs, how smooth this feels will depend on the quality
;;; of your web dev setup. I suppose long-term I could set it up to depend on
;;; the packages I recommend using with it...
;;;
;;; If you want your base directory to be different, just customize
;;; `httpd-root'.
;;;
;;; TODO:
;;;
;;;   * Add a few functions for pulling in common libs locally?
;;;   * Add a function to get a share URL (i.e. get public IP address/domain)
;;;   * Split out to standalone package?

;;; Code:

(require 'simple-httpd)

(defcustom html-scratchpad-index-page "<!doctype html>
<html lang=\"en\">

<head>
    <meta charset=\"utf-8\">
    <meta http-equiv=\"x-ua-compatible\" content=\"ie=edge\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">

    <title></title>

    <link rel=\"stylesheet\" href=\"styles.css\">
</head>

<body>

    <script src=\"main.js\"></script>
</body>

</html>"

  "HTML for a new index.html file.")

(defcustom html-scratchpad-basedir
  (expand-file-name "html-scratchpads" httpd-root)

  "Directory scratchpads live in.")

(defun html-scratchpad-get-path (name)
  "Return the full filesystem path to the scratchpad named `scratchpad-name'."

  (expand-file-name name html-scratchpad-basedir))

(defun html-scratchpad-open (name)
  "Open an HTML scratchpad's buffers and ensure webserver is up.

Will create the scratchpad `NAME' if it does not already exist."
  (interactive "MName scratchpad: ")

  (when (not (httpd-running-p))
    (httpd-start))

  (let* ((scratchpad-path (html-scratchpad-get-path name))
        (scratchpad-html-path (expand-file-name "index.html" scratchpad-path))
        (scratchpad-css-path (expand-file-name "styles.css" scratchpad-path))
        (scratchpad-js-path (expand-file-name "main.js" scratchpad-path)))

    (when (not (file-directory-p scratchpad-path))
      (make-directory scratchpad-path t))

    (find-file scratchpad-css-path)
    (save-buffer)

    (find-file scratchpad-js-path)
    (save-buffer)

    (find-file scratchpad-html-path)
    (insert html-scratchpad-index-page)
    (save-buffer)

    (browse-url (html-scratchpad-get-url name))))

(defun html-scratchpad-get-url (name)
  "Return a URL to the scratchpad stored as `NAME'."

  (concat (format "http://localhost:%d" httpd-port)
          (s-chop-prefix (expand-file-name httpd-root)
                         (html-scratchpad-get-path name)) "/"))

(defun html-scratchpad-delete (name)
  "Delete the scratchpad specified by `NAME' and close its buffers."

  ;; FIXME Make directory selection work like you'd actually want.
  (interactive "DScratchpad name: ")

  (let* ((scratchpad-path (html-scratchpad-get-path name)))
    (mapc (lambda (buffer)
            (when (string-match scratchpad-path (or (buffer-file-name buffer) ""))
              (kill-buffer buffer)))
          (buffer-list))
    (delete-directory scratchpad-path t)
    ))

(provide 'html-scratchpad)
;;; html-scratchpad.el ends here
