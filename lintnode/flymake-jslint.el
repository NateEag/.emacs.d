;; adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript
;;
;; Commentary:
;;
;; Installation:
;;
;; Put this in your load-path, then add the following to your .emacs.
;; You substitude espresso-mode-hook for javascript-mode-hook if you
;; use espresso.
;;
;;     (require 'flymake-jslint)
;;     (add-hook 'javascript-mode-hook
;;         (lambda () (lintnode-hook)))
;;
;; Configuration
;;
;; Do M-x customize-group flymake-jslint to customize paths, port and autostart.
;;
;; By default, all the jslint variables are on, however you can turn them off by adding
;; them to the variable lintnode-jslint-excludes - e.g. adding to your .emacs:
;;
;; (setq lintnode (list 'undef 'nomen))
;;
;; Usage:
;;
;; To start lintnode, either
;; * run M-x lintnode-start before invoking flymake.
;; * set the customize variable lintnode-autostart to t
;;


;; Code:
(require 'flymake)

(defcustom lintnode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-jslint)

(defcustom lintnode-location "~/emacs/lintnode"
  "The directory lintnode's app.js may be found in."
  :type 'string
  :group 'flymake-jslint)

(defcustom lintnode-port 3003
  "The port the lintnode server runs on."
  :type 'integer
  :group 'flymake-jslint)

(defcustom lintnode-autostart t
  "Whether to start lintnode automatically when we've called lintnode-hook"
  :type 'boolean
  :group 'flymake-jslint)

(defvar lintnode-jslint-excludes nil
  "a list of lisp symbols corresponding to jslint boolean options")

(defvar lintnode-jslint-includes nil
  "a list of lisp symbols corresponding to jslint boolean options")

(defvar lintnode-jslint-set nil
  "a string of comma seperated jslint options; values are seperated via colon, e.g. maxlen:80,node:true,eqeqeq:false")

(defun lintnode-start ()
  "Start the lintnode server.
Uses `lintnode-node-program' and `lintnode-location'."
  (interactive)
  (message "Starting lintnode")
  (let ((lintnode-location (expand-file-name (concat lintnode-location "/app.js")))
        (lintnode-excludes (if (not lintnode-jslint-excludes)
                               ""
                             (mapconcat 'identity (mapcar 'symbol-name lintnode-jslint-excludes) ",")))
		(lintnode-includes (if (not lintnode-jslint-includes)
                               ""
                             (mapconcat 'identity (mapcar 'symbol-name lintnode-jslint-includes) ","))))
					   
    (start-process "lintnode-server" "*lintnode*"
                   lintnode-node-program
                   lintnode-location
                   "--port" (number-to-string lintnode-port)
                   "--exclude" lintnode-excludes
				   "--include" lintnode-includes
				   "--set" lintnode-jslint-set)))

(defun lintnode-stop ()
  "stop the lintnode server process"
  (interactive)
  (if (get-process "lintnode-server")
      (kill-process "lintnode-server")))

(defun lintnode-restart()
  "Restart the lintnode server - typically we've fiddled with the configuration"
  (interactive)
  (lintnode-stop)
  (sit-for 1)
  (lintnode-start))

(defun lintnode-hook ()
  "When we open a file in javascript mode, we should check to see if there is a
   jslint process running. If there isn't we check to see if the user has set
   lintnode-autostart to t, then start the process and flymake-mode."
  (let ((proc (get-buffer-process "*lintnode*")))
        (if (not proc)
          (if lintnode-autostart
              (lintnode-start)))
        (flymake-mode t)))

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (jslint-url (format "http://127.0.0.1:%d/jslint" lintnode-port)))
    (list "curl" (list "--form" (format "source=<%s" local-file)
                       "--form" (format "filename=%s" local-file)
                       ;; FIXME: For some reason squid hates this curl invocation.
                       "--proxy" ""
                       jslint-url))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
              flymake-jslint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
              nil 1 2 3)
            flymake-err-line-patterns))

(provide 'flymake-jslint)
