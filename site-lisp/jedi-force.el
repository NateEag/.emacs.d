;;; jedi-force.el -- jedi find current project's virtualenv automatically.

;;; Commentary:
;;;
;;; "The Force is what gives a Jedi his power." -- Obi-Wan Kenobi
;;;
;;; [Jedi](http://jedi.jedidjah.ch/en/latest/) is an awesome tool for
;;; supporting Python auto-completion and jump-to-definition library for
;;; a variety of editors.
;;;
;;; By default, a Jedi server doesn't know what virtualenv a given Python
;;; source file relies on. With the aid of the Force, however, a Jedi will know
;;; where to look.
;;;
;;; This is mostly [kenobi.el](https://gist.github.com/nyergler/6100112)
;;; tweaked to fit my usage patterns. It doesn't have Kenobi's built-in support
;;; for finding .egg files, but you can use `jedi-force-extra-paths' for the
;;; same purpose.
;;;
;;; I would like to pull this out of my config and make it standalone. To do
;;; that, I need to add a one-shot setup function that configures the hooks for
;;; you, since setup isn't entirely trivial.

;;; Code:

(defvar jedi-force-virtualenv-dir-name "virtualenv"
  "Name of directory to check for a Python virtualenv.

If necessary, use .dir-locals.el to configure it on a per-project basis.")

(defvar jedi-force-extra-paths nil
  "List of directories to add to a Jedi instance's sys.path.

Best set in .dir-locals.el for a given project.")

(defvar jedi-force-find-buffer-virtualenv
  'jedi-force-find-virtualenv-in-buffer-path
  "Function to locate virtualenv for a buffer.

Should be a function that accepts a buffer-file-name and returns
the path to the corresponding virtualenv, or `nil' if none is found.

Defaults to `jedi-force-find-virtualenv-in-buffer-path'.")

(defun jedi-force-run-local-vars-mode-hook ()
  "Run additional hooks for `major-mode'.

Should be added to `hack-local-variables-hook', to run major-mode-specific
hooks on file-local variables."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(defun jedi-force-set-up-hooks ()
  "Install hooks to start Jedi with guidance from the Force.

Handles adding jedi:setup to hooks, so don't add it yourself."

  (add-hook 'hack-local-variables-hook 'jedi-force-run-local-vars-mode-hook)
  (add-hook 'python-mode-local-vars-hook 'jedi-force-setup-extra-args)
  (add-hook 'python-mode-local-vars-hook 'jedi:setup))

(defun jedi-force-find-virtualenv-in-buffer-path (buffer-name)
 "Look for a virtualenv anywhere above `buffer-name'."
 (let ((buffer-dir (file-name-directory buffer-name)))
   (while (and (not (file-exists-p
                     (concat
                      buffer-dir
                      jedi-force-virtualenv-dir-name
                      "/bin/activate")))
               buffer-dir)
     (setq buffer-dir
           (if (equal buffer-dir "/")
               nil
             (file-name-directory (directory-file-name buffer-dir))))
     )
   ;; Return result of search.
   (if buffer-dir
       (concat buffer-dir jedi-force-virtualenv-dir-name)
     buffer-dir)))

(defun jedi-force-setup-extra-args ()
  "Hook to set up jedi's arguments for the current buffer."
  (let ((virtualenv-path (apply jedi-force-find-buffer-virtualenv
                                (list buffer-file-name))))

    (make-local-variable 'jedi:server-args)

    (when virtualenv-path (set (make-local-variable 'jedi:server-args)
                               (list "--virtual-env" virtualenv-path)))

    (when jedi-force-extra-paths
      (dolist (path jedi-force-extra-paths)
        (set (make-local-variable 'jedi:server-args)
             (append jedi:server-args (list "--sys-path" path)))))))

(provide 'jedi-force)
;;; jedi-force.el ends here
