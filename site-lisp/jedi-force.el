;;; jedi-force.el -- jedi find current project's virtualenv automatically.

;;; Commentary:
;;;
;;;
;;;
;;; This is just [kenobi.el](https://gist.github.com/nyergler/6100112)
;;; modified to suit my own usage patterns.

;;; Code:

(defvar jedi-force-virtualenv-dir-name "virtualenv"
  "Name of directory containing a Python project's virtualenv.

If necessary, use .dir-locals.el to configure it on a per-project basis.")

(defvar jedi-force-extra-paths nil
  "List of directories to add to a Jedi instance's sys.path.

Best set in .dir-locals.el for a given project.")

(defun run-local-vars-mode-hook ()
  "Run a hook for the major-mode after local variables have been set up."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(defun jedi-force-find-buffer-virtualenv (buffer-name)
 "Return path to the current buffer's virtualenv, or nil if none is found."
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
  (let ((virtualenv-path (jedi-force-find-buffer-virtualenv buffer-file-name))
        )
    (make-local-variable 'jedi:server-args)

    (when virtualenv-path (set (make-local-variable 'jedi:server-args)
                               (list "--virtual-env" virtualenv-path)))

    (when jedi-force-extra-paths
      (dolist (path jedi-force-extra-paths)
        (set (make-local-variable 'jedi:server-args)
             (append jedi:server-args (list "--sys-path" path)))))))

(provide 'jedi-force)
;;; jedi-force.el ends here
