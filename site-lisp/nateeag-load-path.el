;; Set up load-path to include relevant files in my .emacs.d folder.
;; Add appropriate directories to load-path.
(defun add-subdirs-to-front-of-load-path (path)
  "Add directories beneath path to the beginning of load-path."
  (let ((default-directory path))
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path)))
                (normal-top-level-add-subdirs-to-load-path))
                 load-path))))

(add-subdirs-to-front-of-load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
