;;; envrc.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Package-Commit: aae15dd8c4736ceb515b09b1db9eef3db3bd65d9
;; Homepage: https://github.com/purcell/envrc
;; Package-Requires: ((seq "2") (emacs "24.4"))
;; Package-Version: 20200611.254
;; Package-X-Original-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use direnv (https://direnv.net/) to set environment variables on a
;; per-buffer basis.  This means that when you work across multiple
;; projects which have `.envrc` files, all processes launched from the
;; buffers "in" those projects will be executed with the environment
;; variables specified in those files.  This allows different versions
;; of linters and other tools to be installed in each project if
;; desired.

;; Enable `envrc-global-mode' late in your startup files.  For
;; interaction with this functionality, see `envrc-mode-map', and the
;; commands `envrc-reload', `envrc-allow' and `envrc-deny'.

;; In particular, you can enable keybindings for the above commands by
;; binding your preferred prefix to `envrc-command-map' in
;; `envrc-mode-map', e.g.

;;    (with-eval-after-load 'envrc
;;      (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

;;; Code:

;; TODO: special handling for DIRENV_* vars? exclude them? use them to safely reload more aggressively?
;; TODO: special handling for remote files
;; TODO: handle nil default-directory (rarely happens, but is possible)
;; TODO: limit size of *direnv* buffer
;; TODO: special handling of compilation-environment?
;; TODO: handle use of "cd" and other changes of `default-directory' in a buffer over time?
;; TODO: handle "allow" asynchronously?
;; TODO: describe env
;; TODO: click on mode lighter to get details
;; TODO: handle when direnv is not installed?
;; TODO: provide a way to disable in certain projects?

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'ansi-color)
(require 'cl-lib)

;;; Custom vars and minor modes

(defgroup envrc nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom envrc-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *envrc-debug* buffer."
  :type 'boolean)

(defcustom envrc--lighter '(:eval (envrc--lighter))
  "The mode line lighter for `envrc-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)

(put 'envrc--lighter 'risky-local-variable t)

(defcustom envrc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'envrc-allow)
    (define-key map (kbd "d") 'envrc-deny)
    (define-key map (kbd "r") 'envrc-reload)
    map)
  "Keymap for commands in `envrc-mode'.
See `envrc-mode-map' for how to assign a prefix binding to these."
  :type 'keymap)
(fset 'envrc-command-map envrc-command-map)

(defcustom envrc-mode-map (make-sparse-keymap)
  "Keymap for `envrc-mode'.
To access `envrc-command-map' from this map, give it a prefix keybinding,
e.g. (define-key envrc-mode-map (kbd \"C-c e\") 'envrc-command-map)"
  :type 'keymap)

;;;###autoload
(define-minor-mode envrc-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter envrc--lighter
  :keymap envrc-mode-map
  (if envrc-mode
      (envrc--register)
    (envrc--apply (current-buffer) nil)))

;;;###autoload
(define-globalized-minor-mode envrc-global-mode envrc-mode
  (lambda () (unless (or (minibufferp) (file-remote-p default-directory))
          (envrc-mode 1))))

(defface envrc-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface envrc-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface envrc-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

;;; Global state

(defvar envrc--envs (make-hash-table :test 'equal :size 10)
  "Known envrc directories and their direnv results, as produced by `envrc--export'.")

;;; Local state

(defvar-local envrc--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of '(none on error).")

;;; Internals

(defun envrc--lighter ()
  "Return a colourised version of `envrc--status' for use in the mode line."
  `(" env["
    (:propertize ,(symbol-name envrc--status)
                 face
                 ,(pcase envrc--status
                    (`on 'envrc-mode-line-on-face)
                    (`error 'envrc-mode-line-error-face)
                    (`none 'envrc-mode-line-none-face)))
    "]"))

(defun envrc--find-env-dir ()
  "Return the env dir for the current buffer, if any.
This is based on a file scan.  In most cases we prefer to use the
cached list of known dirs.

Regardless of buffer file name, we always use
`default-directory': the two should always match, unless the user
called `cd'"
  (let ((env-dir (locate-dominating-file default-directory ".envrc")))
    (when env-dir
      ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
      (setq env-dir (expand-file-name env-dir)))
    env-dir))

(defun envrc--register ()
  "Add the current buffer's env, if any.
All `envrc'-managed buffers with this env will have their
environments updated."
  (let ((env-dir (envrc--find-env-dir)))
    (when env-dir
      (pcase (gethash env-dir envrc--envs 'missing)
        (`missing (envrc--update-env env-dir))
        (values (envrc--apply (current-buffer) values))))))

(defmacro envrc--at-end-of-special-buffer (name &rest body)
  "At the end of `special-mode' buffer NAME, execute BODY.
To avoid confusion, `envrc-mode' is explicitly disabled in the buffer."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (unless (derived-mode-p 'special-mode)
       (special-mode))
     (when envrc-mode (envrc-mode -1))
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defun envrc--debug (msg &rest args)
  "A version of `message' which does nothing if `envrc-debug' is nil.
MSG and ARGS are as for that function."
  (when envrc-debug
    (envrc--at-end-of-special-buffer "*envrc-debug*"
      (insert (apply 'format msg args))
      (newline))))

(defun envrc--hash-table-to-alist (table)
  "Convert TABLE to an alist."
  (let (pairs)
    (maphash (lambda (k v) (push (cons k v) pairs)) table)
    pairs))

(defun envrc--directory-path-deeper-p (a b)
  "Return non-nil if directory path B is deeper than directory path A."
  (string-prefix-p (file-name-as-directory a) (file-name-as-directory b)))

(defun envrc--deepest-paths-first (paths)
  "Sort PATHS such that the deepest paths in a hierarchy appear first."
  (sort paths
        (lambda (a b) (or (envrc--directory-path-deeper-p b a)
                     (string< a b)))))

(defun envrc--update-env (env-dir)
  "Enable direnv management for ENV-DIR in `envrc-mode' buffers."
  (puthash env-dir (envrc--export env-dir) envrc--envs)
  (envrc--apply-all env-dir))

(defun envrc--export (env-dir)
  "Export the env vars for ENV-DIR using direnv.
Return value is either 'error, or a (possibly-empty)
alist of environment variable names and values."
  (unless (file-exists-p (expand-file-name ".envrc" env-dir))
    (error "%s is not a directory with a .envrc" env-dir))
  (message "Running direnv in %s..." env-dir)
  (let ((stderr-file (make-temp-file "envrc"))
        result)
    (unwind-protect
        (let ((default-directory env-dir))
          (with-temp-buffer
            (let ((exit-code (envrc--call-process-in-default-env "direnv" nil (list t stderr-file) nil "export" "json")))
              (envrc--debug "Direnv exited with %s and output: %S" exit-code (buffer-string))
              (if (zerop exit-code)
                  (progn
                    (message "Direnv succeeded in %s" env-dir)
                    (unless (zerop (buffer-size))
                      (goto-char (point-min))
                      (setq result (let ((json-key-type 'string)) (json-read-object)))))
                (message "Direnv failed in %s" env-dir)
                (setq result 'error))
              (envrc--at-end-of-special-buffer "*envrc*"
                (insert "==== " (format-time-string "%Y-%m-%d %H:%M:%S") " ==== " env-dir " ====\n\n")
                (let ((initial-pos (point)))
                  (insert-file-contents (let (ansi-color-context)
                                          (ansi-color-apply stderr-file)))
                  (goto-char (point-max))
                  (add-face-text-property initial-pos (point) (if (zerop exit-code) 'success 'error)))
                (insert "\n\n")
                (unless (zerop exit-code)
                  (display-buffer (current-buffer)))))))
      (delete-file stderr-file))
    result))

;; Forward declaration for the byte compiler
(defvar eshell-path-env)

(defun envrc--merged-environment (process-env pairs)
  "Make a `process-environment' value that merges PROCESS-ENV with PAIRS.
PAIRS is an alist obtained from direnv's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in PAIRS."
  (append (mapcar (lambda (pair)
                    (if (cdr pair)
                        (format "%s=%s" (car pair) (cdr pair))
                      ;; Plain env name is the syntax for unsetting vars
                      (car pair)))
                  pairs)
          process-env))

(defun envrc--apply (buf result)
  "Update BUF with RESULT, which is a result of `envrc--export'."
  (with-current-buffer buf
    (setq-local
     envrc--status
     (pcase result
       (`error 'error)
       (`() 'none)
       (_ 'on)))
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)
    (kill-local-variable 'eshell-path-env)
    (let ((pairs (when (listp result) result)))
      (if pairs
          (progn
            (envrc--debug "[%s] applied merged environment" buf)
            (setq-local process-environment (envrc--merged-environment process-environment pairs))
            (let ((path (getenv "PATH"))) ;; Get PATH from the merged environment: direnv may not have changed it
              (setq-local exec-path (parse-colon-path path))
              (when (derived-mode-p 'eshell-mode)
                (setq-local eshell-path-env path))))
        (envrc--debug "[%s] reset environment to default" buf)))))

(defun envrc--mode-buffers ()
  "Return a list of all live buffers in which `envrc-mode' is enabled."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                          (with-current-buffer b
                            envrc-mode)))
              (buffer-list)))

(defun envrc--apply-all (env-dir)
  "Update all direnv-managed buffers for ENV-DIR from `envrc--envs'."
  (envrc--debug "Updating all buffers in env %s" env-dir)
  (let ((env-dirs-deepest-paths-first (envrc--deepest-paths-first (hash-table-keys envrc--envs))))
    (envrc--debug "Env dirs deepest first: %S" env-dirs-deepest-paths-first)
    (dolist (buf (envrc--mode-buffers))
      (with-current-buffer buf
        (let ((default-directory (expand-file-name default-directory))) ;; Guard against abbreviation
          ;; Quickly check this buffer is at least "inside" this env
          (when (envrc--directory-path-deeper-p env-dir default-directory)
            ;; Then check that there is no nested env which is "closer"
            (let ((closest-env-dir (seq-find (lambda (dir)
                                               (envrc--directory-path-deeper-p dir default-directory))
                                             env-dirs-deepest-paths-first)))
              (when (string= env-dir closest-env-dir)
                (envrc--debug "[%s] updating from matching env dir %s" (buffer-name) env-dir)
                (envrc--apply buf (gethash env-dir envrc--envs))))))))))

(defmacro envrc--with-required-current-env (varname &rest body)
  "With VARNAME set to the current env dir path, execute BODY.
If there is no current env dir, abort with a user error."
  (declare (indent 1))
  (cl-assert (symbolp varname))
  `(let ((,varname (envrc--find-env-dir)))
     (unless ,varname
       (user-error "No enclosing .envrc"))
     ,@body))

(defun envrc--call-process-in-default-env (&rest args)
  "Like `call-process', but ensures the default `process-environment' is used.
ARGS is as for `call-process'."
  (let ((process-environment (default-value 'process-environment))
        (exec-path (default-value 'exec-path)))
    (apply 'call-process args)))

(defun envrc-reload ()
  "Reload the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--update-env env-dir)))

(defun envrc-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-in-default-env "direnv" nil (get-buffer-create "*envrc-allow*") nil "allow")))
      (if (zerop exit-code)
          (envrc--update-env env-dir)
        (display-buffer "*envrc-allow*")))))

(defun envrc-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir)
           (exit-code (envrc--call-process-in-default-env "direnv" nil (get-buffer-create "*envrc-deny*") nil "deny")))
      (if (zerop exit-code)
          (progn
            (puthash env-dir 'error envrc--envs)
            (envrc--apply-all env-dir))
        (display-buffer "*envrc-deny*")))))

(defun envrc-reload-all ()
  "Reload direnvs for all buffers.
This can be useful if a .envrc has been deleted."
  (interactive)
  (clrhash envrc--envs)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (envrc--register))))


(provide 'envrc)
;;; envrc.el ends here
