;;; update-packages.el --- Update packages via package.el programmatically.

;;; Author: Lars Tveito, Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; I keep my elpa/ directory under version control, so that I can go back to
;; any previous state I want. This makes it easy to revert to old states, and
;; lets me reproduce my setup on new machines easily.
;;
;; One downside is the number of commits I have to make manually when I update
;; my packages. I prefer to do one commit per package, to make git-bisect
;; effective in hunting down regressions, and that's enough work that I find
;; myself putting off updates.
;;
;; This is mostly code by Lars Tveito, available in his dot-emacs repo:
;; https://github.com/larstvei/dot-emacs#package
;;
;; I've just tweaked it a bit for my use case.

;;; Code:

;; Looks like this broke again. Maybe when I upgraded to Emacs 27? Been a long
;; while since I updated packages.
(defun update-packages-get-package-desc (package src)
  "Return the package-desc for PACKAGE in SRC."
  (if (eq (assq package src) nil)
      (progn
        (message "%S" package)
        (error "Package %s is not in package list!

It probably got removed from MELPA or had its name changed.
Uninstall or upgrade it manually" package)))

  (car (cdr (assq package src))))

(defun update-packages-newest-package-installed-p (package)
  "Return non-nil if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (update-packages-get-package-desc package package-alist))
                            ; original version or'd above with below, but it
                            ; doesn't work with my version of package.el
                            ; (assq package package--builtins)
           (newest-pkg-desc (update-packages-get-package-desc package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-version local-pkg-desc)
                           (package-desc-version newest-pkg-desc))))))

(defun update-packages-package-incompatible? (package)
  "Return non-nil if PACKAGE is not compatible with current environment.

We're just peeking inside package.el's internals to make sure
installing this is not guaranteed to fail."

(package--incompatible-p (update-packages-get-package-desc package
                                                           package-archive-contents)))

(defun update-packages-upgrade-or-install-package (package)
  "Ensure latest version of PACKAGE is installed.

Does *not* install PACKAGE if it is known to not be installable."
  (unless (or (update-packages-newest-package-installed-p package)
              (update-packages-package-incompatible? package))
    (let ((pkg-desc (car (cdr (assq package package-alist)))))
      (when pkg-desc
        (package-delete pkg-desc t))
      (and (assq package package-archive-contents)
           (package-install package t)))))

(defun update-packages-upgrade-package-and-commit (package)
  "If PACKAGE can be upgraded, upgrade it and commit."
  (unless (or (update-packages-newest-package-installed-p package)
              (update-packages-package-incompatible? package))
    (let ((package-desc (update-packages-get-package-desc package package-alist))
          (git-executable (executable-find "git"))
          (cwd default-directory)
          (package-dir (expand-file-name package-user-dir)))

      (cd package-dir)
      (update-packages-upgrade-or-install-package package)

      ;; Stage changes to the packages directory.
      (call-process git-executable
                    nil
                    "*git-results*"
                    nil
                    "add"
                    "-A"
                    package-dir)

      ;; Stage changes to package-quickstart.el, as updating a package makes
      ;; changes to it.
      (call-process git-executable
                    nil
                    "*git-results*"
                    nil
                    "add"
                    "-A"
                    (expand-file-name (s-concat user-emacs-directory
                                                "package-quickstart.el")))

      ;; Commit changes.
      (call-process git-executable
                    nil
                    "*git-results*"
                    nil
                    "commit"
                    "-m"
                    (concat "Update package " (package-desc-full-name package-desc)))
      (cd cwd))))

(defun update-packages-update-installed-packages ()
  "Update all installed packages that can be updated."
  (interactive)

  (package-refresh-contents)

  (dolist (package package-activated-list)
    (update-packages-upgrade-package-and-commit package)))

;;; update-packages.el ends here
