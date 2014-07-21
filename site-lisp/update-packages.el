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

(defun update-packages-get-package-desc (package src)
  "Return the package-desc for PACKAGE in SRC."
  (car (cdr (assq package src))))

(defun update-packages-newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (update-packages-get-package-desc package package-alist))
                            ; original version or'd above with below, but it
                            ; doesn't work with my version of package.el
                            ; (assq package package--builtins)
           (newest-pkg-desc (update-packages-get-package-desc package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-version local-pkg-desc)
                           (package-desc-version newest-pkg-desc))))))

(defun update-packages-upgrade-or-install-package (package)
  "Ensure latest version of PACKAGE is installed."
  (unless (update-packages-newest-package-installed-p package)
    (let ((pkg-desc (car (cdr (assq package package-alist)))))
      (when pkg-desc
        (package-delete pkg-desc))
      (and (assq package package-archive-contents)
           (package-install package)))))

(defun update-packages-upgrade-package-and-commit (package)
  "If PACKAGE can be upgraded, upgrade it and commit."
  (unless (update-packages-newest-package-installed-p package)
    (let ((package-desc (update-packages-get-package-desc package package-alist)))
      (update-packages-upgrade-or-install-package package)
      (require 'magit)
      (call-process magit-git-executable
                    nil
                    nil
                    nil
                    "add"
                    ;; Stage all modifications, including deletions.
                    "-u"
                    (expand-file-name package-user-dir))
      ;; Just need to get this doing package name and I should be set...
      (call-process magit-git-executable
                    nil
                    nil
                    nil
                    "commit"
                    "-m"
                    (concat "Update package " (package-desc-name package-desc))))))

(defun update-packages-update-installed-packages ()
  "Update all installed packages that can be updated."
  (interactive)

  (package-refresh-contents)

  (dolist (package package-activated-list)
    (update-packages-upgrade-package-and-commit package)))

;;; update-packages.el ends here
