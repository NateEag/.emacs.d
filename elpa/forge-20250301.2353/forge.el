;;; forge.el --- Access Git forges from Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/forge
;; Keywords: git tools vc

;; Package-Version: 20250301.2353
;; Package-Revision: 1c904090dfdc
;; Package-Requires: (
;;     (emacs "29.1")
;;     (compat "30.0.2.0")
;;     (closql "2.2.1")
;;     (emacsql "4.2.0")
;;     (ghub "4.2.2")
;;     (let-alist "1.0.6")
;;     (llama "0.6.1")
;;     (magit "4.3.1")
;;     (markdown-mode "2.7")
;;     (seq "2.24")
;;     (transient "0.8.5")
;;     (yaml "1.2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Work with Git forges, such as Github and Gitlab, from the comfort
;; of Magit and the rest of Emacs.

;; The schema of the database has not been finalized yet.  Until that
;; has happened it will occasionally have to be discarded.  For now
;; the database does not contain any information that cannot simply
;; be fetched again.

;;; Code:

(require 'magit)

(require 'forge-db)
(require 'forge-core)

(provide 'forge)

(require 'forge-repo)
(require 'forge-post)
(require 'forge-topic)
(require 'forge-issue)
(require 'forge-pullreq)
(require 'forge-revnote)
(require 'forge-notify)

(require 'forge-forgejo)
(require 'forge-github)
(require 'forge-gitlab)
(require 'forge-gitea)
(require 'forge-gogs)
(require 'forge-bitbucket)
(require 'forge-semi)

(require 'forge-commands)
(require 'forge-topics)
(require 'forge-repos)

;;; Add Sections

(defvar forge-add-default-sections t
  "Whether to add Forge's sections to `magit-status-sections-hook'.

If you want to disable this, then you must set this to nil before
`forge' is loaded.")

(when forge-add-default-sections
  (magit-add-section-hook 'magit-status-sections-hook #'forge-insert-pullreqs nil t)
  (magit-add-section-hook 'magit-status-sections-hook #'forge-insert-issues   nil t))

;;; Add Bindings

;;;###autoload
(defvar forge-add-default-bindings t
  "Whether to add Forge's bindings to various Magit keymaps.

If you want to disable this, then you must set this to nil before
`magit' is loaded.  If you do it before `forge' but after `magit'
is loaded, then `magit-mode-map' ends up being modified anyway.")

;;;###autoload
(with-eval-after-load 'magit-mode
  (when forge-add-default-bindings
    (keymap-set magit-mode-map "'" #'forge-dispatch)
    (keymap-set magit-mode-map "N" #'forge-dispatch)
    (keymap-set magit-mode-map "<remap> <magit-browse-thing>"
                #'forge-browse)
    (keymap-set magit-mode-map "<remap> <magit-copy-thing>"
                #'forge-copy-url-at-point-as-kill)))

;;;###autoload
(with-eval-after-load 'magit-repos
  (when forge-add-default-bindings
    (keymap-set magit-repolist-mode-map "N" #'forge-dispatch)))

;;;###autoload
(with-eval-after-load 'git-commit
  (when forge-add-default-bindings
    (keymap-set git-commit-mode-map "C-c C-v" #'forge-visit-topic)))

(when forge-add-default-bindings
  (keymap-set magit-commit-section-map "C-c C-v" #'forge-visit-topic)
  (keymap-set magit-branch-section-map "C-c C-v" #'forge-visit-topic)

  (transient-insert-suffix 'magit-dispatch "o"
    '("N" "Forge" forge-dispatch))

  (transient-append-suffix 'magit-fetch "m" '("n" forge-pull))
  (transient-append-suffix 'magit-fetch "n" '("N" forge-pull-notifications))

  (transient-append-suffix 'magit-pull  "m" '("n" forge-pull))
  (transient-append-suffix 'magit-pull  "n" '("N" forge-pull-notifications))

  (transient-append-suffix 'magit-branch "w"
    '("f" "pull-request" forge-checkout-pullreq))
  (transient-append-suffix 'magit-branch "W"
    '("F" "from pull-request" forge-branch-pullreq))

  (transient-append-suffix 'magit-remote "a"
    '("f" "Fork" forge-fork))
  (transient-insert-suffix 'magit-remote "d u"
    '("d s" "Set default branch" forge-set-default-branch))
  (transient-append-suffix 'magit-remote "d u"
    '("d r" "Rename default branch" forge-rename-default-branch))

  (transient-append-suffix 'magit-worktree "c"
    '("n" "pull-request worktree" forge-checkout-worktree))

  (transient-append-suffix 'magit-status-jump "w"
    '("Np" "Pull requests" forge-jump-to-pullreqs))
  (transient-append-suffix 'magit-status-jump "Np"
    '("Ni" "Issues" forge-jump-to-issues))

  (transient-append-suffix 'magit-merge "a"
    '(7 "M" "Merge using API" forge-merge)))

;;; Startup Asserts

(defconst forge--minimal-git "2.25.0")

(defun forge-startup-asserts ()
  (let ((version (magit-git-version)))
    (when (and version (version< version forge--minimal-git))
      (display-warning 'magit (format "\
Forge requires Git >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.

If you use Tramp to work inside remote Git repositories, then you
have to make sure a suitable Git is used on the remote machines
too.\n" forge--minimal-git version) :error))))

(if after-init-time
    (forge-startup-asserts)
  (add-hook 'after-init-hook #'forge-startup-asserts t))

;;; forge.el ends here
