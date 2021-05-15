;;; magrant-box.el --- Emacs interface to Vagrant commands on boxes  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Jordan Besly

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This sub-module to magrant.el contains all the box related functions.
;;
;; A "box", in Vagrant lingo, is a pre-configured VM/container template that can be used a a basis to create "machines" (instances).


;;; Code:


;; REQUIRES

(require 'rx)
(require 's)
(require 'dash)
(require 'tablist)
(require 'transient)

(require 'magrant-core)
(require 'magrant-utils)



;; VARS

(defgroup magrant-box nil
  "vagrant boxes customization group."
  :group 'magrant)

(defcustom magrant-box-default-sort-key '("Name" . nil)
  "Sort key for vagrant boxes.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'magrant-box
  :type '(cons (choice (const "Name")
                       (const "Provider")
                       (const "Version"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))



;; CLI CALLS

(defun magrant-box-refresh ()
  "Refresh the boxes list."
  (setq tabulated-list-entries (magrant-box-entries)))

(defun magrant-box-entries ()
  "Return the vagrant boxes data for `tabulated-list-entries'."
  (let* ((data (magrant-run-vagrant "box list"))
         (lines (s-split "\n" data t)))
    (-map #'magrant-box-parse lines)))

(defun magrant-box-parse (line)
  "Convert a LINE from \"vagrant box list\" to a `tabulated-list-entries' entry."
  (string-match (rx bol
                    (group (one-or-more anything))
                    space
                    "("
                    (group (one-or-more anything))
                    ","
                    space
                    (group (one-or-more anything))
                    ")"
                    eol)
                line)
  (let ((tab-line (vector
                   (s-trim (match-string 1 line))
                   (match-string 2 line)
                   (match-string 3 line))))
    (list (aref tab-line 0) tab-line)))

(defun magrant-box-read-name ()
  "Read an box name."
  (completing-read "Box: " (-map #'car (magrant-box-entries))))



;; MODE

(defvar magrant-box-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'magrant-box-help)
    (define-key map "A" #'magrant-box-add)
    (define-key map "U" #'magrant-box-update)
    ;; (define-key map "O" #'magrant-box-outdated)
    ;; (define-key map "P" #'magrant-box-prune)
    (define-key map "D" #'magrant-box-remove)
    ;; (define-key map "R" #'magrant-box-repackage)
    (define-key map "l" #'magrant-box-list)
    map)
  "Keymap for `magrant-box-mode'.")

(define-derived-mode magrant-box-mode tabulated-list-mode "Boxes Menu"
  "Major mode for handling a list of vagrant boxes."
  (setq tabulated-list-format [("Name" 30 t)("Provider" 15 t)("Version" 16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key magrant-box-default-sort-key)
  (add-hook 'tabulated-list-revert-hook #'magrant-box-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))



;; TRANSIENT: ENTRY POINT

(transient-define-prefix magrant-box-help ()
  "Help transient for vagrant boxes."
  ["Vagrant boxes help"
   ("A" "Add"        magrant-box-add)
   ("U" "Update"     magrant-box-update)
   ;; ("O" "Outdated"   magrant-box-outdated)
   ;; ("P" "Prune"      magrant-box-prune)
   ("D" "Remove"     magrant-box-remove)
   ;; ("R" "Repackage"  magrant-box-repackage)
   ("l" "List"       magrant-box-list)])

(transient-define-prefix magrant-box-list ()
  "Transient for listing boxes."
  :man-page "magrant-box-list"
  ["Arguments"
   ("-i" "All" "--box-info")]
  ["Actions"
   ;; NB: `magrant-box-refresh' called by hook, see `magrant-box-mode'
   ;; TODO: when "--box-info" is set, display extended info is separate buffer
   ("l" "List" tablist-revert)])



;; TRANSIENT: ADD

(transient-define-prefix magrant-box-add ()
  "Transient for adding boxes."
  :man-page "magrant-box-add"
  ["Validation arguments"
   ("-c" "Checksum" "--checksum " read-string)
   ("-t" "Checksum Type" "--checksum-type " read-string)]
  [:description "Actions"
                ("N" "Add a new image" magrant-box-add-one)])

(defun magrant-box-add-one (name args)
  "Add the box named NAME.
Additional ARGS are retrieved through transient."
  (interactive (list (magrant-box-read-name)
                     (transient-args transient-current-command)))
  (magrant-run-vagrant-async
   "box add"
   (concat "*vagrant box add - " name "*")
   args
   name))



;; TRANSIENT: REMOVE

(magrant-utils-transient-define-prefix magrant-box-remove ()
  "Transient for removing boxes."
  :man-page "magrant-box-remove"
  ["Filter arguments"
   ("-p" "Provider" "--provider " read-string)
   ("-v" "Version" "--box-version " read-string)
   ("-a" "All versions" "--all")]
  ["Tune arguments"
   ("-f" "Force" "-f")]
  [:description magrant-utils-generic-actions-heading
                ("D" "Remove" magrant-utils-generic-action)])



;; TRANSIENT: UPDATE

(magrant-utils-transient-define-prefix magrant-box-update ()
  "Transient for updating boxes."
  :man-page "magrant-box-update"
  ["Filter arguments"
   ("-p" "Provider" "--provider " read-string)]
  ["Validation arguments"
   ("-i" "Insecure" "--insecure")
   ("-cert" "SSL certificate file" "--cert " read-string)
   ("-caf" "CA certificate file" "--cacert " read-string)
   ("-cad" "CA certificate dir" "--capath " read-string)]
  ["Tune arguments"
   ("-f" "Force" "-f")]
  [:description magrant-utils-generic-actions-heading
                ("U" "Update" magrant-box--update-action)])

(defun magrant-box--update-action ()
  "Update selected boxes."
  (interactive)
  (magrant-utils-generic-action-with-item-prefix "--box "))



;; COMMAND

(defun magrant-boxes ()
  "List vagrant boxes."
  (interactive)
  (magrant-utils-pop-to-buffer "*vagrant-boxes*")
  (magrant-box-mode)
  (tablist-revert))




(provide 'magrant-box)

;;; magrant-box.el ends here
