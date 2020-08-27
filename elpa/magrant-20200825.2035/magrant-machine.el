;;; magrant-machine.el --- Emacs interface to Vagrant commands on machines  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Jordan Besly

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This sub-module to magrant.el contains all the machine related functions.
;;
;; A "machine", in Vagrant lingo, is an instance of a VM/container spawned from a "box" (i.e. a template).


;;; Code:



;; REQUIRES

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'tablist)
(require 'transient)

(require 'magrant-core)
(require 'magrant-utils)
(require 'magrant-faces)



;; VARS

(defgroup magrant-machine nil
  "vagrant machines customization group."
  :group 'magrant)

(defcustom magrant-machine-default-sort-key '("Id" . nil)
  "Sort key for vagrant machine.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'magrant-machine
  :type '(cons (choice (const "Id")
                       (const "Name")
                       (const "Provider")
                       (const "State")
                       (const "Directory"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom magrant-machine-force-all-status-refresh-on-startup nil
  "If t, will force Vagrant to fetch all machines statuses, one by one.

This prevents Vagrant from displaying outdated information but
makes initial display of machines list slow."
  :group 'magrant-machine
  :type 'boolean)



;; CLI CALLS

;; TODO: use --prune if buffer already exists

(defun magrant-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (magrant-machine-entries)))

(defun magrant-machine-force-all-status-refresh (&optional tablist)
  "Force the refresh of all machines statuses.
This is needed for first retrieval as \"vagrant global status\" often reports outdated statuses.
This forces Vagrant to refresh its cache for each machine."
  (let ((tablist (or tablist (magrant-machine-entries))))
    (--each tablist
      (let* ((dir (aref (cadr it) 4))
             (alias (file-name-nondirectory dir))
             (id (magrant-utils-tablist-entry-id it))
             (default-directory dir))
        (magrant-run-vagrant "status")))))

(defun magrant-machine-entries ()
  "Return the vagrant machines data for `tabulated-list-entries'."
  (let* ((data (magrant-run-vagrant "global-status"))
         (lines (s-split "\n" data t)))
    (cl-loop with i = 0
             for l in lines

             until (s-equals? (s-trim l) "")

             unless (< i 2)
             collect (magrant-machine-parse l)

             do (setq i (+ i 1)))))

(defun magrant-machine-parse (line)
  "Convert a LINE from \"vagrant global-status\" to a `tabulated-list-entries' entry."
  (let* ((tab-line (apply #'vector
                          (-remove 's-blank?
                                   (s-split " " line))))
         (state (aref tab-line 3)))
    (aset tab-line 3 (propertize state 'font-lock-face (magrant-machine-state-face state)))
    (list (aref tab-line 0) tab-line)))

(defun magrant-machine-state-face (state)
  "Return the correct face according to STATE."
  (cond
   ((s-equals? "running" state)
    'magrant-face-status-up)
   ((member state '("poweroff" "aborted"))
    'magrant-face-status-down)
   ((s-equals? "saved" state)
    'magrant-face-status-other)
   (t
    'magrant-face-status-other)))



;; MODE

(defvar magrant-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'magrant-machine-help)

    (define-key map "c" #'magrant-machine-config)

    (define-key map "p" #'magrant-machine-provision)
    (define-key map "s" #'magrant-machine-up)
    (define-key map "H" #'magrant-machine-halt)
    (define-key map "S" #'magrant-machine-suspend)

    (define-key map "b" #'magrant-machine-ssh)

    (define-key map "D" #'magrant-machine-destroy)
    (define-key map "l" #'magrant-machine-list)
    map)
  "Keymap for `magrant-machine-mode'.")

(define-derived-mode magrant-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of vagrant machines."
  (setq tabulated-list-format [("Id" 8 t)("Name" 15 t)("Provider" 15 t)
                               ("State" 10 t)
                               ("Directory" 30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key magrant-machine-default-sort-key)
  (add-hook 'tabulated-list-revert-hook #'magrant-machine-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))



;; TRANSIENT: ENTRY POINT

(transient-define-prefix magrant-machine-help ()
  "Help transient for vagrant machines."
  ["Vagrant machines help"
   ("c" "Config" magrant-machine-config)

   ("p" "Provision" magrant-machine-provision)

   ("s" "Up (start)" magrant-machine-up)
   ("H" "Halt" magrant-machine-halt)
   ("S" "Suspend" magrant-machine-suspend)

   ("b" "SSH" magrant-machine-ssh)
   ("D" "Destroy" magrant-machine-destroy)
   ("l" "List" magrant-machine-list)])

(transient-define-prefix magrant-machine-list ()
  "Transient for listing machines."
  :man-page "magrant-machine-list"
  ["Tune arguments"
   ("-f" "Force" "-f")]
  ["Actions"
   ;; NB: `magrant-machine-refresh' called by hook, see `magrant-machine-mode'
   ("l" "List" magrant-machine-list-impl)])

(defun magrant-machine-list-impl ()
  (interactive)
  (when (member "-f" (transient-args transient-current-command))
    (magrant-machine-force-all-status-refresh tabulated-list-entries))
  (tablist-revert))



;; TRANSIENT: CONFIG

(magrant-utils-transient-define-prefix magrant-machine-config ()
  "Transient related to machine confs."
  :man-page "magrant-box-config"
  ["Filter arguments"
   ("-p" "Ignore Provider" "--ignore-provider")]
  [:description magrant-utils-generic-actions-heading
                ("v" "Validate" magrant-machine--validate-config-action)
                ("f" "Visit" magrant-machine--visit-config-action)])

(defun magrant-machine--validate-config-action ()
  "Validate selected machines."
  (interactive)
  (magrant-machine-generic-action-async "validate" nil))

(defun magrant-machine--visit-config-action ()
  "Visit selected machines config."
  (interactive)
  (--each (tablist-get-marked-items)
    (let ((dir (magrant-machine-tablist-entry-dir it)))
      (find-file (concat dir "/Vagrantfile")))))



;; TRANSIENT: PROVISION

(magrant-utils-transient-define-prefix magrant-machine-provision ()
  "Transient for provisioning machines."
  :man-page "magrant-machine-provision"
  [:description magrant-utils-generic-actions-heading
                ("p" "ALL" magrant-machine-generic-action-async)])



;; TRANSIENT: START / STOP

(magrant-utils-transient-define-prefix magrant-machine-up ()
  "Transient for starting machines."
  :man-page "magrant-machine-up"
  [:description magrant-utils-generic-actions-heading
                ("s" "Start" magrant-machine-generic-action-async)])

(magrant-utils-transient-define-prefix magrant-machine-halt ()
  "Transient for halting machines."
  :man-page "magrant-machine-halt"
  ["Tune arguments"
   ("-f" "Force" "-f")]
  [:description magrant-utils-generic-actions-heading
                ("H" "Halt" magrant-machine-generic-action-async)])

(magrant-utils-transient-define-prefix magrant-machine-suspend ()
  "Transient for suspending machines."
  :man-page "magrant-machine-suspend"
  [:description magrant-utils-generic-actions-heading
                ("S" "Suspend" magrant-machine-generic-action-async)])



;; TRANSIENT: CONNECT

;; TODO: add option to use vagrant-tramp, or maybe even detect IP in Vagrantfile

(magrant-utils-transient-define-prefix magrant-machine-ssh ()
  "Transient for SSH'ing into machines."
  :man-page "magrant-machine-ssh"
  [:description magrant-utils-generic-actions-heading
                ("b" "SSH" magrant-machine-generic-action-async)])



;; TRANSIENT: DESTROY

(magrant-utils-transient-define-prefix magrant-machine-destroy ()
  "Transient for destroying machines."
  :man-page "magrant-machine-destroy"
  ["Tune arguments"
   ("-f" "Force" "-f")
   ("-p" "Parallel" "--parallel")]
  [:description magrant-utils-generic-actions-heading
                ("D" "Destroy" magrant-machine-generic-action-async)])



;; TRANSIENT: HELPERS

(defun magrant-machine-tablist-entry-dir (entry)
  "Get the \"Directory\" of a tablist ENTRY."
  (aref (cdr entry) 4))

(defun magrant-machine-get-transient-action ()
  "Get the vagrant action from the transient name.
Inspired by `magrant-utils-get-transient-action'."
  (s-chop-prefix "magrant-machine-" (symbol-name transient-current-command)))

(defun magrant-machine-generic-action-async (action args)
  "Perform ACTION on selected machines with additional transient ARGS.

Same as `magrant-utils-generic-action' with an additional cd to
the box dir, needed for vagrant machine actions to know their
target.
Additionally, the call is non-blocking and outputs to a newly
created buffer (for each target)."
  (interactive (list (magrant-machine-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (tablist-get-marked-items)
    (let* ((dir (magrant-machine-tablist-entry-dir it))
           (alias (file-name-nondirectory dir))
           (id (magrant-utils-tablist-entry-id it))
           (default-directory dir))
      (magrant-run-vagrant-async action
                                 (concat
                                  "*vagrant " action " | " alias "(" id ")*")
                                 args)))
  (tablist-revert))



;; COMMAND

(defun magrant-machines ()
  "List vagrant machines."
  (interactive)
  (magrant-utils-pop-to-buffer "*vagrant-machines*")
  (magrant-machine-mode)
  (when magrant-machine-force-all-status-refresh-on-startup
    (magrant-machine-force-all-status-refresh))
  (tablist-revert))




(provide 'magrant-machine)

;;; magrant-machine.el ends here
