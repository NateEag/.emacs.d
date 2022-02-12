;;; project-shells-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "project-shells" "project-shells.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from project-shells.el

(autoload 'project-shells-activate-for-key "project-shells" "\
Create or switch to the shell buffer for the key, the project
name, and the project root directory.

\(fn KEY &optional PROJ PROJ-ROOT)" nil nil)

(autoload 'project-shells-activate "project-shells" "\
Create or switch to the shell buffer for the key just typed

\(fn P)" t nil)

(autoload 'project-shells-setup "project-shells" "\
Configure the project shells with the prefix keymap and the
setup, for format of setup, please refer to document of
project-shells-setup.

\(fn MAP &optional SETUP)" nil nil)

(autoload 'project-shells-mode "project-shells" "\
Toggle Project-Shells mode on or off.

If called interactively, enable Project-Shells mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{project-shells-mode-map}

\(fn &optional ARG)" t nil)

(put 'global-project-shells-mode 'globalized-minor-mode t)

(defvar global-project-shells-mode nil "\
Non-nil if Global Project-Shells mode is enabled.
See the `global-project-shells-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-project-shells-mode'.")

(custom-autoload 'global-project-shells-mode "project-shells" nil)

(autoload 'global-project-shells-mode "project-shells" "\
Toggle Project-Shells mode in all buffers.
With prefix ARG, enable Global Project-Shells mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Project-Shells mode is enabled in all buffers where
`project-shells-mode' would do it.
See `project-shells-mode' for more information on Project-Shells mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project-shells" '("project-shells-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; project-shells-autoloads.el ends here
