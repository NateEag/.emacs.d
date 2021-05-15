;;; symex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "symex" "symex.el" (0 0 0 0))
;;; Generated autoloads from symex.el

(autoload 'symex-mode "symex" "\
An evil way to edit Lisp symbolic expressions as trees.

If called interactively, enable Symex mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'symex-initialize "symex" "\
Initialize symex mode.

This registers symex mode for use in all recognized Lisp modes, and also
advises functions to enable or disable features based on user configuration." nil nil)

(autoload 'symex-mode-interface "symex" "\
The main entry point for editing symbolic expressions using symex mode.

Enter the symex evil state and show a hydra menu for accessing various
features." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex" '("symex-")))

;;;***

;;;### (autoloads nil "symex-computations" "symex-computations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-computations.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-computations" '("symex-")))

;;;***

;;;### (autoloads nil "symex-custom" "symex-custom.el" (0 0 0 0))
;;; Generated autoloads from symex-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-custom" '("symex-")))

;;;***

;;;### (autoloads nil "symex-data" "symex-data.el" (0 0 0 0))
;;; Generated autoloads from symex-data.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-data" '("symex-")))

;;;***

;;;### (autoloads nil "symex-dsl" "symex-dsl.el" (0 0 0 0))
;;; Generated autoloads from symex-dsl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-dsl" '("deftraversal" "symex-")))

;;;***

;;;### (autoloads nil "symex-evaluator" "symex-evaluator.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from symex-evaluator.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-evaluator" '("symex-")))

;;;***

;;;### (autoloads nil "symex-evil" "symex-evil.el" (0 0 0 0))
;;; Generated autoloads from symex-evil.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-evil" '("symex-")))

;;;***

;;;### (autoloads nil "symex-evil-support" "symex-evil-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-evil-support.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-evil-support" '("symex--define-evil-key")))

;;;***

;;;### (autoloads nil "symex-hydra" "symex-hydra.el" (0 0 0 0))
;;; Generated autoloads from symex-hydra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-hydra" '("hydra-symex" "symex-")))

;;;***

;;;### (autoloads nil "symex-interface-arc" "symex-interface-arc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-arc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-arc" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interface-clojure" "symex-interface-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-clojure" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interface-common-lisp" "symex-interface-common-lisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-common-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-common-lisp" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interface-elisp" "symex-interface-elisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-elisp" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interface-racket" "symex-interface-racket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-racket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-racket" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interface-scheme" "symex-interface-scheme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-interface-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interface-scheme" '("symex-")))

;;;***

;;;### (autoloads nil "symex-interop" "symex-interop.el" (0 0 0 0))
;;; Generated autoloads from symex-interop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-interop" '("symex-")))

;;;***

;;;### (autoloads nil "symex-misc" "symex-misc.el" (0 0 0 0))
;;; Generated autoloads from symex-misc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-misc" '("symex-")))

;;;***

;;;### (autoloads nil "symex-primitives" "symex-primitives.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from symex-primitives.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-primitives" '("symex-")))

;;;***

;;;### (autoloads nil "symex-transformations" "symex-transformations.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from symex-transformations.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-transformations" '("symex-")))

;;;***

;;;### (autoloads nil "symex-traversals" "symex-traversals.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from symex-traversals.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-traversals" '("symex-")))

;;;***

;;;### (autoloads nil "symex-ui" "symex-ui.el" (0 0 0 0))
;;; Generated autoloads from symex-ui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-ui" '("symex--toggle-highlight")))

;;;***

;;;### (autoloads nil "symex-utils" "symex-utils.el" (0 0 0 0))
;;; Generated autoloads from symex-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symex-utils" '("symex--")))

;;;***

;;;### (autoloads nil nil ("symex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; symex-autoloads.el ends here
