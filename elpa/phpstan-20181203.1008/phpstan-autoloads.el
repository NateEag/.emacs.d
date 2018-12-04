;;; phpstan-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "phpstan" "phpstan.el" (23558 35263 0 0))
;;; Generated autoloads from phpstan.el

(defvar phpstan-working-dir nil "\
Path to working directory of PHPStan.

*NOTICE*: This is different from the project root.

STRING
     Absolute path to `phpstan' working directory.

`(root . STRING)'
     Relative path to `phpstan' working directory from project root directory.

NIL
     Use (php-project-get-root-dir) as working directory.")

(make-variable-buffer-local 'phpstan-working-dir)

(put 'phpstan-working-dir 'safe-local-variable #'(lambda (v) (if (consp v) (and (eq 'root (car v)) (stringp (cdr v))) (null v) (stringp v))))

(defvar phpstan-config-file nil "\
Path to project specific configuration file of PHPStan.

STRING
     Absolute path to `phpstan' configuration file.

`(root . STRING)'
     Relative path to `phpstan' configuration file from project root directory.

NIL
     Search phpstan.neon(.dist) in (phpstan-get-working-dir).")

(make-variable-buffer-local 'phpstan-config-file)

(put 'phpstan-config-file 'safe-local-variable #'(lambda (v) (if (consp v) (and (eq 'root (car v)) (stringp (cdr v))) (null v) (stringp v))))

(defvar phpstan-level "0" "\
Rule level of PHPStan.

INTEGER or STRING
     Number of PHPStan rule level.

max
     The highest of PHPStan rule level.")

(make-variable-buffer-local 'phpstan-level)

(put 'phpstan-level 'safe-local-variable #'(lambda (v) (or (null v) (integerp v) (eq 'max v) (and (stringp v) (string= "max" v) (string-match-p "\\`[0-9]\\'" v)))))

(defvar phpstan-replace-path-prefix)

(make-variable-buffer-local 'phpstan-replace-path-prefix)

(put 'phpstan-replace-path-prefix 'safe-local-variable #'(lambda (v) (or (null v) (stringp v))))

(defvar phpstan-executable nil "\
PHPStan excutable file.

STRING
     Absolute path to `phpstan' executable file.

`docker'
     Use Docker using phpstan/docker-image.

`(root . STRING)'
     Relative path to `phpstan' executable file.

`(STRING . (ARGUMENTS ...))'
     Command name and arguments.

NIL
     Auto detect `phpstan' executable file.")

(make-variable-buffer-local 'phpstan-executable)

(put 'phpstan-executable 'safe-local-variable #'(lambda (v) (if (consp v) (or (and (eq 'root (car v)) (stringp (cdr v))) (and (stringp (car v)) (listp (cdr v)))) (or (eq 'docker v) (null v) (stringp v)))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; phpstan-autoloads.el ends here
