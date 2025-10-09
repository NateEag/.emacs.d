;;; csproj-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "csproj-mode" "csproj-mode.el" (0 0 0 0))
;;; Generated autoloads from csproj-mode.el

(autoload 'csproj-mode-dotnet-new "csproj-mode" "\
Invoke 'dotnet new' with the given TEMPLATE-NAME.

\(fn TEMPLATE-NAME)" t nil)

(autoload 'csproj-mode "csproj-mode" "\
A major mode for editing csproj and other msbuild-style project files

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.[^.]*proj\\'" . csproj-mode))

(register-definition-prefixes "csproj-mode" '("csproj-mode--get-dotnet-new-templates"))

;;;***

;;;### (autoloads nil nil ("csproj-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; csproj-mode-autoloads.el ends here
