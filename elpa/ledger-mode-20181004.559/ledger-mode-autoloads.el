;;; ledger-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ledger-flymake" "ledger-flymake.el" (23486
;;;;;;  17763 0 0))
;;; Generated autoloads from ledger-flymake.el

(autoload 'ledger-flymake-enable "ledger-flymake" "\
Enable `flymake-mode' in `ledger-mode' buffers.

Don't enable flymake if flycheck is on and flycheck-ledger is
available.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ledger-mode" "ledger-mode.el" (23486 17763
;;;;;;  0 0))
;;; Generated autoloads from ledger-mode.el

(autoload 'ledger-mode "ledger-mode" "\
A mode for editing ledger data files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

;;;***

;;;### (autoloads nil nil ("ledger-check.el" "ledger-commodities.el"
;;;;;;  "ledger-complete.el" "ledger-context.el" "ledger-exec.el"
;;;;;;  "ledger-fontify.el" "ledger-fonts.el" "ledger-init.el" "ledger-mode-pkg.el"
;;;;;;  "ledger-navigate.el" "ledger-occur.el" "ledger-post.el" "ledger-reconcile.el"
;;;;;;  "ledger-regex.el" "ledger-report.el" "ledger-schedule.el"
;;;;;;  "ledger-sort.el" "ledger-state.el" "ledger-test.el" "ledger-texi.el"
;;;;;;  "ledger-xact.el") (23486 17763 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ledger-mode-autoloads.el ends here
