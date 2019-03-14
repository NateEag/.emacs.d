;;; github-review-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "github-review" "github-review.el" (23690 17705
;;;;;;  739165 890000))
;;; Generated autoloads from github-review.el

(autoload 'github-review-start "github-review" "\
Start review given PR URL.

\(fn URL)" t nil)

(autoload 'github-review-approve "github-review" "\
Approve a PR (to be run from a buffer corresponding to a review).

\(fn)" t nil)

(autoload 'github-review-reject "github-review" "\
Reject a PR (to be run from a buffer corresponding to a review).

\(fn)" t nil)

(autoload 'github-review-comment "github-review" "\
Comment on a PR (to be run from a buffer corresponding to a review).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; github-review-autoloads.el ends here
