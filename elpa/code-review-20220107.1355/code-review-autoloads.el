;;; code-review-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "code-review" "code-review.el" (0 0 0 0))
;;; Generated autoloads from code-review.el

(autoload 'code-review-forge-pr-at-point "code-review" "\
Review the forge pull request at point.
OUTDATED." t nil)

(autoload 'code-review-start "code-review" "\
Start review given PR URL.

\(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-actions" "code-review-actions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-actions.el

(autoload 'code-review--submit "code-review-actions" "\
Submit your review with a final verdict (EVENT).
If you already have a FEEDBACK string use it.
If you want only to submit replies, use ONLY-REPLY? as non-nil.

\(fn EVENT &optional FEEDBACK ONLY-REPLY\\=\\?)" t nil)

(autoload 'code-review-submit-approve "code-review-actions" "\
Approve current PR.
Optionally set a FEEDBACK message.

\(fn &optional FEEDBACK)" t nil)

(autoload 'code-review-submit-comments "code-review-actions" "\
Submit a Review Comment for the current PR." t nil)

(autoload 'code-review-submit-request-changes "code-review-actions" "\
Submit a Request Change for the current PR." t nil)

(autoload 'code-review-submit-lgtm "code-review-actions" "\
Submit an Approve Review with a LGTM message." t nil)

(autoload 'code-review-submit-only-replies "code-review-actions" "\
Submit only replies comments." t nil)

(autoload 'code-review-submit-single-top-level-comment "code-review-actions" "\
Submit a single comment without an attached Review." t nil)

(autoload 'code-review-submit-single-diff-comment-at-point "code-review-actions" "\
Submit a single diff comment without an attached Review." t nil)

(autoload 'code-review-save-unfinished-review "code-review-actions" "\
Save unfinished Review." t nil)

(autoload 'code-review-recover-unfinished-review "code-review-actions" "\
Recover unfinished Review for the given URL.

\(fn URL)" t nil)

(autoload 'code-review-open-unfinished-review "code-review-actions" "\
Choose an unfinished Review from the previous unfinished list." t nil)

(autoload 'code-review-merge-merge "code-review-actions" "\
Merge PR with MERGE strategy." t nil)

(autoload 'code-review-merge-rebase "code-review-actions" "\
Merge PR with REBASE strategy." t nil)

(autoload 'code-review-merge-squash "code-review-actions" "\
Merge PR with SQUASH strategy." t nil)

(autoload 'code-review-set-feedback "code-review-actions" "\
Add review FEEDBACK locally.  Required to Comment and Request Change reviews." t nil)

(autoload 'code-review-set-title "code-review-actions" "\
Change the title of current PR.  Sent immediately." t nil)

(autoload 'code-review-set-label "code-review-actions" "\
Change the labels of current PR.  Sent immediately.
Rewrite all current labels with the options chosen here." t nil)

(autoload 'code-review-set-description "code-review-actions" "\
Submit new PR description.  Sent immediately." t nil)

(autoload 'code-review-delete-feedback "code-review-actions" "\
Delete review FEEDBACK locally." t nil)

(autoload 'code-review-reload "code-review-actions" "\
Reload the buffer.  All your local comments will be lost." t nil)

(autoload 'code-review-promote-comment-at-point-to-new-issue "code-review-actions" "\
Promote comment at point to a new issue.  Sent immediately." t nil)

(autoload 'code-review-request-reviews "code-review-actions" "\
Request reviewers for current PR using LOGIN if available.

\(fn &optional LOGIN)" t nil)

(autoload 'code-review-request-review-at-point "code-review-actions" "\
Request reviewer at point.

\(fn &rest _)" t nil)

(autoload 'code-review-toggle-display-all-comments "code-review-actions" "\
Toggle display comments." t nil)

(autoload 'code-review-toggle-display-top-level-comments "code-review-actions" "\
Toggle display the top level comments." t nil)

(autoload 'code-review-toggle-display-diff-comments "code-review-actions" "\
Toggle display the top level comments." t nil)

(autoload 'code-review-comment-code-suggestion "code-review-actions" "\
Add code suggestion box." t nil)

(autoload 'code-review-comment-jump-next "code-review-actions" "\
Go to next comment in the buffer." t nil)

(autoload 'code-review-comment-jump-previous "code-review-actions" "\
Go to previous comment in the buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-actions" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-bitbucket" "code-review-bitbucket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-bitbucket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-bitbucket" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-comment" "code-review-comment.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-comment.el

(autoload 'code-review-comment-add-or-edit "code-review-comment" "\
Add or edit comment depending on context.
Inform if a SUGGESTION-CODE? is being proposed.

\(fn &optional SUGGESTION-CODE\\=\\?)" t nil)

(autoload 'code-review-comment-commit "code-review-comment" "\
Commit comment." t nil)

(autoload 'code-review-input-mention-user-at-point "code-review-comment" "\
Insert @USERNAME at current point to mention an user." t nil)

(autoload 'code-review-comment-quit "code-review-comment" "\
Quit the comment window." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-comment" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-db" "code-review-db.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from code-review-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-db" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-github" "code-review-github.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-github.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-github" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-gitlab" "code-review-gitlab.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-gitlab.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-gitlab" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-parse-hunk" "code-review-parse-hunk.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-parse-hunk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-parse-hunk" '("code-review-parse-")))

;;;***

;;;### (autoloads nil "code-review-section" "code-review-section.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-section.el

(autoload 'code-review-section-delete-comment "code-review-section" "\
Delete a local comment." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-section" '("code-review-")))

;;;***

;;;### (autoloads nil "code-review-utils" "code-review-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from code-review-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "code-review-utils" '("code-review-")))

;;;***

;;;### (autoloads nil nil ("code-review-faces.el" "code-review-interfaces.el"
;;;;;;  "code-review-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; code-review-autoloads.el ends here
