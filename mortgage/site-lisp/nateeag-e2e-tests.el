;;; nateeag-e2e-tests.el --- E2E tests for my core Emacs config behaviors.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; This is an experiment.
;;
;; I know I want some E2E tests covering the core behaviors I rely on, but I'm
;; not sure if ERT will be sufficient to the job.
;;
;; I may wind up trying some in emacs-director, too, as it's more explicitly
;; aimed at writing full-blown E2E tests, as I understand it.
;;
;; I want to give the built-in solution a fair shake first, though.

;;; Code:

;; The ert-play-keys function is in this package.
(require 'ert-x)

(ert-deftest startup-time-test ()
  (should (< nateeag-elapsed-start-time 0.5)))

;; FIXME: This currently fails.
;;
;; Commented until I get to a working configuration I want to actually write
;; the test for.
;;
;; This was enough to demonstrate that ert-play-keys is working.
;;(ert-deftest open-project-test () (ert-play-keys "SPC f ~/.emacs.d/readme.md
;;  C-j"))

(defun nateeag-run-e2e-tests ()
  "Run E2E tests, exiting with nonzero status if any fail.

Exits Emacs with status zero if all tests have expected results.

Otherwise, dumps an XML test report to disk and exits Emacs with status
equal to number of unsuccessful tests."

  (let ((stats (ert-run-tests-interactively t)))
    (ert-write-junit-test-report stats)
    (kill-emacs (ert-stats-completed-unexpected stats))
    ))

(provide 'nateeag-e2e-tests)
;;; nateeag-e2e-tests.el ends here
