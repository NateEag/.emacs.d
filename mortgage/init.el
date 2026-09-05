;;; init.el ---

;;; Author: Nate Eagleson

;;; Version: 0.0.0

;;; Commentary:

;; I hope this version will be an improvement on what has gone before, while
;; preserving what I have grown used to.

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(require 'elpaca-init)
(require 'package-conf)
(require 'prefs)

(setq nateeag-elapsed-start-time nil)

;; TODO: Write an actual test that verifies startup time
;;
;; Uncomment the following to verify we log the situation.
;;(sleep-for 2)
(add-hook 'after-init-hook
          (lambda ()
            (setq nateeag-elapsed-start-time
                  (float-time (time-subtract after-init-time before-init-time)))))

;; Now that we're done loading, don't trigger debugging on errors.
(setq debug-on-error nil)
;;; init.el ends here
