;;; config-packages.el --- configure my Emacs packages.

;;; Author: Nate Eagleson

;;; Version: 0.0.1

;;; Commentary:

;; Just a big collection of use-package invocations.

;;; Code:

(require 'use-package)

(use-package uniquify
             :init
             (setq
              uniquify-buffer-name-style 'post-forward
              uniquify-separator ":"
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"))

;;; config-packages.el ends here
