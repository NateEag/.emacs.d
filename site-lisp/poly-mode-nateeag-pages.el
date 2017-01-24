;;; poly-mode-nateeag-pages.el --- polymode for nateeag.com page definitions

;;; Author: Nate Eagleso

;;; Version: 0.0.1

;;; Commentary:

;; Just testing whether it's brain-dead easy to define a polymode that will
;; work nicely for nateeag.com pages.

;; Answer so far: no. What I have below does not work.

;;; Code:

(defcustom pm-host/yaml
  (pm-bchunkmode "yaml" :mode 'yaml-mode)
  "yaml host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-inner/markdown-nateeag
  (pm-hbtchunkmode "markdown-nateeag"
                   :head-reg "body: |"
                   :tail-reg "\\'")
  "Markdown hunk in nateeag.com page documents."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/nateeag-page
  (pm-polymode-one "nateeag-page"
                   :hostmode 'pm-host/yaml
                   :innermode 'pm-inner/markdown-nateeag)
  "Test polymode for editing nateeag.com pages."
  :group 'polymodes
  :type 'object)

(define-polymode poly-nateeag-page-mode pm-poly/nateeag-page)

(provide 'poly-mode-nateeag-pages)
;;; poly-mode-nateeag-pages.el ends here
