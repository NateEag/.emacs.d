;;; poly-ts-graphql-mode.el ---

;;; Author:

;;; Version:

;;; Commentary:

;;

;;; Code:

;; FIXME Don't use require. Get autoloading to work somehow.
(require 'polymode)

;; Quick hack at attempting to use graphql-mode in Apollo's embedded
;; graphql-in-typescript setup that I use at present $DAYJOB.
(defcustom pm-inner/graphql-ts
  (pm-inner-chunkmode :mode #'graphql-mode
                      :head-matcher "graphql(`"
                      :tail-matcher "`);"
                      :head-mode 'body
                      :head-adjust-face nil)
  "GraphQL-in-TypeScript chunk."
  :group 'innermodes
  :type 'object)

(define-polymode poly-ts-graphql-mode
  :hostmode 'pm-host/typescript
  :innermodes '(pm-inner/graphql-ts))

(provide 'poly-ts-graphql-mode)
;;; poly-ts-graphql-mode.el ends here
