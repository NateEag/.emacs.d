;;; poly-argocd-template-mode.el ---

;;; Author:

;;; Version:

;;; Commentary:

;; A quick dumb hack to get reasonable highlighting within ArgoCD template
;; files.
;;
;; I touch them occasionally in my current $DAYJOB, so I wanted something that
;; gave me reasonable syntax coloration.

;;; Code:

;; FIXME Don't use require. Get autoloading to work somehow.
(require 'polymode)

;; FIXME Replace this copy/paste with a dependency on poly-ansible-mode. This
;; was just a quick test that, shockingly, seems to've panned out.
;;
;; ...if you want to be technical, these [actually
;; use](https://argo-cd.readthedocs.io/en/stable/operator-manual/notifications/templates/)
;; golang's html/template package, but the syntax appears to be close enough to
;; jinja2's to be fine for now.
(defcustom pm-inner/jinja2
  (pm-inner-chunkmode :mode #'jinja2-mode
                      :head-matcher "{[%{#][+-]?"
                      :tail-matcher "[+-]?[%}#]}"
                      :head-mode 'body
                      :head-adjust-face nil)
  "Jinja2 chunk."
  :group 'innermodes
  :type 'object)

(define-polymode poly-argocd-template-mode
  :hostmode 'pm-host/yaml
  :innermodes '(pm-inner/jinja2))

(provide 'poly-argocd-template-mode)
;;; poly-argocd-template-mode.el ends here
