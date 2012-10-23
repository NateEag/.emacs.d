;; This is a sample .dir-locals file for working with projects that use
;; Tabs for indentation.
;; The 'nil' configuration applies to all modes.
((nil . ((indent-tabs-mode . t)
         (tab-width . 4)
         (eval . (highlight-regexp "^ *")))))

;; On rare occasions I might work on a project that uses smart tabs - tabs
;; indicate indentation level, spaces indicate alignment level. In that case,
;; uncomment this stuff.
;; (when (locate-library "smart-tabs-mode")
;;   (autoload 'smart-tabs-mode "smart-tabs-mode"
;;     "Intelligently indent with tabs, align with spaces!")
;;   (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;   (autoload 'smart-tabs-advice "smart-tabs-mode")
;;   (smart-tabs-mode-enable))
