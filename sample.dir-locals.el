;; This is a sample .dir-locals file so I can keep track of values I need to
;; use sometimes.

;; The 'nil' configuration applies to all modes.
;; DEBUG I originally had '(eval . (highlight-regexp "^ *"))' as the third
;; line of my nil config, but it caused memory exhaustion errors on my work
;; setup. I think it has to do with the hack I'm using to load .dir-locals.el
;; over TRAMP/plink, but until I have it solved, I'm removing it.
((nil .  (
          ;; Set a variable for the directory holding this .dir-locals.el file.
          ;; Useful for setting project-specific path variables.
          (eval . (set (make-local-variable 'nateeag/dir-locals-dir)
                       (file-name-directory
                        (let ((d (dir-locals-find-file ".")))
                          (if (stringp d) d (car d))))))
          )
      ))

;; SEMI-DEPRECATED I haven't had to work on anything that prefers tabs in a
;; while. I'd have to integrate this
;; with my newer import of guess-style.el
;; On rare occasions I might work on a project that uses smart tabs - tabs
;; indicate indentation level, spaces indicate alignment level. In that case,
;; uncomment this stuff.
;; (when (locate-library "smart-tabs-mode")
;;   (autoload 'smart-tabs-mode "smart-tabs-mode"
;;     "Intelligently indent with tabs, align with spaces!")
;;   (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;   (autoload 'smart-tabs-advice "smart-tabs-mode")
;;   (smart-tabs-mode-enable))
