;;; key-assist-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "key-assist" "key-assist.el" (0 0 0 0))
;;; Generated autoloads from key-assist.el

(autoload 'key-assist "key-assist" "\
Prompt to eval a locally relevant function, with hints and keybindings.
Press TAB to see the hints.

Interactively, the optional arg SPEC is either a regexp string
for candidate commands to match, or a keymap from which to
prepare the hints. If NIL, a regexp is generated based upon the
first word of the buffer's major mode. Results are presented
sorted alphabetically by keybinding length.

Programmatically, optional arg PROMPT can be used to customize
the prompt. For the further programmatic options of SPEC and for
a description of arg NOSORT, see function `key-assist--get-cmds'.

See also variables `key-assist-exclude-regexps' and
`key-assist-exclude-cmds'.

\(fn &optional SPEC PROMPT NOSORT)" t nil)

(register-definition-prefixes "key-assist" '("key-assist-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; key-assist-autoloads.el ends here
