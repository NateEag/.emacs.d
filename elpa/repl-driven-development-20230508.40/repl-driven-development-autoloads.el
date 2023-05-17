;;; repl-driven-development-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "repl-driven-development" "repl-driven-development.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from repl-driven-development.el

(autoload 'repl-driven-development "repl-driven-development" "\
Make Emacs itself a REPL for your given language of choice.

Suppose you're exploring a Python/Ruby/Java/JS/TS/Haskell/Lisps/etc
API, or experimenting with an idea and want immediate feedback.
You could open a terminal and try things out there; with no editor
support, and occasionally copy-pasting things back into your editor
for future use.  Better yet, why not use your editor itself as a REPL.

Implementation & behavioural notes can be found in the JavaScript
Example below.

######################################################################
### JavaScript Example ---Basic usage, and a minimal server ##########
######################################################################

   ;; C­x C­j now evaluates arbitrary JavaScript code
   (repl-driven-development [C­x C­j] \"node -i\")

That's it! Press “C­x C­e” on the above line so that “C­x C­j”
will now evaluate a selection, or the entire line, as if it were
Java code.  ⟦Why C­x C­j? C­x C­“e” for Emacs Lisp code, and C­x
C­“j” for JavaScript code!⟧ For instance, copy-paste the
following examples into a JS file ---or just press “C­x C­j” to
evaluate them!

    1 + 2                                     // ⮕ 3
    1 + '2'                                   // ⮕ '12'
    let me = {name: 'Jasim'}; Object.keys(me) // ⮕ ['name']
    me.doesNotExist('whoops')                 // ⮕ Uncaught TypeError

All of these results are echoed inline in an overlay, by default.
Moreover, there is a *REPL* buffer created for your REPL so you
can see everything you've sent to it, and the output it sent
back.  This is particularly useful for lengthy error messages,
such as those of Java, which cannot be rendered nicely within an
overlay.

How this works is that Emacs spawns a new “node -i” process, then
C­x C­j sends text to that process.  Whenever the process emits
any output ---on stdout or stderr--- then we emit that to the
user via an overlay starting with “⮕”.

Finally, “C­h k  C­x C­j” will show you the name of the function
that is invoked when you press C­x C­j, along with minimal docs.

A useful example would be a minimal server, and requests for it.

   // First get stuff with C­x C­e:
   // (async-shell-command \"npm install -g express axios\")

   let app = require('express')()
   let clicked = 1
   app.get('/hi', (req, res) => res.send(`Hello World × ${clicked++}`))

   let server = app.listen(3000)
   // Now visit   http://localhost:3000/hi   a bunch of times!

  // Better yet, see the output programmatically...
  let axios = require('axios')
  // Press C­x C­j a bunch of times on the following expression ♥‿♥
  console.log((await axios.get('http://localhost:3000/hi')).data)

  // Consider closing the server when you're done with it.
  server.close()

Just as “Emacs is a Lisp Machine”, one can use “VSCodeJS” to use
“VSCode as a JS Machine”.
See http://alhassy.com/vscode-is-itself-a-javascript-repl.

######################################################################
### Description of Arguments #########################################
######################################################################

- KEYS: A vector such as [C­x C­p] that declares the keybindings for
  the new REPL evaluator.

- CLI: A string denoting the terminal command to start your repl;
  you may need an “-i” flag to force it to be interactive even though
  we use it from a child process rather than a top-level shell.

- PROMPT: What is the prompt that your REPL shows, e.g., “>”.
  We try to ignore showing it in an overlay that would otherwise hide
  useful output.

### Misc Remarks #####################################################
VSCode has a similar utility for making in-editor REPLs, by the
same author: http://alhassy.com/making-vscode-itself-a-java-repl

\(fn KEYS CLI &key (PROMPT \">\"))" nil nil)

(register-definition-prefixes "repl-driven-development" '("repl-driven-development-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; repl-driven-development-autoloads.el ends here
