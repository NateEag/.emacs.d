============================
Nate Eagleson's Emacs Config
============================

Ever since [Dr. Bui](http://cs.hbg.psu.edu/~bui/) told our C++ programming
class to use it for programming in the Sun lab, Emacs has been my editor.

On that first encounter, I was put off by its user interface, which is
obscurantist at best. I discovered that the default install is incredibly
heavyweight fairly quickly, and I was not impressed with how little it believed
in graphical environments.

After a naive search for the perfect editor in my early twenties (I narrowly
avoided wasting years writing one), in 2009 I decided to learn Emacs well,
since I knew a lot of its shortcuts and had decided to stick with an editor.

A year or two into the project, I realized my dream editor had been sitting in
my lap all along, if only I had tried to learn it.


Why Emacs (or: Hokey Religions and Ancient Weapons)
===================================================

Like all art forms, programming has technique.

A musician's technique is how he makes the instrument produce sound.
A painter's technique is how she puts paint on the canvas.
A programmer's technique is how he gives the computer instructions.

Despite the grumbling from the graphical language crowd, most programming comes
down to entering, reading, and changing plain text in files.

Thus, a programmer should manipulate text fluidly and effortlessly, the way a
pianist plays arpeggios or a painter wields a brush.

Changing editors for each language complicates technique. Eclipse for Java,
PyCharm for Python, Sublime for JavaScript... The keystrokes for editing a
program are different in each of these, and over a lifetime adds cognitive
burden.

Instead of changing editors for each language, a programmer's editor should
adapt itself to each language, so that the technique of programming remains
unchanged.

In the same way, the programmer should not adapt herself to the editor -
the editor should adapt to her.

For these purposes, Emacs reigns supreme.

It has been honed over decades to a razor-sharp edge, it runs almost
everywhere, and it can be rewritten without restarting it.

My Setup
========

Emacs' default keybindings are powerful but verbose. They're hard to type and
are likely why
`RMS has had wrist problems <https://stallman.org/stallman-computing.html>`__.
I use `evil-mode <https://gitorious.org/evil/pages/Home>`__ for vim-style modal
editing, which is more powerful and easier on my hands.
`evil-leader <https://github.com/cofi/evil-leader>`__,
`evil-commentary <https://github.com/linktohack/evil-commentary>`__,
`evil-surround <https://github.com/timcharper/evil-surround>`__, and
`ace-jump-mode <https://github.com/winterTTr/ace-jump-mode>`__ make it even better.

I'm slowly moving to `use-package <https://github.com/jwiegley/use-package>`__
for configuring and loading packages on-demand. It's a huge improvement over my
accumulated piles of ad-hoc code.

I use `flycheck <https://github.com/flycheck/flycheck>`__ for style-checking
just about everything, and install the relevant checkers whenever I start
working with a new language.

I use `smartparens <https://github.com/Fuco1/smartparens>`__ to insert paired
characters (:code:`()`, :code:`[]`, :code:`{}`, etc). It also has powerful
navigation features I haven't really learned to use.

I use `yasnippet <http://capitaomorte.github.io/yasnippet/>`__ to insert code
snippets, so I don't have to type as much in verbose languages. I mostly use it
for language constructs, since anything more is usually bad for codebase health
(one-time code generation makes for WET code, and fixes in the generator
rarely make it to previously-generated code).

I use `Solarized <https://github.com/bbatsov/solarized-emacs>`__ as my color
theme and flip between light and dark variants based on lighting conditions.

I use `undo-tree <http://www.dr-qubit.org/emacs.php#undo-tree>`__ to make
Emacs' infinite-undo feature more usable. I have occasionally been bitten by a
bug that loses part of undo history, but have not been able to reproduce it
consistently - it may relate to `undo in region
<https://lists.gnu.org/archive/html/bug-gnu-emacs/2014-01/msg01106.html>`__.

I use `helm <http://emacs-helm.github.io/helm/>`__ to find files and commands
quickly and easily. `helm-swoop
<https://github.com/ShingoFukuyama/helm-swoop>`__ is pretty neat too.

I use `backup-walker <https://github.com/lewang/backup-walker>`__ to search
through a file's backups when I need an older version I didn't commit. I store
all backups in `autosaves/` and back up on every save, even in version control
repositories. I have not lost a line of code since setting this up.

I have a decent Python setup, with `Jedi
<http://jedi.jedidjah.ch/en/latest/>`__ for auto-complete, jump-to-definition
and docstring display. I taught it to auto-detect virtualenvs in a project's
directory with jedi-force.el.

I use a combination of `js2-mode <https://github.com/mooz/js2-mode>`__,
`Tern.js <http://ternjs.net/>`__,
`eslint <http://eslint.org/>`__ (via flycheck),
`js2-refactor <https://github.com/magnars/js2-refactor.el>`__ and
`skewer-mode <https://github.com/skeeto/skewer-mode>`__
for live-editing browser-based JavaScript. In theory this should be awesome,
and it's not bad, but there are some kinks to work out.

My PHP setup is lacking smart code-completion and jump-to-definition, but
otherwise, it's pretty solid. The last time I was hacking on PHP seriously I was
on Windows, and I couldn't get `pfff <https://github.com/facebook/pfff>`__ to
compile there, but on *nix that would be the next thing I'd add.

For editing web templates, I use the awesome `web-mode <http://web-mode.org/>`__
with several extensions, including
`emmet-mode <https://github.com/smihica/emmet-mode>`__.

I use css-mode for CSS, which is somewhat lacking, but I use
`skewer-reload-stylesheets
<https://github.com/NateEag/skewer-reload-stylesheets>`__ to live-edit, and
that makes life better.

Look in todo.txt and tell me about entries that can be resolved by turning on
built-in features. I've found a few and would not mind finding more.

Layout
======

elpa/ - elisp packages installed via package.el. I keep this under version
control so that I can always return to a known-good state if an upgrade has
unwanted effects, and so my configuration is less dependent on third-party
services.

githooks/ - a few git hooks to aid in hacking on this config, mostly useful
when pushing config changes between multiple machines.

init.el - where the magic begins.

site-lisp/ - elisp packages I update manually. Some are not available via
package.el while others are my own. There are probably some third-party
libraries that I never realized are on package.el, too.

snippets/ - my personal yasnippets, augmenting the base yasnippet collection.
