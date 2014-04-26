============================
Nate Eagleson's Emacs Config
============================

Ever since Dr. Thang Bui told his C++ programming class to use it for writing
programs in the Sun lab, Emacs has been my editor.

On that first encounter, I was put off by its user interface, which is
obscurantist at best. I discovered that the default install is incredibly
heavyweight fairly quickly, and I was not impressed with how little it believed
in graphical environments.

After a naive search for the perfect editor in my early twenties (I narrowly
avoided wasting years writing one), in 2009 I decided to learn Emacs, since I
knew some of its shortcuts fairly well, and I needed to learn something well.

A year or two into the project, I realized my dream editor had been sitting in
my lap all along, if only I'd tried to learn it.

It is Turing-complete, it has been honed over decades to a razor-sharp edge, it
runs almost everywhere, and it slowly molds itself to my workflow without the
overhead of recompiling or even restarting.

Since I have only used it seriously for a few years, I know there is much I am
missing. Still, you may find a few useful things in here.

If you have a moment, glance at todo.txt and send me the two-line patches that
address desires I didn't know Emacs already had built-in. I've found some of
those myself, and wouldn't mind finding more.

Layout
======

bin/ - external programs for use by Emacs extensions like flycheck. At present
this contains only interpretable source code, no true binaries, so installing
system-wide tools like Python and node.js is still necessary.

elpa/ - elisp packages installed via package.el. I keep this under version
control so that I can always return to a known-good state if an upgrade has
unwanted effects, and so my configuration is less dependent on third-party services.

githooks/ - a few git hooks to aid in hacking on this config, mostly useful
when pushing config changes between multiple machines.

site-lisp/ - elisp packages I maintain manually. Some are not available via
package.el, while others are my own little hackjobs.

snippets/ - my personal yasnippets, used as an extension to the default
yasnippet collection.
