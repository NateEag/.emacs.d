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


Why Emacs (or: Hokey Religions and Ancient Weapons)
===================================================

Like all art forms, programming has technique.

Despite the grumbling from the graphical language crowd, most programming comes
down to entering, reading, and changing plain text in files. Thus, a programmer
should manipulate text fluidly and effortlessly, the way a pianist plays
arpeggios and scales.

Changing editors for each language, therefore, is a horrible burden. Eclipse
for Java, PyCharm for Python, Sublime for JavaScript... The act of editing a
program in any one of these is different. Rather, the editor should change for
each language, so that the fundamental acts of programming remain unchanged.

For that purpose, Emacs reigns supreme.

It is Turing-complete, it has been honed over decades to a razor-sharp edge, it
runs almost everywhere, and I can mold it to myself without recompiling or even
turning it off.

I am no master, but if you look, you may find things of use to you,
particularly for PHP, HTML templates, and CSS. Python and JavaScript see some
use too, but are not what they could be.

Examine todo.txt and tell me about entries that can be resolved by turning on
existing features. I've found some myself, and would not mind finding more.

Layout
======

bin/ - external programs for use by Emacs extensions like flycheck and tern.
Mostly just shell script wrappers that pass everything to the actual binaries
in lib/, so they work on platforms where symlinks aren't reliable or available.

elpa/ - elisp packages installed via package.el. I keep this under version
control so that I can always return to a known-good state if an upgrade has
unwanted effects, and so my configuration is less dependent on third-party
services.

lib/ - source and/or scripts for installing third-party programs this config
utilizes. I don't want actual binaries in here if I can help it, hence the
installer scripts.

githooks/ - a few git hooks to aid in hacking on this config, mostly useful
when pushing config changes between multiple machines.

init.el - where the magic begins.

site-lisp/ - elisp packages I maintain manually. Some are not available via
package.el, others are my own little hackjobs, and probably some never got
moved.

snippets/ - my personal yasnippets, augmenting the base yasnippet collection.
