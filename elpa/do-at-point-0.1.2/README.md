Dispatch Context Sensitive Commands
===================================

Find here the source for do-at-point.el and the command `do-at-point`,
that uses `thing-at-point` to cycle through various "things" to select
and then dispatch appropriate actions on these selections.

See also Omar Antolín Camarena's [Embark] package, which provides a
more feature-full implementation of the same concept, with am emphasis
on interacting with the minibuffer.

[Embark]:
	https://github.com/oantolin/embark

Installation
------------

Do-at-point.el is avaliable from [GNU ELPA].  It can be installed by
invoking

	M-x package-install RET do-at-point RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/do-at-point.html

Usage
-----

The main and presently only entry point for this package is the
autoloaded `do-at-point` command itself.  Bind it to a convenient key,
for example

    (global-set-key (kbd "C-'") #'do-at-point)

There are some options that users may configure.  After having have
loaded the package consult `M-x apropos-user-option RET do-at-point-
RET` for an overview.

Contribute
----------

As do-at-point.el is distributed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Do-at-point.el is developed on [SourceHut].

[SourceHut]:
	https://git.sr.ht/~pkal/do-at-point

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted to my [public
inbox].

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox

Distribution
------------

Auto-header.el and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
