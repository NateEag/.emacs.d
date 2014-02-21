============================
Nate Eagleson's Emacs Config
============================

Ever since the fateful day Dr. Thang Bui told his C++ programming class that we
should use emacs to edit text files on the machines in the Sun lab, Emacs has
been my editor.

On that first encounter, I was put off by its user interface, which is
obscurantist at best. I discovered that the default install is incredibly
heavyweight fairly quickly, and I was not impressed with how little it believed
in graphical environments.

After a naive search for the perfect editor in my naive early twenties, when I
narrowly avoided wasting years on writing my own, in 2009 I decided to learn
Emacs, since I knew some of its shortcuts fairly well, and I needed to stick
with something.

A year or two into the project, I realized my dream editor had been sitting in
my lap all along, if only I'd tried to learn it.

It is Turing-complete, it has been polished over decades to be an
extraordinarily powerful programmer's editor, I can use it on almost any
platform, and I can evolve it to work exactly the way I want it to, without the
overhead of recompiling or even restarting it.

Still, you may find a few useful things in here. If you have a moment, glance
at todo.txt and send me the two-line patches that address desires I didn't know
Emacs already had built-in. I've found some of those myself, and wouldn't mind
finding more.

Note that the elpa/ subdir contains packages installed via package.el. Many
people insist that you shouldn't keep these under version control, but I
disagree.

My editor is fundamental to my workflow, and keeping these files under version
control means that I'm not at the whims of package servers and authors who make
breaking updates.

Imagine discovering a subtle, critical bug in a major mode *after* updating
every install you have, or updating then discovering the author broke
compatibility with all those little helper functions you've piled up.

By keeping my ELPA-installed packages in the repo, I avoid that risk, make my
setup easier to reproduce on a new box, and make it easy to bring back
configurations from the distant past.
