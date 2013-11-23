============================
Nate Eagleson's Emacs Config
============================

I used to have a love/hate relationship with Emacs.

Ever since the fateful day Dr. Thang Bui told his C++ programming class that we
should use emacs to edit text files on the machines in the Sun lab, Emacs has
been my editor.

On that first encounter, I was put off by its user interface, which is
obscurantist at best. I discovered that the default install is incredibly
heavyweight fairly quickly, and I was not impressed with how little it believed
in graphical environments.

These days I pretty much just love it.

It is Turing-complete, it has been polished over decades to be an
extraordinarily powerful programmer's editor, I can use it on almost any
platform, and I can evolve it to work exactly the way I want it to, no
shutdowns required.

I only really buckled down to start using it semi-seriously around 2009,
so I'm not that far along the curve yet.

Still, you may find a few useful things in here. If you have a moment, glance
at todo.txt and send me the two-line patches that address desires I didn't know
Emacs already had built-in. I've found some of those myself, and wouldn't mind
finding more.

Note that the elpa/ subdir contains packages installed via package.el. Many
people insist that you shouldn't keep these under version control, but I
disagree.

My editor is fundamental to my workflow, and keeping these files under version
control means that I'm not at the whims of package servers and authors who make
breaking updates. Imagine discovering a subtle, critical bug in a major mode
*after* updating every install you have. By keeping my ELPA-installed packages
in my git repo, I avoid that risk, make my setup easier to reproduce on a new
box, and make it easy to bring back configurations from the distant past.
