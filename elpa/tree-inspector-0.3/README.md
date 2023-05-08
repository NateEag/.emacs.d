# emacs-tree-inspector

Inspection tool for Emacs Lisp objects that uses a tree view.

An inspector tool for Emacs Lisp objects that uses a tree view.

![tree-inspector.png](tree-inspector.png "Tree Inspector")

Works together with the "normal" inspector when it is loaded; when an object label is clicked on the tree, an inspector is opened on that object.

## Install and usage

`(require 'tree-inspector)` to load.

Then start the inspector with either `M-x tree-inspector-inspect-expression` or `M-x tree-inspector-inspect-last-sexp`.
