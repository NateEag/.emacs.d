#! /bin/bash

file_list=$(git diff --name-only --diff-filter=[ACM] $1 $2 | grep '\.el$')
updated_files=''
for file in $file_list
do
    updated_files+=$(echo "~/.emacs.d/$file")' '
done

emacs -batch -l site-lisp/nateeag-load-path.el -f batch-byte-compile $updated_files
