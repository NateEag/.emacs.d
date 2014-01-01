#! /bin/bash

updated_files=$(git diff --name-only --diff-filter=[ACM] $1 $2 | grep '\.el$')
for file in $updated_files
do
    emacs -batch -f batch-byte-compile "~/.emacs.d/$file"
done
