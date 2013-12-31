#! /bin/bash

# If passed a pair of commit identifiers, this shell script will use emacsclient
# to load any new or updated files in the existing emacs daemon.

# This is probably only useful after git pull, to load latest config changes
# into a long-running instance.

prev_head=$1
new_head=$2

updated_files=$(git diff --name-only --diff-filter=[ACM] $prev_head $new_head | grep '\.el$')
for file in $updated_files
do
    emacsclient --eval "(load-file \"~/.emacs.d/$file\")"
done
