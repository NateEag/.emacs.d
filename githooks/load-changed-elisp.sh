#! /bin/bash

# If passed a pair of commit identifiers, this shell script will use emacsclient
# to load any new or updated files in the existing emacs daemon.

# This is probably only useful after git pull, to load latest config changes
# into a long-running instance.

emacs --batch --eval "(require 'server)" \
    --eval "(if (not (eq (server-running-p) t)) (kill-emacs 1))"
is_server_running=$?
if [ $is_server_running -eq 0 ]; then
    updated_files=$(git diff --name-only --diff-filter=ACM "$1" "$2" | grep '\.el$')
    for file in $updated_files
    do
        emacsclient --eval \
            "(if (server-running-p) (load-file \"~/.emacs.d/$file\"))"
    done
fi
