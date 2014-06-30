#! /bin/bash

# Delete any .elc files whose corresponding .el files were deleted or renamed
# between commit $1 and $2.

file_list=$(git diff --name-only --diff-filter=DR "$1" "$2" | grep '\.el$')
for file in $file_list
do
    bytecode_file=${file}c
    if [ -f "$bytecode_file" ]; then
        rm "$bytecode_file"
        echo "Deleted $bytecode_file"
    fi
done
