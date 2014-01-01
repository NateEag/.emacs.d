#! /bin/bash

# My post-merge hook.

hooks_dir=$(dirname $0)

$hooks_dir/load-changed-elisp.sh HEAD ORIG_HEAD
$hooks_dir/compile-changed-elisp.sh HEAD ORIG_HEAD
