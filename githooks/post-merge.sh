#! /bin/bash

# My post-merge hook.

hooks_dir=$(dirname $0)

$hooks_dir/compile-changed-elisp.sh ORIG_HEAD HEAD
$hooks_dir/load-changed-elisp.sh ORIG_HEAD HEAD
$hooks_dir/rm-stale-bytecode-files.sh ORIG_HEAD HEAD
