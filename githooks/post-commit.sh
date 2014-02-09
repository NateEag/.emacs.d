#! /bin/bash

hooks_dir=$(dirname $0)

$hooks_dir/compile-changed-elisp.sh HEAD^ HEAD
$hooks_dir/rm-stale-bytecode-files.sh HEAD^ HEAD
