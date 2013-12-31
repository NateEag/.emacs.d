#! /bin/bash

githooks_dir=$(dirname $0)
git_dir=$githooks_dir/../.git

cp $githooks_dir/post-merge.sh $git_dir/hooks/post-merge
chmod +x $git_dir/hooks/post-merge
