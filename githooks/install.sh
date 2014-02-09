#! /bin/bash

githooks_dir=$(dirname $0)
git_dir=$githooks_dir/../.git

cp $githooks_dir/post-merge.sh $git_dir/hooks/post-merge
chmod +x $git_dir/hooks/post-merge

cp $githooks_dir/post-commit.sh $git_dir/hooks/post-commit
chmod +x $git_dir/hooks/post-commit

cp $githooks_dir/load-changed-elisp.sh $git_dir/hooks/load-changed-elisp.sh
chmod +x $git_dir/hooks/load-changed-elisp.sh

cp $githooks_dir/compile-changed-elisp.sh $git_dir/hooks/compile-changed-elisp.sh
chmod +x $git_dir/hooks/compile-changed-elisp.sh

cp $githooks_dir/rm-stale-bytecode-files.sh $git_dir/hooks/rm-stale-bytecode-files.sh
chmod +x $git_dir/hooks/rm-stale-bytecode-files.sh
