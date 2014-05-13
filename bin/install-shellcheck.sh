#! /bin/bash

# Simple script to compile and install ShellCheck, for the good of flycheck.
#
# It requires Haskell and cabal, and relies on cabal's install system, so the
# binary winds up being installed for your user account, not directly here in
# .emacs.d.

tmp_dir=$(dirname "$0")/../tmp
cd "$tmp_dir"
git clone https://github.com/koalaman/shellcheck.git
cd shellcheck
cabal install
