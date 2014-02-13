#! /bin/bash

# Lame little script to install flake8 for use by flycheck. I don't like having
# platform-specific binary files in a repo, or else I'd just commit it.

# NOTE WELL: If you're on OS X 10.6.8, built-in Python's got an SSL issue that
# makes installing things via pip unworkable. To use this script on that
# platform, install a more recent Python and get it first on your path. Once
# the virtualenv's built, that shouldn't matter, of course...

bin_dir=$(dirname $0)
virtualenv $bin_dir/flake8-env

source $bin_dir/flake8-env/bin/activate

easy_install-2.6 pip

pip install flake8
