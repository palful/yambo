#!/bin/bash

#sh clang_format.sh
git init
git add -A
git commit -m "$1"
git_branch=$(git symbolic-ref --short HEAD)
git branch -M $git_branch
git push -u origin $git_branch

# git remote add upstream https://gitlab.com/lumen-code/lumen.git
# git checkout master
# git fetch upstream
# git pull upstream master
# git checkout -b tmp
# git checkout -B master temp
