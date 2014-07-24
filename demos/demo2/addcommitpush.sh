#!/bin/sh

git add -u
git commit -m ${1}
git push origin jsonBranch
