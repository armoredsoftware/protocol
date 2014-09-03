#!/bin/sh
#
# Change the username for the git push command to work.
# NOTE: This file is created in the kickstart file %post section.
# 
if [ ! $# -eq 1 ] ; then 
  echo "You must provide 1 argument that is a github username that is a member of the armoredsoftware github group."
  exit 1
fi
git remote set-url origin https://${1}@github.com/armoredsoftware/protocol.git
git config user.name "${1}"
