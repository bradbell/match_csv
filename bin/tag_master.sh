#! /bin/bash -e
# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
program='bin/tag_master.sh'
if [ "$0" != "$program" ]
then
	echo "$program: must be executed from its parent directory."
	exit 1
fi
git_branch=`git rev-parse --abbrev-ref HEAD`
git_status=`git status -s`
version=`version.sh get`
#
if [ "$git_branch" != 'master' ]
then
	echo 'bin/tag_master.sh must be executed from master branch'
	exit 1
fi
if [ "$git_status" != '' ]
then
	echo 'bin/tag_master.sh: first check in or remove changes to master'
	git status -s
	exit 1
fi
if git --list | grep "$version"
then
	echo "The git tag $version alread exists. Delete the old version ?"
	echo "	git tag -d $version"
	echo "	git push --delete origin $version"
	exit 1
fi
#
echo "git tag -a -m \"corresponds to master branch on $version\" $version"
git tag -a -m "corresponds to master branch on $version" $version
#
echo "git push origin $version"
git push origin $version
#
echo 'tag_master.sh: OK'
