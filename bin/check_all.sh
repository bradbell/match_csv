#! /bin/bash -e
# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#       Copyright (C) 2017-18 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#              GNU General Public License version 3.0 or later see
#                  https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
program='bin/check_all.sh'
if [ "$0" != "$program" ]
then
	echo "$program: must be executed from its parent directory."
	exit 1
fi
# -----------------------------------------------------------------------------
# bash function that echos and executes a command
echo_eval() {
	echo $*
	eval $*
}
# -----------------------------------------------------------------------------
list='
	example/student_college/get_started.R
	example/student_college/bad_student.R
'
ok='yes'
for file in $list
do
	name=`echo $file | sed -e 's|.*/||' -e 's|\.R||'`
	output="build/$name.out"
	if R CMD BATCH --slave --vanilla $file $output
	then
		echo "$name: OK:"
	else
		echo "$name: Error:"
		ok='no'
	fi
done
# -----------------------------------------------------------------------------
if [ "$ok" == 'no' ]
then
	echo "$program: Error"
	exit 1
fi
echo "$program: OK"
exit 0
