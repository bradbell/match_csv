# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin get_started.R$$ $newlinech #$$
#
# $section Get Starting Using student_college$$
#
# $head Run This Example$$
# $list number$$
# Change into the
# $cref/distribution directory
#	/match_csv
#	/Installation
#	/Distribution Directory/$$,
# $lnext
# If it does not yet exist, create the $code build$$ sub-directory.
# $lnext
# Start the R program.
# $lnext
# Execute the following R command.
# $srccode%R%
#	source('example/student_college/get_started.R')
# %$$
# $lend
# Below is a step by step explanation of this example
# together with the corresponding R source code.
#
# $head Load The student_college Routine$$
# The following R commands loads the $code student_college$$ routine
# into your R session.
# You must do this before you can run the $code student_college$$ routine.
# $srccode%R%
source('R/student_college.R')
# %$$
#
# $head Create The Student File$$
# The R commands below create the
# $cref/student_file/student_college/student_file/$$ for this example.
# You could use a spread sheet to create this file.
# $srccode%R%
data <- paste(
"s1,s2,s3,s4,s5,s6,s7" , # student names
"c1,c1,c1,c1,c1,c1,c1" , # college c1 is first choice for all students
"c2,c2,  ,c2,c2,c2,c2" , # student s3 has no second choice of college
sep="\n"                 # put a newline character between each line above
)
write(data, "build/student.csv")
# %$$
# Note that this example has 7 students.
#
# $head Create the College File$$
# The R commands below create the
# $cref/college_file/student_college/college_file/$$ for this example.
# You could use a spread sheet to create this file.
# $srccode%R%
data <- paste(
"c1,c2",  # college names
" 3, 3",  # both colleges have 3 available slots
"s1,s1",  # student s1 is first  choice for both colleges
"s2,s2",  # student s2 is second choice for both colleges
"s3,s3",  # student s3 is third  choice for both colleges
"s4,s4",  # student s4 is fourth choice for both colleges
"s5,s5",  # student s5 is fifth  choice for both colleges
"s6,  ",  # s6 is sixth   choice for c1, not acceptable to c2
"s7,  ",  # s7 is seventh choice for c1, not acceptable to c2
sep="\n"  # put a newline character between each line above
)
write(data, "build/college.csv")
# %$$
# Note that this example has
# 2 colleges and 3 available slots available for each college.
#
# $head Run student_college$$
# The R commands below run the $cref student_college$$ routine
# for this example.
# Normally you would change the file names when running this routine.
# $srccode%R%
student_file <- "build/student.csv"
college_file <- "build/college.csv"
match_file   <- "build/match.csv"
student_college(student_file, college_file, match_file)
# %$$
#
# $head Check Match$$
# The R commands below check the match file for this example.
# You could load this file into a spread sheet
# to see the match.
#
# $subhead Read Match File$$
# $srccode%R%
match_data_frame <- read.csv(
	"build/match.csv", colClasses  = "character", check.names = FALSE
)
# %$$
#
# $subhead Extract Optimal Match$$
# $srccode%R%
optimal <- match_data_frame[,"optimal"]
student <- match_data_frame[,"student"]
college <- match_data_frame[,"college"]
matchid <- match_data_frame[,"matchid"]
# %$$
#
# $subhead Check Optimal Match$$
# $srccode%R%
ok  <- length(optimal) == 5
ok  <- ok && student[1] == "s1" && college[1] == "c1"
ok  <- ok && student[2] == "s2" && college[2] == "c1"
ok  <- ok && student[3] == "s3" && college[3] == "c1"
ok  <- ok && student[4] == "s4" && college[4] == "c2"
ok  <- ok && student[5] == "s5" && college[5] == "c2"
for( i in seq(5) )
	ok <- ok && optimal[i] == "b" && matchid[i] == "1"
# %$$
#
# $head Report Check Result$$
# $srccode%R%
if( ok )
{	message("get_started: OK")
} else {
	stop("get_started: Error")
}
# %$$
#
#
# $end
