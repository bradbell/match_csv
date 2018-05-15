# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin bad_student.R$$ $newlinech #$$
#
# $section Example: Student Thinks to Highly of It's Self$$
#
# $head Discussion$$
# This is a case where a student will only accept the best college,
# but that college will not accept that student.
#
# $head Source Code$$
# $srcfile%example/student_college/bad_student.R%0# %# BEGIN R%# END R%1%$$
#
# $end
# BEGIN R
# ---------------------------------------------------------------------------
source('R/student_college.R') # load the student_college function
setwd('build')                # put files in build directory
# ---------------------------------------------------------------------------
# student.csv
data <- paste(
"s1,s2,s3,s4,s5,s6" , # student names
"c3,c3,c3,c3,c3,c3" , # college c3 is first choice for all students
"  ,c2,c2,c2,c2,c2" , # college c2 is second choice for all but student s1
"  ,c1,c1,c1,c1,c1" , # college c1 is third  choice for all but student s1
sep="\n" # put a newline character between each line above
)
write(data, "student.csv")
# ---------------------------------------------------------------------------
# college.csv
data <- paste(
"c1,c2,c3",  # college names
" 2, 2, 2",  # all colleges have 2 available slots
"s6,s6,s6",  # student s6 is first choice for all colleges
"s5,s5,s5",  # student s5 is 2nd   choice for all colleges
"s4,s4,s4",  # student s4 is 3rd   choice for all colleges
"s3,s3,s3",  # student s3 is 4th   choice for all colleges
"s2,s2,s2",  # student s2 is 5th   choice for all colleges
"s1,s1,  ",  # student s1 is not acceptable for college c3
sep="\n" # put a newline character between each line above
)
write(data, "college.csv")
# ---------------------------------------------------------------------------
student_file <- "student.csv"
college_file <- "college.csv"
match_file   <- "match.csv"
student_college(student_file, college_file, match_file)
# ---------------------------------------------------------------------------
match_data_frame <- read.csv(
	match_file, colClasses  = "character", check.names = FALSE
)
print(match_data_frame)
#
optimal <- match_data_frame[,"optimal"]
student <- match_data_frame[,"student"]
college <- match_data_frame[,"college"]
matchid <- match_data_frame[,"matchid"]
#
ok  <- length(optimal) == 5
ok  <- ok && student[1] == "s2" && college[1] == "c1"
ok  <- ok && student[2] == "s3" && college[2] == "c2"
ok  <- ok && student[3] == "s4" && college[3] == "c2"
ok  <- ok && student[4] == "s5" && college[4] == "c3"
ok  <- ok && student[5] == "s6" && college[5] == "c3"
for( i in seq(5) )
	ok <- ok && optimal[i] == "b" && matchid[i] == "1"
#
if( ok )
{	message("bad_student: OK")
} else {
	stop("bad_student: Error")
}
# END R
