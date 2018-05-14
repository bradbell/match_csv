# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#       Copyright (C) 2017-18 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#              GNU General Public License version 3.0 or later see
#                  https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin bad_student.R$$ $newlinech #$$
#
# $section Student / College Matching example$$
#
# $head Discussion$$
# This is a case where a student will only accept the best college,
# but that college will not accept the student.
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
match <- read.csv(
	match_file, colClasses  = "character", check.names = FALSE
)
ok  <- nrow(match) == 5
ok  <- ok && match[1,"student"] == "s2" && match[1,"college"] == "c1"
ok  <- ok && match[2,"student"] == "s3" && match[2,"college"] == "c2"
ok  <- ok && match[3,"student"] == "s4" && match[3,"college"] == "c2"
ok  <- ok && match[4,"student"] == "s5" && match[4,"college"] == "c3"
ok  <- ok && match[5,"student"] == "s6" && match[5,"college"] == "c3"
#
if( ok )
{	message("bad_student: OK")
} else {
	stop("bad_student: Error")
}
# END R
