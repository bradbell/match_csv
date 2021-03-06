# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin bad_college.R$$ $newlinech #$$
#
# $section Example: College Thinks To Highly of It's Self$$
#
# $head Discussion$$
# This is a case where a college will only accept the best students,
# but they will not accept the college.
#
# $head Source Code$$
# $srcfile%example/student_college/bad_college.R%0# %# BEGIN R%# END R%1%$$
#
# $end
# BEGIN R
# ---------------------------------------------------------------------------
source('R/student_college.R') # load the student_college function
setwd('build')                # put files in build directory
# ---------------------------------------------------------------------------
# student.csv
data <- paste(
"s1,s2,s3,s4,s5,s6", # student names
"c1,c1,c1,c1,c1,c1", # college c1 is first choice for all students
"c2,c2,c2,c2,c2,c2", # college c2 is second choice for all students
"c3,c3,c3,c3,  ,  ", # college c3 is not acceptible to s5 and s6
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
"s4,s4,  ",  # student s4 is 3rd   choice for all colleges except c3
"s3,s3,  ",  # student s3 is 4th   choice for all colleges except c3
"s2,s2,  ",  # student s2 is 5th   choice for all colleges except c3
"s1,s1,  ",  # student s1 is 6th   choice for all colleges except c3
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
#
optimal <- match_data_frame[,"optimal"]
student <- match_data_frame[,"student"]
college <- match_data_frame[,"college"]
matchid <- match_data_frame[,"matchid"]
#
ok  <- length(optimal) == 4
ok  <- ok && student[1] == "s3" && college[1] == "c2"
ok  <- ok && student[2] == "s4" && college[2] == "c2"
ok  <- ok && student[3] == "s5" && college[3] == "c1"
ok  <- ok && student[4] == "s6" && college[4] == "c1"
for( i in seq(4) )
	ok <- ok && optimal[i] == "b" && matchid[i] == "1"
#
if( ok )
{	message("bad_college: OK")
} else {
	stop("bad_college: Error")
}
# END R
