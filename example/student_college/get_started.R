# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin get_started.R$$ $newlinech #$$
#
# $section Get Starting Using This Package$$
#
# $head Discussion$$
# This is the 7 students, 2 colleges, 3 available slots example from
# $href%
# https://cran.r-project.org/web/packages/matchingMarkets/matchingMarkets.pdf
# %matchingMarkets
# %$$.
# The results are checked for correctness as well as being printed
# to standard output.
#
# $head Source Code$$
# $srcfile%example/student_college/get_started.R%0# %# BEGIN R%# END R%1%$$
#
# $end
# BEGIN R
# ---------------------------------------------------------------------------
source('R/student_college.R') # load the student_college function
setwd('build')                # put files in build directory
# ---------------------------------------------------------------------------
# student.csv
data <- paste(
"s1,s2,s3,s4,s5,s6,s7" , # student names
"c1,c1,c1,c1,c1,c1,c1" , # college c1 is first choice for all students
"c2,c2,  ,c2,c2,c2,c2" , # student s3 has no second choice of college
sep="\n" # put a newline character between each line above
)
write(data, "student.csv")
# ---------------------------------------------------------------------------
# college.csv
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
ok  <- length(optimal) == 5
ok  <- ok && student[1] == "s1" && college[1] == "c1"
ok  <- ok && student[2] == "s2" && college[2] == "c1"
ok  <- ok && student[3] == "s3" && college[3] == "c1"
ok  <- ok && student[4] == "s4" && college[4] == "c2"
ok  <- ok && student[5] == "s5" && college[5] == "c2"
for( i in seq(5) )
	ok <- ok && optimal[i] == "b" && matchid[i] == "1"
#
if( ok )
{	message("get_started: OK")
} else {
	stop("get_started: Error")
}
# END R