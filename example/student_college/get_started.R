# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#       Copyright (C) 2017-18 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#              GNU General Public License version 3.0 or later see
#                  https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin get_started.R$$ $newlinech #$$
#
# $section Student / College Matching example$$
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
match        <- student_college(student_file, college_file)
ok  <- nrow(match) == 5
ok  <- ok && match[1,"student"] == "s1" && match[1,"college"] == "c1"
ok  <- ok && match[2,"student"] == "s2" && match[2,"college"] == "c1"
ok  <- ok && match[3,"student"] == "s3" && match[3,"college"] == "c1"
ok  <- ok && match[4,"student"] == "s4" && match[4,"college"] == "c2"
ok  <- ok && match[5,"student"] == "s5" && match[5,"college"] == "c2"
# print the results
print(match)
#
if( ok )
{	message("get_started: OK")
} else {
	stop("get_started: Error")
}
# END R
