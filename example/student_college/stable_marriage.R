# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#       Copyright (C) 2017-18 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#              GNU General Public License version 3.0 or later see
#                  https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin stable_marriage.R$$ $newlinech #$$
#
# $section Marriage Problem With The Couples$$
#
# $head Under Construction$$
# This example currently crashes and is under investigation.
#
# $head Discussion$$
# This is a marriage problem because there is just one slot per college.
# There are three couples and three stable solutions; see
# $href%https://en.wikipedia.org/wiki/Stable_marriage_problem%
# stabe_marriage_problem%$$.
#
# $head Source Code$$
# $srcfile%example/student_college/stable_marriage.R%0# %# BEGIN R%# END R%1%$$
#
# $end
# BEGIN R
# ---------------------------------------------------------------------------
source('R/student_college.R') # load the student_college function
setwd('build')                # put files in build directory
# ---------------------------------------------------------------------------
# student.csv
data <- paste(
"A,B,C" , # student names
"Y,Z,X" ,
"Z,Y,X" ,
"X,Z,Y" ,
sep="\n" # put a newline character between each line above
)
write(data, "student.csv")
# ---------------------------------------------------------------------------
# college.csv
data <- paste(
"X,Y,Z" , # college names
"1,1,1" ,
"B,C,A" ,
"A,B,C" ,
"C,A,B" ,
sep="\n"  # put a newline character between each line above
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
ok <- TRUE
print(match)
#
if( ok )
{	message("stable_marriage: OK")
} else {
	stop("stable_marriage: Error")
}
# END R
