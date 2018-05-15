# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin stable_marriage.R$$ $newlinech #$$
# $spell
#	optimality
# $$
#
# $section Stable Marriage Problem With Three Solutions$$
#
# $head Discussion$$
# This is a marriage problem because there is just one slot per college.
# There are three couples and three stable solutions;
# see optimality of the solution for the
# $href%https://en.wikipedia.org/wiki/Stable_marriage_problem#Optimality_of_the_solution%
# stabe_marriage_problem%$$.
# Only one of the solutions is student optimal.
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
"X,Y,Z" ,
"Z,X,Y" ,
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
match_data_frame <- read.csv(
	match_file, colClasses  = "character", check.names = FALSE
)
#
# select the solution that is student optimal
optimal <- match_data_frame[,"optimal"]
student <- match_data_frame[,"student"][ optimal == "s" ]
college <- match_data_frame[,"college"][ optimal == "s" ]
matchid <- match_data_frame[,"matchid"][ optimal == "s" ]
#
browser()
id1 <- matchid[1]
ok  <- length(student) == 3
ok  <- ok && student[1] == "A" && college[1] == "Y" && matchid == id1
ok  <- ok && student[2] == "B" && college[2] == "Z" && matchid == id1
ok  <- ok && student[3] == "C" && college[3] == "X" && matchid == id1
#
if( ok )
{	message("stable_marriage: OK")
} else {
	stop("stable_marriage: Error")
}
# END R
