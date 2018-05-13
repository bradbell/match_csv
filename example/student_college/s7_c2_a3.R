# $begin student_college_s7_c2_a3.R$$ $newlinech #$$
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
# $srcfile%example/student_college/s7_c2_a3.R%0# %# BEGIN R%# END R%1%$$
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
"c2,c2,,c2,c2,c2,c2"   , # student s3 has no second choice of college
sep="\n"                 # put a newline character between each line above
)
write(data, "student.csv")
# ---------------------------------------------------------------------------
# college.csv
data <- paste(
"c1,c2",  # college names
"3,3",    # both colleges have 3 available slots
"s1,s1",  # student s1 is first  choice for both colleges
"s2,s2",  # student s2 is second choice for both colleges
"s3,s3",  # student s3 is third  choice for both colleges
"s4,s4",  # student s4 is fourth choice for both colleges
"s5,s5",  # student s5 is fifth  choice for both colleges
"s6,",    # s6 is sixth   choice for c1, not acceptable to c2
"s7,",    # s7 is seventh choice for c1, not acceptable to c2
sep="\n"  # put a newline character between each line above
)
write(data, "college.csv")
# ---------------------------------------------------------------------------
student_file <- "student.csv"
college_file <- "college.csv"
result       <- student_college(student_file, college_file)
ok  <- nrow(result) == 5
ok  <- ok && result[1,"student"] == "s1" && result[1,"college"] == "c1"
ok  <- ok && result[2,"student"] == "s2" && result[2,"college"] == "c1"
ok  <- ok && result[3,"student"] == "s3" && result[3,"college"] == "c1"
ok  <- ok && result[4,"student"] == "s4" && result[4,"college"] == "c2"
ok  <- ok && result[5,"student"] == "s5" && result[5,"college"] == "c2"
# print the results
print(result)
#
if( ok )
{	message("student2college_xam: OK")
} else {
	message("student2college_xam: Error")
	quit(status = 1)
}
# END R
