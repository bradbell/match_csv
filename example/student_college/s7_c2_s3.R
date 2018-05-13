# -------------------------
source('R/student_college.R')
setwd('build')
# -------------------------
# student.csv
data <- paste(
"s1,s2,s3,s4,s5,s6,s7" ,
"c1,c1,c1,c1,c1,c1,c1" ,
"c2,c2,,c2,c2,c2,c2"   ,
sep="\n"
)
write(data, "student.csv")
# -------------------------
# college.csv
data <- paste(
"c1,c2",
"1,1",
"s1,s1",
"s2,s2",
"s3,s3",
"s4,s4",
"s5,s5",
"s6,",
"s7,",
sep="\n"
)
write(data, "college.csv")
# ----------------------------------------------------------------
student_file <- "student.csv"
college_file <- "college.csv"
result       <- student_college(student_file, college_file)
# expect 5 matches
ok  <- dim(result)[1] == 5
# result[i,1] is student and result[i,2] is college for i-th match
ok  <- ok && paste(result[1,1], result[1,2], sep=",") == "s1,c1"
ok  <- ok && paste(result[2,1], result[2,2], sep=",") == "s2,c1"
ok  <- ok && paste(result[3,1], result[3,2], sep=",") == "s3,c1"
ok  <- ok && paste(result[4,1], result[4,2], sep=",") == "s4,c2"
ok  <- ok && paste(result[5,1], result[5,2], sep=",") == "s5,c2"
if( ok )
{	message("student2college_xam: OK")
} else {
	message("student2college_xam: Error")
	quit()
}
# ----------------------------------------------------------------

