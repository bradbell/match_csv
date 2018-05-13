# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#       Copyright (C) 2017-18 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#              GNU General Public License version 3.0 or later see
#                  https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin student_college$$ $newlinech #$$
# $spell
#	nrow
# $$
#
# $section Student / College Matching Problem$$
#
# $head Description$$
# Finds one of the stable matchings for the college admissions problem.
#
# $head Assumptions$$
# $list number$$
# No two students have the same name and no student name is empty.
# $lnext
# No two colleges have the same name and no college name is empty.
# $lend
#
# $head n_student$$
# We use $code n_student$$ to denote the number of students in the match,
# which is also equal to the number of columns in $icode student_file$$.
#
# $head n_college$$
# We use $icode n_college$$ to denote the number of colleges in the match,
# which is also equal to the number of columns in $icode college_file$$.
#
# $head student_file$$
# This is a comma separated value file with $icode n_student$$ columns and
# $icode%n_college%+1%$$ rows.
# The first row contains the names of the students.
# The other entries in each column are the names of the
# acceptable colleges for the student name in the first row of the column.
# The lower the row index, the more preferred the college is to the student.
# No college name appears twice in a column.
# Empty rows, for the colleges that do not appear in a column, are all at
# the bottom of the column.
#
# $head college_file$$
# This is a comma separated value file with $icode n_college$$ columns and
# $icode n_student+2$$ rows.
# The first row contains the names of the colleges.
# Each column of the second row contains a positive integer that specifies
# the number of students the corresponding
# college is willing to accept.
# The other entries in each column are the names of the
# acceptable students for the college name in the first row of the column.
# The lower the row index, the more preferred the college is to the student.
# No student name appears twice in a column.
# Empty rows, for the students that do not appear in a column, are all at
# the bottom of the column.
#
# $head result$$
# The return value $icode result$$ is a data frame with column names
# $code student$$ and $code college$$.
# Each row of the result is a matched pair of a student to a particular college.
# The number of rows $codei%nrow(%result%)%$$ is the number of pairs.
# The student and college for the $th i$$ pair are
# $icode%result%[%i%,"student"]%$$ and $icode%[%i%,"college"]%$$ respectively.
#
# $children%example/student_college/s7_c2_a3.R
# %$$
# $head Example$$
# The file $cref student_college_s7_c2_a3.R$$ contains an example
# and test using $code student_college$$.
#
# $end
student_college <- function(student_file, college_file)
{
	# packages
	requireNamespace("readr")
	requireNamespace("matchingMarkets")
	# -------------------------------------------------------------------------
	# read input files
	#
	col_types          <- readr::cols( .default = readr::col_character() )
	student_data_frame <- readr::read_csv(student_file, col_types = col_types)
	college_data_frame <- readr::read_csv(college_file, col_types = col_types)
	# -------------------------------------------------------------------------
	# sizes
	student_size    <- dim(student_data_frame)
	college_size    <- dim(college_data_frame)
	#
	n_student       <- student_size[2]
	n_college       <- college_size[2]
	#
	# check number of rows in student_file
	if( student_size[1] != n_college )
	{	fmt <- "number of rows in student_file = %d"
		message( sprintf(fmt, student_size[1] + 1) )
		fmt <- "n_college + 1 = %d"
		message( sprintf(fmt, n_college + 1 ) )
		quit(status = 1)
	}
	#
	# check number of rows in college_file
	if( college_size[1]  != n_student + 1 )
	{	fmt <- "number of rows in college_file = %d"
		message( sprintf(fmt, college_size[1] + 1) )
		fmt <- "n_student + 2 = %d"
		message( sprintf(fmt, n_student + 2 ) )
		quit(status = 1)
	}
	# -------------------------------------------------------------------------
	# names
	student_name <- colnames(student_data_frame)
	college_name <- colnames(college_data_frame)
	# -------------------------------------------------------------------------
	# number of slots for j-th college is n_slot[j]
	n_slot <- rep(0, n_college)
	for ( j in seq(n_college) )
	{	n_slot[j]      <- as.integer( college_data_frame[1, j] )
	}
	# -------------------------------------------------------------------------
	# college preference matrix
	college_preference <- matrix(
		rep(0, n_student * n_college), n_student, n_college
	)
	for( j in seq(n_college) )
	{	for(i in seq(n_student) )
		{	name  <- college_data_frame[i+1,j]
			if( is.na(name) )
				college_preference[i,j] <- NA
			else
			{	name <- as.character(name)
				index <- which( student_name == name )
				if( length(index) > 1 )
				{	message("program error")
					quit(status = 1)
				}
				if( length(index) == 0 )
				{	fmt <- "college file, row %d, column %d, name ='%s'"
					message( sprintf(fmt, i+2, j, name) )
					message("is not a valid student name")
					quit(status = 1)
				}
				college_preference[i,j] <- index
			}
		}
	}
	# -------------------------------------------------------------------------
	# student preference matrix
	student_preference <- matrix(
		rep(0, n_college * n_student), n_college, n_student
	)
	for( j in seq(n_student) )
	{	for(i in seq(n_college) )
		{	name  <- student_data_frame[i,j]
			if( is.na(name) )
				student_preference[i,j] <- NA
			else
			{	name  <- as.character(name)
				index <- which( college_name == name )
				if( length(index) > 1 )
				{	message("program error")
					quit(status = 1)
				}
				if( length(index) == 0 )
				{	fmt <- "student file, row %d, column %d, name ='%s'"
					message( sprintf(fmt, i+1, j, name) )
					message("is not a valid college name")
					quit(status = 1)
				}
				student_preference[i,j] <- index
			}
		}
	}
	# -------------------------------------------------------------------------
	# preform the match
	res <- matchingMarkets::hri(
		nStudents = n_student,
		nColleges = n_college,
		nSlots    = n_slot,
		s.prefs   = student_preference,
		c.prefs   = college_preference,
		s.range   = NULL,
		c.range   = NULL
	)
	matching       <- res["matchings"][[1]]
	match_number   <- matching[["matching"]]
	student_match  <- matching[["student"]][match_number == 1]
	college_match  <- matching[["college"]][match_number == 1]
	n_match        <- length(college_match)
	student        <- c()
	college        <- c()
	for ( i in seq(n_match) )
	{	this_student <- student_name[ as.integer( student_match[i] ) ]
		this_college <- college_name[ as.integer( college_match[i] ) ]
		student     <- c(student, this_student)
		college     <- c(college, this_college)
	}
	result <- data.frame(student, college)
	result <- result[order(student),]
	return(result)
}
# ----------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/matchingMarkets/matchingMarkets.pdf
# 7 students, 2 colleges, 3 slots at each college
#s.prefs <- matrix(c(1,2, 1,2, 1,NA, 1,2, 1,2, 1,2, 1,2), 2,7)
#c.prefs <- matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,NA,NA), 7,2)
#res <- matchingMarkets::hri(s.prefs=s.prefs, c.prefs=c.prefs, nSlots=c(3,3))
#matching <- res["matchings"][[1]]
#student  <- matching[["student"]]
#college  <- matching[["college"]]
#n_match  <- length(college)
#message( sprintf("%10s%10s", "student", "college") )
#for ( i in seq(n_match) )
  # student s1 is first choice for both colleges#	message( sprintf("%10d%10d", student[i], college[i]) )
