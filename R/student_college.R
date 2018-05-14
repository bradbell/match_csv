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
#	csv
# $$
#
# $section Student / College Matching Problem$$
#
# $head Syntax$$
# $code%student_college(%student_file%, %college_file%, %match_file%)
# %$$
#
# $head Description$$
# Finds one of the stable matchings for the college admissions problem.
#
# $head Conventions$$
# $list number$$
# The data corresponding to one row and one column is called a cell.
# Leading and trailing white space, in a cell, does not matter.
# A cell with only white space, or nothing, is called empty.
# $lnext
# No two students have the same name and no student name is empty.
# $lnext
# No two colleges have the same name and no college name is empty.
# $lend
#
# $head n_student$$
# We use $icode n_student$$ to denote the number of students in the match,
# which is also equal to the number of columns in $icode student_file$$.
#
# $head n_college$$
# We use $icode n_college$$ to denote the number of colleges in the match,
# which is also equal to the number of columns in $icode college_file$$.
#
# $head student_file$$
# This is a csv input file
# with $icode n_student$$ columns and
# $icode%n_college%+1%$$ rows.
# The first row contains the names of the students.
# The other cells in each column are the names of the
# acceptable colleges for the student name in the first row of the column.
# The lower the row index, the more preferred the college is to the student.
# No college name appears twice in a column.
# For each student (column), empty cells are used for the colleges that are
# not acceptable to a student and are placed at the bottom of the column.
#
# $head college_file$$
# This is a csv input file with $icode n_college$$ columns and
# $icode n_student+2$$ rows.
# The first row contains the names of the colleges.
# Each column of the second row contains a positive integer that specifies
# the number of students positions the corresponding
# college has available.
# The other cells in each column are the names of the
# acceptable students for the college name in the first row of the column.
# The lower the row index, the more preferred the student is to the college.
# No student name appears twice in a column.
# For each college (column), empty cells are used for the students that are
# not acceptable to a college and are placed at the bottom of the column.
#
# $head match_file$$
# This is a csv output file with two columns,
# one named $code student$$ and the other named $code college$$.
# Each row of $icode match$$ is a student, college pair.
# No student appears more than once and not college appears more than
# the number of available positions it has.
# The rows of $icode match$$ are in increasing alphabetical order by
# students name.
#
# $head Example$$
# $childtable%
#	example/student_college/get_started.R%
#	example/student_college/bad_student.R%
#	example/student_college/bad_college.R
# %$$
#
# $end
# -----------------------------------------------------------------------------
empty_cell <- function(cell)
{	return( is.na(cell) || cell == "" )
}
student_college <- function(student_file, college_file, match_file)
{
	# packages
	requireNamespace("matchingMarkets")
	# -------------------------------------------------------------------------
	# read input files
	#
	student_data_frame <- read.csv(
		student_file,
		strip.white = TRUE,
		colClasses  = "character",
		check.names = FALSE
	)
	college_data_frame <- read.csv(
		college_file,
		strip.white = TRUE,
		colClasses  =  "character",
		check.names = FALSE
	)
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
	{	fmt  <- "number of rows in student_file = %d"
		msg1 <- sprintf(fmt, student_size[1] + 1)
		fmt  <- "n_college + 1 = %d"
		msg2 <- sprintf(fmt, n_college + 1 )
		stop( paste(msg1, msg2, sep=', ') )
	}
	#
	# check number of rows in college_file
	if( college_size[1]  != n_student + 1 )
	{	fmt  <- "number of rows in college_file = %d"
		msg1 <- sprintf(fmt, college_size[1] + 1)
		fmt  <- "n_student + 2 = %d"
		msg2 <- sprintf(fmt, n_student + 2 )
		stop( paste(msg1, msg2, sep=', ') )
	}
	# -------------------------------------------------------------------------
	# names
	student_name <- colnames(student_data_frame)
	college_name <- colnames(college_data_frame)
	# -------------------------------------------------------------------------
	# matrices
	student_matrix <- as.matrix( student_data_frame )
	college_matrix <- as.matrix( college_data_frame )
	# -------------------------------------------------------------------------
	# number of slots for j-th college is n_slot[j]
	n_slot <- rep(0, n_college)
	for ( j in seq(n_college) )
		n_slot[j]      <- as.integer( college_matrix[1, j] )
	# -------------------------------------------------------------------------
	# Check for bad students
	student_ok = rep(FALSE, n_student)
	for( j in seq(n_student) )
	{	for( i in seq(n_college) )
		{	college <- student_matrix[i,j]
			if( ! ( empty_cell(college) || student_ok[j] ) )
			{	acceptible_student = college_matrix[,college]
				if( student_name[j] %in% acceptible_student )
					student_ok[j] <- TRUE
			}
		}
	}
	#
	# new college_matrix
	# do not worry about overwriting first row becasue already have n_solt
	vec <- rep(TRUE, (n_student + 1) * n_college)
	ok  <- matrix(vec, n_student + 1, n_college)
	for( student in student_name[ ! student_ok ] )
		ok <- ok & college_matrix != student
	college_matrix[! ok] <- NA
	#
	# new n_student
	n_student <- sum(student_ok)
	# new student_matrix
	student_matrix <- student_matrix[, student_ok]
	# new student_name
	student_name       <- student_name[student_ok]
	# -------------------------------------------------------------------------
	# Check for bad colleges
	college_ok = rep(FALSE, n_college)
	for( j in seq(n_college) )
	{	for( i in seq(n_student) )
		{	student <- college_matrix[i+1,j]
			if( ! ( empty_cell(student) || college_ok[j] ) )
			{	acceptible_college = student_matrix[,student]
				if( college_name[j] %in% acceptible_college )
					college_ok[j] <- TRUE
			}
		}
	}
	#
	# new student_matrix
	vec <- rep(TRUE, n_college * n_student)
	ok  <- matrix(vec, n_college, n_student)
	for( college in college_name[ ! college_ok ] )
		ok <- ok & student_matrix != college
	student_matrix[! ok] <- NA
	#
	# new n_college
	n_college      <- sum(college_ok)
	# new college_matrix
	college_matrix <- college_matrix[, college_ok]
	# new college_name
	college_name   <- college_name[college_ok]
	# new n_slot
	n_slot         <- n_slot[college_ok]
	# -------------------------------------------------------------------------
	# college preference matrix
	college_preference <- matrix(
		rep(0, n_student * n_college), n_student, n_college
	)
	for( j in seq(n_college) )
	{	for(i in seq(n_student) )
		{	name  <- college_matrix[i+1,j]
			if( empty_cell(name) )
				college_preference[i,j] <- NA
			else
			{	name <- as.character(name)
				index <- which( student_name == name )
				if( length(index) > 1 )
				{	stop("program error")
				}
				if( length(index) == 0 )
				{	fmt  <- "college file, row %d, column '%s', name ='%s'"
					msg1 <- sprintf(fmt, i+2, college_name[j], name)
					msg2 <- "is not a valid student name"
					stop( paste(msg1, msg2, sep=' ') )
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
		{	name  <- student_matrix[i,j]
			if( empty_cell(name) )
				student_preference[i,j] <- NA
			else
			{	name  <- as.character(name)
				index <- which( college_name == name )
				if( length(index) > 1 )
				{	stop("program error")
				}
				if( length(index) == 0 )
				{	fmt  <- "student file, row %d, column '%s', name ='%s'"
					msg1 <- sprintf(fmt, i+1, student_name[j], name)
					msg2 <- "is not a valid college name"
					stop( paste(msg1, msg2, sep=' ') )
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
		seed      = 123,
		s.range   = NULL,
		c.range   = NULL
	)
	# -------------------------------------------------------------------------
	# match_file
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
	match_data_frame <- data.frame(student, college)
	match_data_frame <- match_data_frame[order(student),]
	write.csv(
		match_data_frame, file = match_file, quote = FALSE, row.names = FALSE
	)
	# -------------------------------------------------------------------------
	return
}
