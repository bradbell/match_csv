# -----------------------------------------------------------------------------
#        match_csv: Csv File Interface to Matching Problem Solvers
#         Copyright (C) 2018 Bradley M. Bell (bradbell@seanet.com)
#            This program is distributed under the terms of the
#            GNU General Public License version 3.0 or later see
#                 https://www.gnu.org/licenses/gpl-3.0.txt
# -----------------------------------------------------------------------------
# $begin student_college$$ $newlinech #$$
# $spell
#	nrow
#	csv
#	matchid
# $$
#
# $section Student / College Matching Problem$$
#
# $head Syntax$$
# $codei%student_college(%student_file%, %college_file%, %match_file%)
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
# with $icode n_student$$ columns at most
# $icode%n_college%+1%$$ rows.
# The first row contains the names of the students.
# The other cells in each column are the names of the
# acceptable colleges for the student name in the first row of the column.
# The lower the row index, the more preferred the college is to the student.
# No college name appears twice in a column.
# For each student (column), empty cells correspond to
# colleges that are acceptable to the student.
#
# $head college_file$$
# This is a csv input file with $icode n_college$$ columns and
# at most $icode n_student+2$$ rows.
# The first row contains the names of the colleges.
# Each column of the second row contains a positive integer that specifies
# the number of students positions the corresponding
# college has available.
# The other cells in each column are the names of the
# acceptable students for the college name in the first row of the column.
# The lower the row index, the more preferred the student is to the college.
# No student name appears twice in a column.
# For each college (column), empty cells correspond to
# students that are acceptable to the college.
#
# $head match_file$$
# This is a csv output file where each row represents a pairing
# of a student with a college. It has the columns below and is sorted
# first by $icode matchid$$ and second by $icode student$$.
#
# $subhead student$$
# The column contains the name of the student for this pairing.
#
# $subhead college$$
# This column contains the name of the college for this pairing.
#
# $subhead matchid$$
# This column contains an integer, starting with one and incrementing by one,
# that identifies a set of pairs having the same $icode match$$ value.
# Each set of pairs constitutes a different stable matching.
#
# $subhead optimal$$
# If a cell in this column contains the single character or is empty.
# If the character is $code s$$, this match is student optimal.
# If it is $code c$$, this match is college optimal.
# If it is $code b$$, this match is both student and college optimal.
# Otherwise, cell is empty.
#
# $head Example$$
# $childtable%
#	example/student_college/get_started.R%
#	example/student_college/bad_student.R%
#	example/student_college/bad_college.R%
#	example/student_college/stable_marriage.R%
#	example/student_college/fewer_rows.R%
#	example/student_college/empty_cells.R
# %$$
#
# $end
# -----------------------------------------------------------------------------
empty <- function(x)
{	return( is.na(x) | x == "" )
}
# A duplicate is two non-empty entires in the same column that are equal.
# An empty return means no duplicates are found. Otherwise:
#	result["column"] is the column where the duplicate was found
#	result["first"] is one row index of the duplicate.
#	result["second"] is the other row index of the duplicate.
check4duplicates <- function(mat)
{	result = c()
	for( j in seq( ncol(mat) ) )
	{	column <- mat[,j]
		check  <- column[ ! empty(column) ]
		for( i in seq( length(check) ) )
		{	name <- as.character( check[i] )
			found <- which( name == column )
			count <- length(found)
			if( count != 1 )
			{	result["column"] = j
				result["first"]  = found[1]
				result["second"] = found[2]
				return( result )
			}
		}
	}
	return( result )
}
student_college <- function(student_file, college_file, match_file)
{
	# load matchingMarketspackages::hri
	requireNamespace("matchingMarkets")
	#
	# Debugging verison of hri (so can put browser() commands in it):
	# library(rJava)
	# source('/home/bradbell/repo/matching_markets.git/R/hri.R')
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
	# number of students and colleges
	n_student       <- student_size[2]  # number of columns in student file
	n_college       <- college_size[2]  # number of columns in college file
	#
	# maximum number of choices by students, colleges
	m_college <- student_size[1]
	m_student <- college_size[1]  - 1
	#
	if( m_college > n_college )
	{	fmt  <- "number of rows in student_file = %d"
		msg1 <- sprintf(fmt, student_size[1] + 1)
		fmt  <- "is greater than n_college + 1 = %d"
		msg2 <- sprintf(fmt, n_college + 1 )
		stop( paste(msg1, msg2, sep=', ') )
	}
	#
	if( m_student >  n_student )
	{	fmt  <- "number of rows in college_file = %d"
		msg1 <- sprintf(fmt, college_size[1] + 1)
		fmt  <- "is greater than n_student + 2 = %d"
		msg2 <- sprintf(fmt, n_student + 2 )
		stop( paste(msg1, msg2, sep=', ') )
	}
	# -------------------------------------------------------------------------
	# names
	student_name <- colnames(student_data_frame)
	college_name <- colnames(college_data_frame)
	# -------------------------------------------------------------------------
	# matrices (not including first row which contained names)
	student_matrix <- as.matrix( student_data_frame )
	college_matrix <- as.matrix( college_data_frame )
	# -------------------------------------------------------------------------
	# Number of slots for each college is currently first row of college_matrix.
	# Move it to n_slot while converting it to integer.
	n_slot <- rep(0, n_college)
	for ( j in seq(n_college) )
		n_slot[j]      <- as.integer( college_matrix[1, j] )
	#
	# remove row 1 from college_matrix (no longer needed)
	college_matrix <- college_matrix[-1,]
	# -------------------------------------------------------------------------
	# check no duplicates in list of choices for each student
	result <- check4duplicates(student_matrix)
	if( length(result) > 0 )
	{	j   <- result["column"]
		i1  <- result["first"]
		i2  <- result["second"]
		fmt <- "row %d and row %d in column '%s' of student file are equal"
		stop( sprintf(fmt, i1+1, i2+1, student_name[j]) )
	}
	# check not duplicates in list of choices for each college
	result <- check4duplicates(college_matrix)
	if( length(result) > 0 )
	{	j   <- result["column"]
		i1  <- result["first"]
		i2  <- result["second"]
		fmt <- "row %d and row %d in column '%s' of college file are equal"
		stop( sprintf(fmt, i1+1, i2+1, college_name[j]) )
	}
	# -------------------------------------------------------------------------
	# students that have at least one possible match
	student_ok = rep(FALSE, n_student) # initilaize as false
	for( j in seq(n_student) )  # j-th student
	{	for( i in seq(m_college) )
		{	college <- student_matrix[i,j] # i-th choice for this student
			if( ! ( empty(college) || student_ok[j] ) )
			{	# acceptible students for this college
				acceptible_student = college_matrix[,college]
				if( student_name[j] %in% acceptible_student )
					student_ok[j] <- TRUE # j-th student has possible match
			}
		}
	}
	#
	# remove students that have no possible match from
	# college_matrix, student_matrix, student_name, n_student
	vec <- rep(TRUE, m_student * n_college)
	ok  <- matrix(vec, m_student, n_college)
	for( student in student_name[ ! student_ok ] )
		ok <- ok & college_matrix != student
	college_matrix[! ok] <- ""
	n_student            <- sum(student_ok)
	student_matrix       <- student_matrix[, student_ok]
	student_name         <- student_name[student_ok]
	# -------------------------------------------------------------------------
	# colleges that have at least one possible match
	college_ok = rep(FALSE, n_college)
	for( j in seq(n_college) ) # j-th college
	{	for( i in seq(m_student) )
		{	student <- college_matrix[i,j]
			if( ! ( empty(student) || college_ok[j] ) )
			{	acceptible_college = student_matrix[,student]
				if( college_name[j] %in% acceptible_college )
					college_ok[j] <- TRUE
			}
		}
	}
	#
	# remove colleges that have no possible match from
	# student_matrix, college_matrix, college_name, n_college
	vec <- rep(TRUE, m_college * n_student)
	ok  <- matrix(vec, m_college, n_student)
	for( college in college_name[ ! college_ok ] )
		ok <- ok & student_matrix != college
	student_matrix[! ok] <- ""
	n_college            <- sum(college_ok)
	college_matrix       <- college_matrix[, college_ok]
	college_name         <- college_name[college_ok]
	n_slot               <- n_slot[college_ok]
	# -------------------------------------------------------------------------
	# college preference matrix
	# is college_matrix with student names replaced by student index
	college_preference <- matrix(
		rep(NA, n_student * n_college), n_student, n_college
	)
	for( j in seq(n_college) )
	{	count <- 0
		for(i in seq(m_student) )
		{	name  <- college_matrix[i,j]
			if( ! empty(name) )
			{	count <- count + 1
				name <- as.character(name)
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
				college_preference[count,j] <- index
			}
		}
	}
	# -------------------------------------------------------------------------
	# student preference matrix
	# is student_matrx with college names replaced by college index
	student_preference <- matrix(
		rep(NA, n_college * n_student), n_college, n_student
	)
	for( j in seq(n_student) )
	{	count <- 0
		for(i in seq(m_college) )
		{	name  <- student_matrix[i,j]
			if( ! empty(name) )
			{	count <- count + 1
				name  <- as.character(name)
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
				student_preference[count,j] <- index
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
	#
	# match_data_frame
	pairing <- res["matchings"][[1]]
	n_pair  <- nrow(pairing)
	student <- c()
	college <- c()
	matchid <- c()
	optimal <- c()
	for ( i in seq(n_pair) )
	{	tmp      <- student_name[ as.integer( pairing[i, "student"] ) ]
		student  <- c(student, tmp)
		#
		tmp      <- college_name[ as.integer( pairing[i, "college"] ) ]
		college  <- c(college, tmp)
		#
		tmp      <- as.character( pairing[i, "matching"] )
		matchid  <- c(matchid, tmp)
		#
		# optimal
		s_optimal <- as.integer( pairing[i, "sOptimal"] )
		c_optimal <- as.integer( pairing[i, "cOptimal"] )
		ch        <- ""
		if( c_optimal == 1 )
			ch <- "c"
		if( s_optimal == 1 )
		{	if( c_optimal == 1 )
				ch <- "b"
		else
				ch <- "s"
		}
		optimal   <- c(optimal, ch)
	}
	match_data_frame <- data.frame(student, college, matchid, optimal)
	#
	# sort by match first and then by student
	match_data_frame <- match_data_frame[order(matchid, student),]
	#
	# write file
	write.csv(
		match_data_frame, file = match_file, quote = FALSE, row.names = FALSE
	)
	# -------------------------------------------------------------------------
	return
}
