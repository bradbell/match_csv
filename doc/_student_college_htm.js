var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_up0 = [
'match_csv.htm',
'student_college.htm'
];
var list_down1 = [
'student_college.htm'
];
var list_down0 = [
'get_started.r.htm',
'bad_student.r.htm',
'bad_college.r.htm'
];
var list_current0 = [
'student_college.htm#Syntax',
'student_college.htm#Description',
'student_college.htm#Conventions',
'student_college.htm#n_student',
'student_college.htm#n_college',
'student_college.htm#student_file',
'student_college.htm#college_file',
'student_college.htm#match_file',
'student_college.htm#Example',
'student_college.htm#Contents'
];
function choose_across0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_across0[index-1];
}
function choose_up0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_up0[index-1];
}
function choose_down1(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down1[index-1];
}
function choose_down0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down0[index-1];
}
function choose_current0(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_current0[index-1];
}
