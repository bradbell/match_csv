var list_across0 = [
'_contents.htm',
'_reference.htm',
'_index.htm',
'_search.htm',
'_external.htm'
];
var list_up0 = [
'match_csv.htm',
'student_college.htm',
'get_started.r.htm'
];
var list_down2 = [
'student_college.htm',
'whats_new.htm'
];
var list_down1 = [
'get_started.r.htm',
'bad_student.r.htm',
'bad_college.r.htm',
'stable_marriage.r.htm',
'fewer_rows.r.htm',
'empty_cells.r.htm',
'empty_column.r.htm'
];
var list_current0 = [
'get_started.r.htm#Run This Example',
'get_started.r.htm#Load The student_college Routine',
'get_started.r.htm#Create The Student File',
'get_started.r.htm#Create the College File',
'get_started.r.htm#Run student_college',
'get_started.r.htm#Check Match',
'get_started.r.htm#Check Match.Read Match File',
'get_started.r.htm#Check Match.Extract Optimal Match',
'get_started.r.htm#Check Match.Check Optimal Match',
'get_started.r.htm#Report Check Result'
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
function choose_down2(item)
{	var index          = item.selectedIndex;
	item.selectedIndex = 0;
	if(index > 0)
		document.location = list_down2[index-1];
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
