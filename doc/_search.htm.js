// ------------------------------------------------------------ 
// Copyright (C) Bradley M. Bell 1998-2015, All rights reserved 
// ------------------------------------------------------------ 
Keyword = 
[
'match_csv  match_csv-20190127: Csv File Interface to Matching Problem Solvers  ',' ',
'student_college  Student / College Matching Problem  ',' ',
'get_started.R  Get Starting Using student_college  ',' ',
'bad_student.R  Example: Student Thinks to Highly of It\'s Self  ',' ',
'bad_college.R  Example: College Thinks To Highly of It\'s Self  ',' ',
'stable_marriage.R  Stable Marriage Problem With Three Solutions  ',' ',
'fewer_rows.R  Example With Fewer Rows than Possible in Data Files  ',' ',
'empty_cells.R  Example With Empty Cells in Data Files  ',' ',
'whats_new  Changes in Reverse Order by Date  ',' '
]

var MaxList = 100;
var Nstring = -1;
var Nkeyword = Keyword.length / 2;
Initialize();

function Initialize()
{
	UpdateList();
	document.search.keywords.focus();
}
function UpdateList(event)
{
	key = 0;
	if( window.event )
		key = window.event.keyCode;
	else if( event )
		key = event.which;
	if( key == 13 )
	{	Goto();
		return;
	}
	var string  = document.search.keywords.value;
	if( Nstring == string.length )
		return;
	Nstring     = string.length;

	var word    = string.match(/\S+/g);
	var nword   = 0;
	if(word != null )
		nword   = word.length;

	var pattern = new Array(nword);
	for(var j = 0; j < nword; j++)
		pattern[j] = new RegExp(word[j], 'i');

	var nlist = 0;
	var list  = '';
	for(i = 0; (i < Nkeyword) && (nlist < MaxList) ; i++)
	{
		var match = true;
		for(j = 0; j < nword; j++)
		{	var flag = pattern[j].test(Keyword[2*i]);
			flag     = flag || pattern[j].test(Keyword[2*i+1]);
			match    = match && flag;
		}

		if( match )
		{
			line  = Keyword[2*i].split(/\s+/);
			line  = line.join(' ');
			list  = list + line + '\n';
			nlist = nlist + 1;
		}
	}
	document.search.list.value    = list;
}
function Choose(textarea)
{	var start_select = textarea.value.substring(0, textarea.selectionStart);
	var start_pos    = Math.max(0, start_select.lastIndexOf('\n') );
	var length       = textarea.value.length;
	var end_select   = 
		textarea.value.substring(textarea.selectionEnd, length);
	var end_pos      = end_select.indexOf('\n');
	if( end_pos >= 0 ) 
	{	end_pos = textarea.selectionEnd + end_pos;
	} else 
	{	end_pos = length;
	}
	// highlight the selected line
	textarea.selectionStart = start_pos;
	textarea.selectionEnd   = end_pos;
	// get the choice from the beginning of the line
	var line = textarea.value.substring(start_pos, length);
	var end_choice = line.indexOf(' ');
	if( end_choice >= 0 )
	{	var choice = line.substring(0, end_choice);
		document.search.choice.value = choice.toLowerCase();
	}
	
	return true;
}
function Goto()
{
parent.location = document.search.choice.value + '.htm';
}
