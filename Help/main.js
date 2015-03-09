var ShowButton = "[+] ";
var HideButton = "[-] ";

var TableOfContents = [
	"Functional Help",[
		{text:"Editing",href:"./Subjects/Editing.htm"},
		{text:"Compiling",href:"./Subjects/Compiling.htm"},
		{text:"Debugging",href:"./Subjects/Debugging.htm"},
		{text:"Profiling",href:"./Subjects/Profiling.htm"},
	],
	"Interface Help",[
		"Dialog Windows",[
			{text:"Compiler Options",href:"./Interface/Dialog Windows/Compiler Options/index.htm"},[
				{text:"General",href:"./Interface/Dialog Windows/Compiler Options/General.htm"},
				{text:"Settings",href:"./Interface/Dialog Windows/Compiler Options/Settings.htm"},
				{text:"Directories",href:"./Interface/Dialog Windows/Compiler Options/Directories.htm"},
				{text:"Programs",href:"./Interface/Dialog Windows/Compiler Options/Programs.htm"},
				{text:"Makefile",href:"./Interface/Dialog Windows/Compiler Options/Makefile.htm"},
			],
			{text:"Profile Analysis",href:"./Interface/Dialog Windows/Profile Analysis/index.htm"},[
				{text:"Flat Output",href:"./Interface/Dialog Windows/Profile Analysis/Flat Output.htm"},
				{text:"Call Graph",href:"./Interface/Dialog Windows/Profile Analysis/Call Graph.htm"},
				{text:"Profiling Options",href:"./Interface/Dialog Windows/Profile Analysis/Profiling Options.htm"},
			],
			"Project Options",[
				{text:"General",href:"./Interface/Dialog Windows/Project Options/General.htm"},
				{text:"Files",href:"./Interface/Dialog Windows/Project Options/Files.htm"},
				{text:"Compiler",href:"./Interface/Dialog Windows/Project Options/Compiler.htm"},
				{text:"Parameters",href:"./Interface/Dialog Windows/Project Options/Parameters.htm"},
				{text:"Directories",href:"./Interface/Dialog Windows/Project Options/Directories.htm"},
				{text:"Build Options",href:"./Interface/Dialog Windows/Project Options/Build Options.htm"},
				{text:"Makefile",href:"./Interface/Dialog Windows/Project Options/Makefile.htm"},
				{text:"Version Info",href:"./Interface/Dialog Windows/Project Options/Version Info.htm"},
			],
		],
		"Menus",[
			{text:"File",href:"./Interface/Menus/File.htm"},
			{text:"Edit",href:"./Interface/Menus/Edit.htm"},
			{text:"Search",href:"./Interface/Menus/Search.htm"},
			{text:"View",href:"./Interface/Menus/View.htm"},
			{text:"Project",href:"./Interface/Menus/Project.htm"},
		]
	],
	"Frequenty Asked Questions",[
		{text:"Compiler",href:"./FAQ/Compiler.htm"},
		{text:"Editor",href:"./FAQ/Editor.htm"},
		{text:"Environment",href:"./FAQ/Environment.htm"},
		{text:"Versions",href:"./FAQ/Versions.htm"},
	]
];

function OnTableOfContentsLinkClick(e) {
	var event = GetEvent(e); // cross browser meuk
	
	// Don't follow link
	event.preventDefault();
	
	// Load iframe
	$("#Content").attr("src",event.target.href);
	
	// Set caption
	document.title = event.target.text;
}

function OnHiderClick(e) {
	var event = GetEvent(e); // cross browser meuk
	var target = GetTarget(event); // idem
	var parent = target.parentElement;
	var parentsublist = parent.nextElementSibling;
	if(target.innerHTML == ShowButton) {
		target.innerHTML = HideButton;
		parentsublist.style.display = "";
	} else {
		target.innerHTML = ShowButton;
		parentsublist.style.display = "none";
	}
}

function CreateTableOfContentsRecurse(array) {
	var result = "<ul>";
	for(var i = 0;i < array.length;i++) {
		if(array[i] instanceof Array) {
			// add folding mark inside <li>Caption</li>
			var index = result.lastIndexOf("<li>");
			
			// this li will not have a CSS based bullet point
			result = result.substr(0,index+3) + " class='ChildLi'" + result.substr(index+3);
			
			// repeat search for last "<li "
			var newopeningtag = "<li class='ChildLi'>";
			index = result.lastIndexOf(newopeningtag);

			// Add folder before caption
			result = result.substr(0,index + newopeningtag.length) + "<span class='Hider' onclick='OnHiderClick(event)'>" + HideButton + "</span>" + result.substr(index + newopeningtag.length);
			
			// Insert children elements
			result += CreateTableOfContentsRecurse(array[i]);
		} else if(array[i] instanceof Object) { // assume a link
			result += "<li><a class='TableOfContentsLink' href='" + array[i].href + "'>" + array[i].text + "</a></li>";
		} else {
			result += "<li>" + array[i] + "</li>"; // assume string
		}
	}
	result += "</ul>";
	return result;
}

window.onload = function() {
	// Fill table of contents
	var html = CreateTableOfContentsRecurse(TableOfContents);
	$("#TableOfContents").html(html);
	
	// Redirect table of contents links
	$('.TableOfContentsLink').click(OnTableOfContentsLinkClick);
	
	// Redirect content links
	if(window.attachEvent) {
		window.attachEvent("onmessage",OnMessage); // Internet Explorer
	} else {
		window.addEventListener("message", OnMessage, false); // Opera/Mozilla/Webkit
	}
}

// handy function for end users
function OpenPage(caption,path) {
	PostParentMessage(caption + '||' + path);
}

function PostParentMessage(text) {
	window.parent.postMessage(text,'*');
}

// Handle content links here
function OnMessage(e) {
	var parts = e.data.split('||');
	if(parts.length == 2) {
		// Set caption
		document.title = parts[0];
		
		// Load iframe
		$("#Content").attr("src",parts[1]);
	}
}

function GetEvent(e) {
	if(e !== undefined) {
		return e;
	} else if(window.event !== undefined) {
		return window.event;
	} else {
		return null;
	}
}
function GetTarget(e) {
	if(e.target !== undefined) {
		return e.target;
	} else if(e.srcElement !== undefined) {
		return e.srcElement;
	} else {
		return null;
	}
}
function GetEventTarget(e) {
	return GetTarget(GetEvent(e));
}