1. Compiling devcpp.exe

To build devcpp a recent Delphi version is required (10.4 and onwards).

The main executable devcpp.exe can be built using the following steps:

	1) Install
		VCL Style - Windows10BlueWhale from GetIt in the IDE.
		https://getitnow.embarcadero.com/VCLStyle-Windows10BlueWhale-1.0/
	1) Compile and install the following package(s):
		Source\VCL\DevCpp.dpk
		Source\VCL\SynEdit\Packages\Sydney\Delphi\SynEditDelphi.groupproj
		Source\VCL\ClassBrowsing\ClassBrowsing.dpk
		Source\VCL\SVGIconImageList\Packages\D10_4\SVGIconImageListGroupPackages.groupproj

	2) Compile resources by running the following script(s):
		Source\CompileResources.bat
		
	3) Open the project file devcpp.dpr. Your IDE should not produce any 
	   'Module Not Found' or 'Resource Not Found' errors when opening files, 
	   compiling files or running devcpp.exe.

	4) It is advisable to run CleanSource.bat before commiting work, when you
	   get errors complaining about old versions of code or when you want to 
	   diff the source folder.

2. Compiling associated tools

There are a couple of executables that need to be compiled and/or put in the
right folder when building a full release. The source code of these files can be
found in the Source\Tools subfolder. Precompiled versions are available in the
Tools folder in the SourceForge repository. These three associated are required
to be present by the setup scripts:

	1) ConsolePauser.exe. This needs to be put in the root directory next to
	   devcpp.exe. This executable is launched by devcpp.exe when a console 
	   program is run and the option "Pause console programs after return" is
	   enabled in Environment Options. This file can be compiled using
	   Source\Tools\ConsolePauser\ConsolePauser.dev
	   
	2) devcppPortable.exe. This file should be provided with all builds of
	   Dev-C++ (also the nonportable ones) and should also be placed in the root
	   directory. Launching this executable will run devcpp.exe and tell it to
	   store configuration files in the program directory instead of in
	   %APPDATA%\Dev-Cpp. This file can be compiled using
	   Source\Tools\DevCppPortable\DevCppPortable.dev
	   
	3) Packman.exe (legacy). This file is launched by the menu item located at
	   Tools >> Package Manager and provides .pak plugin support. This file has
	   not been touched since like 2005 so do what you wish with it.
	   
3. Other tools

Other tools provided for legacy reasons or to easy the developer's life are:

	1) GitPush. Automated script that adds, commits, and pushes the current code
	   base to the SourceForge git repo. Please position this executable in the
	   root folder of Dev-C++.
	   
	2) HeywordHighlighter. Used to generate the hash table of function pointers
	   used by SynEditHighlighterCpp.pas to determine if a word is a keyword.
	   
	3) LangCheck. Compares any language file to the english language file and
	   checks for missing entries and formatting options compatibility.
	   
	4) PackMaker (legacy). Used to create PAK extension files.
	   
4. UPX

To decrease the main executable size, the old developers from Bloodshed used a
program called UPX to compress it. Here is a copy of their instructions on how
to use it (I don't, since the mere megabyte you save in size pales to the 330MB
of the provided compiler):

	When compressing with upx (Ultimate Packer for Executables) use:
	
	upx -9 --compress-icons=0 devcpp.exe
	
	Otherwise upx will compress all icons and the file associations will point
	to nonexisting (moved) icons within devcpp.exe
	
5. Shortcut Map

This is an up to date map of the default shortcuts of Dev-C++. All Ctrl+(Letter)
combinations are in use, except for the following letters: UJKL. The Shift 
modifier should be added to a shortcut to signify a multiplicated version
of the regular shortcut. The use of Ctrl+Alt+(Key) or Shift+(Key) should be
avoided.

Misc.
-----
F1			Help
Alt+F4		Close
Ctrl+Space	Open Code Completion

Executing/Debugging
-------------------
F2			Goto Breakpoint
F4			Toggle Breakpoint
F5			Start Debug
F6			Stop Debug/Regular
F7			Next Line
F8			Step Line
F9			Compile
F10			Start Regular
F11			Compile and Run Regular
F12			Rebuild
Ctrl+F9		Syntax Check Current File

Editing
-------
Ctrl+Z			Undo
Ctrl+Y			Redo
Ctrl+X			Cut
Ctrl+C			Copy
Ctrl+V			Paste
Ctrl+A			Select All
Ctrl+Q			Swap Header/Source
Ctrl+.			Comment
Ctrl+,			Uncomment
Ctrl+/			Toggle Comment
Ctrl+;			Toggle Inline Comment
Ctrl+T			Add TODO
Ctrl+D			Delete Line
Ctrl+E			Duplicate Line
Tab				Indent
Shift+Tab 		Unindent
Shift+Ctrl+Up	Move Selection Up
Shift+Ctrl+Down	Move Selection Down
Shift+Ctrl+A	Format Current File

Searching
---------
Ctrl+F			Find
Shift+Ctrl+F	Find in Files
Ctrl+R			Replace
Shift+Ctrl+R	Replace in Files
Ctrl+G			Goto Line
Shift+Ctrl+G	Goto Function
Ctrl+I			Incremental Search
Ctrl+M			Swap Editor
F3				Search Again
Shift+F3		Reverse Search Again

Files
-----
Ctrl+N			New File
Ctrl+O			Open File
Ctrl+S			Save File
Shift+Ctrl+S	Save All Files
Ctrl+W			Close File
Shift+Ctrl+W	Close All Files
Ctrl+P			Print File
Ctrl+B			Open Containing Folder

View
----
Ctrl+F11		Toggle Fullscreen
Ctrl+F12		Toggle Fullscreen Bar
Ctrl+Tab		Next Editor
Shift+Ctrl+Tab	Previous Editor

Project
-------
Ctrl+H			Project Options
	
7. Image Map (somewhat outdated)

These are the descriptions of the indices of the menu images lists used in d
Transparent color is selected as the left most pixel of the bottom row.

Menus:
0 = New Project
1 = New Source File/Project New Source File
2 = New Resource/Project Edit Resource/Resource Sheet
3 = New Template
4 = Open
5 = Clear History/Remove Watch
6 = Save File
7 = Save As
8 = Save All
9 = Close File/Close Sheet
10 = Print
11 = Exit
13 = Undo
14 = Redo/Step Over
15 = Cut
16 = Copy
17 = Paste
18 = Insert(edit Menu)/Next Step
19 = Toggle Bookmark
20 = Goto Bookmark
21 = Find/Add Watch/Find Sheet
22 = Replace
23 = Find Next
24 = Goto Line
25 = Project Add File
26 = Project Remove File
27 = Project Options
28 = Compile/Compile Sheet
30 = Rebuild
31 = Run
32 = Debug/Debug Sheet
33 = Compile and Run
34 = Compiler Options/Export (submenu)
35 = Environment Options
36 = Editor Options/Edit Watch
37 = Configure Tools
38 = Full Screen
39 = Next Editor
40 = Previous Editor
41 = Update Check
42 = About
43 = Log Sheet
44 = Toolbars Menu
45 = Full Screen Mode
46 = Help Toolbar Button
47 = Delete Profiling Information
48 = Package Manager
49 = Syntax Check
50 = Close All
51 = Class Browser Class/Struct
52 = Class Browser Method
53 = Class Browser Variable


Gutter Images
0 = Breakpoint
1 = Active Breakpoint
2 = Invalid Breakpoint
3 = ???

Class Browser Images:
0 = Folder
1 = Classes
2 = Private Variables
3 = Protected Variables
4 = Public Variables
5 = Private Methods
6 = Protected Methods
7 = Public Methods
8 = Protected Inherited Methods ???
9 = Public Inherited Methods ???
10 = Protected Inherited Variables ???
11 = Public Inherited Variables ???

Project Images
0 = Root Node
1 = File Node
2 = Run
3 = Recycle Bin ???
4 = Folder Node
5 = Hamburger Icon ???
