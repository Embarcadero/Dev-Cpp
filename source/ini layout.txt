dev-c v4 project.ini

[PROJECT]
FileName=<string>
Name=<string>
UnitCount=<integer>
IsDLL=<boolean>
ObjFile=<string>
ResFiles=<string>
CompilerOptions=<string>
IncludeDirs=<string>
Use_gpp=<boolean>
NoConsole=<boolean>
Icon=<string>

[Unitx] (x=0-UnitCount)
FileName=<string>
FileTime=<integer>

dev-c v5 project.ini
[PROJECT]
FileName=<relative string>		; should remove seems rather redundent
Type=<integer>			 
Ver=<integer>			 
ObjFiles=<string> 		; not needed
Include=<; sep list>	
CompilerOptions=<string>
LinkerOptions=<string>
Libs<; sep list>
Resources=<; sep list>
IsCpp=<boolean>
Icon=<relative string>

[Unitx] (x=0-unitcount)
FileName=<relative string>