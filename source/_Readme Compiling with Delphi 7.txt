1) Install the following packages by double clicking them and clicking on compile and/or install:

.\Vcl\DevCpp.dpk
.\Vcl\SynEditPackages\SynEdit_D7.dpk
.\Vcl\Class Browser & Completion\ClassBrowsing.dpk

2) Compile resources: run .\CompileResources.bat to create binary .res files or run manually from cmd:

brcc32 DefaultFiles.rc
brcc32 LangFrm.rc
brcc32 icons.rc

3) Opening and using the main project file 'devcpp.dpr' should not bring up any 'not found' erros now.

4) Other versions of Delphi should work, but I've only tested Delphi 7.