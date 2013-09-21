to compile with Delphi 7:

1) Add the following packages to Delphi:
DevCpp.dpk
SynEdit_D7.dpk
ClassBrowsing.dpk

2) Compile resources: run CompileResources.bat or run manually from cmd:
brcc32 DefaultFiles.rc
brcc32 webupdate\selfupdater.rc
brcc32 LangFrm.rc
brcc32 icons.rc

otherwise you'll get the following errors:
[Error] File not found: 'DefaultFiles.res'
[Error] File not found: 'webupdate\selfupdater.res'
[Error] File not found: 'LangFrm.res'
[Error] File not found: 'Icons.res'

3) Opening and using the main project file devcpp.dpr should not bring up any 'not found' erros now.