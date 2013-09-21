to compile with Delphi 7:

1) add the following packages to Delphi:
DevCpp.dpk
SynEdit_D7.dpk
ClassBrowsing.dpk

2) compile resources manually. run cmd (Delphi adds brcc32 to global definitions):
brcc32 DefaultFiles.rc
brcc32 webupdate\selfupdater.rc
brcc32 LangFrm.rc
brcc32 icons.rc

otherwise you'll get the following errors:
[Error] File not found: 'DefaultFiles.res'
[Error] File not found: 'webupdate\selfupdater.res'
[Error] File not found: 'LangFrm.res'
[Error] File not found: 'Icons.res'
