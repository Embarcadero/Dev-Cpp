to compile with Delphi 7:

1) use appropriate Delphi 7 packages for SynEdit,

2) compile resources manually. do:
brcc32 DefaultFiles.rc
brcc32 webupdate\selfupdater.rc
brcc32 LangFrm.rc
brcc32 icons.rc

otherwise you'll get an errors:
[Error] File not found: 'DefaultFiles.res'
[Error] File not found: 'webupdate\selfupdater.res'
[Error] File not found: 'LangFrm.res'
[Error] File not found: 'Icons.res'
