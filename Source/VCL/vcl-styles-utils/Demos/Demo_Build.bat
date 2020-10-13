@SET ORGPATH=%PATH%

IF %1% LEQ 13 GOTO RADSTUDIO
GOTO STUDIO

:RADSTUDIO
IF EXIST "c:\Program Files (x86)\Embarcadero\RAD Studio\%1.0\bin\rsvars.bat" GOTO INITRADSTUDIO 
ECHO ...\Embarcadero\RAD Studio\%1.0\bin\rsvars.bat was not found.
GOTO DONE
  
:STUDIO  
IF EXIST "c:\Program Files (x86)\Embarcadero\Studio\%1.0\bin\rsvars.bat" GOTO INITSTUDIO 
ECHO ...\Embarcadero\Studio\%1.0\bin\rsvars.bat was not found.
GOTO DONE

:INITRADSTUDIO
call "%c:\Program Files (x86)\Embarcadero\RAD Studio\%1.0\bin\rsvars.bat" 	
GOTO INIT

:INITSTUDIO
call "c:\Program Files (x86)\Embarcadero\Studio\%1.0\bin\rsvars.bat" 	
GOTO INIT


:INIT
msbuild.exe "Vcl Styles Basic Demo (Demo App)\Demo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 1
pause
EXIT

:1
msbuild.exe "Vcl Styles Color Hook Form (Demo App)\VCLStylesSample.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 2
pause
EXIT

:2
msbuild.exe "Vcl Styles Color Tabs (Demo App)\ColorTabs.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 3
pause
EXIT

:3
msbuild.exe "Vcl Styles Custom ActionMenu (Demo App)\DemoActionMenu.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 4
pause
EXIT

:4
msbuild.exe "Vcl Styles Hooks (Demo App)\Demo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 5
pause
EXIT

:5
msbuild.exe "Vcl Styles Hooks Colors (Demo App)\Demo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 6
pause
EXIT

:6
msbuild.exe "Vcl Styles Owner draw (Demo App)\VclStylesOwnerDraw.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 7
pause
EXIT

:7
msbuild.exe "Vcl Styles Preview (Demo App)\VCLStylePreview.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 8
pause
EXIT

:8
msbuild.exe "Vcl Styles TWebBrowser (Demo App)\VclStylesTWB.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 9
pause
EXIT

:9
msbuild.exe "Vcl Styles Utils Addtional Dialogs (Demo App)\Dialogs2.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 10
pause
EXIT

:10
msbuild.exe "Vcl Styles Utils Custom SysBtnFontColor (Demo App)\CustomSysBtnFontColor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 11
pause
EXIT

:11
msbuild.exe "Vcl Styles Utils CustomDraw PopupMenu (Demo App)\CustomDrawPopupMenu.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 12
pause
EXIT

:12
msbuild.exe "Vcl Styles Utils NC (Demo App)\Demo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 13
pause
EXIT

:13
msbuild.exe "Vcl Styles Utils SysControls (Demo App)\ThemedSysControls.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 14
pause
EXIT

:14
msbuild.exe "Vcl Styles Utils System Menu (Demo App)\Demo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 15
pause
EXIT

:15
msbuild.exe "Vcl Styles Utils TTaskDialog (Demo App)\TTaskDialogsDemo.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO 16
pause
EXIT

:16
msbuild.exe "Vcl Styles Utils WinApi Window (Demo App)\CustomSkinedWindow.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=debug
set BUILD_STATUS=%ERRORLEVEL%
if %BUILD_STATUS%==0 GOTO DONE
pause
EXIT


:DONE
@SET PATH=%ORGPATH%

REM pause