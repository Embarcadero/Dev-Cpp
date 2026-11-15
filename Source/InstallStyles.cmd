@echo off
rem Installs the VCL styles devcpp.dproj embeds, from GetIt. See InstallStyles.ps1.
rem Usage: InstallStyles.cmd [-Delphi 10.4^|11.0^|12.0^|13.0] [-AcceptEulas] [-CheckOnly]
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0InstallStyles.ps1" %*
exit /b %ERRORLEVEL%
