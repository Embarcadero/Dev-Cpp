@echo off
rem Sets up Dev-C++ development with DPM. See dpm-setup.ps1 and BUILDING-DPM.md.
rem Usage: dpm-setup.cmd [-Delphi 11.0^|12.0^|13.0] [-Feed folder] [-Clean] [-CheckOnly]
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0dpm-setup.ps1" %*
exit /b %ERRORLEVEL%
