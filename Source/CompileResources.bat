@rem Run from a RAD Studio command prompt (brcc32 on the PATH).
cd /d "%~dp0"
brcc32 DefaultFiles.rc
brcc32 Icons.rc
brcc32 VCL\vcl-styles-utils\Common\AwesomeFont.RC -foVCL\vcl-styles-utils\Common\AwesomeFont.res
pause
