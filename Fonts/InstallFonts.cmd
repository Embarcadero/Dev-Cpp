echo Should be run as Administrator.
copy "Heebo-Regular.ttf" "%SYSTEMROOT%\Fonts"
reg add "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts" /v "Heebo Regular (TrueType)" /t REG_SZ /d Heebo-Regular.ttf /f
copy "SourceCodePro-Regular.ttf" "%SYSTEMROOT%\Fonts"
reg add "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts" /v "Source Code Pro (TrueType)" /t REG_SZ /d SourceCodePro-Regular.ttf /f