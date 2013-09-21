program SelfUpdater;

{
	(c)2002.
	This program was created by Yiannis An. Mandravellos (mandrav@supergoal.gr)
	You are free to use it in commercial and non-commercial projects, if you 
	find it interesting. Just send me a mail that you 're using it. I 'd like 
	to know where it's used ;)

	You must retain this text in this file and you may not alter it.
	I TAKE NO RESPONSIBILITY FOR ANY LOSS OR DAMAGE BY THE USE OF THIS PROGRAM.
	USE IT ON YOUR OWN.
}

uses Windows, ShellAPI;

// ParamCount = Nr of parameters
// ParamStr(0) = exe name
// ParamStr(1..ParamCount) = parameters
//
// Usage: SelfUpdater.exe <from_filename> <to_filename> [<app_title> <success_msg> <error_msg>]

{.$DEFINE USE_SHELLAPI}
{.$DEFINE DEBUG}

const
  TIMEOUT = 5000; // milliseconds

var
{$IFDEF USE_SHELLAPI}
  P: TSHFileOpStruct;
{$ENDIF}
  AppTitle: string;
  Success: string;
  Error: string;
{$IFDEF DEBUG}
  S: string;
  I: integer;
{$ENDIF}
  OK : boolean;
begin
{$IFDEF DEBUG}
  S := '';
  for I := 0 to ParamCount do
    S := S + ParamStr(I) + #13;
  MessageBox(0, PChar(S), 'debug', MB_OK or MB_ICONINFORMATION);
{$ENDIF}
  Success := '';
  Error := '';
  AppTitle := '';
  OK := true;
  if ParamCount >= 3 then
    AppTitle := ParamStr(3);
  if ParamCount >= 4 then
    Success := ParamStr(4);
  if ParamCount >= 5 then
    Error := ParamStr(5);
  if ParamCount >= 2 then begin
{$IFDEF USE_SHELLAPI}
    P.wFunc := FO_COPY;
    P.pFrom := PChar(ParamStr(1));
    P.pTo := PChar(ParamStr(2));
    P.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    P.fAnyOperationsAborted := False;
    P.hNameMappings := nil;
    P.lpszProgressTitle := nil;

    while (SHFileOperation(P) <> 0) do begin
{$ELSE}

    while (not CopyFile(PChar(ParamStr(1)), PChar(ParamStr(2)), False)) do begin
{$ENDIF}
      if MessageBox(0, 'Dev-C++ is currently in use, please close it now and click Retry to proceed', PChar(AppTitle), MB_RETRYCANCEL or MB_ICONINFORMATION) <> IDRETRY then begin
        OK := false;
        break;
      end;
      Sleep(250);
    end;

    if OK then begin
      if Success <> '' then
        MessageBox(0, PChar(Success), PChar(AppTitle), MB_OK or MB_ICONINFORMATION);
      DeleteFile(PChar(ParamStr(1)));
      ShellExecute(0, 'open', PChar(ParamStr(2)), nil, nil, SW_SHOWNORMAL);
    end
    else begin
      if Error <> '' then
        MessageBox(0, PChar(Error), PChar(AppTitle), MB_OK or MB_ICONERROR);
    end;
  end;
end.

