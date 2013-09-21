{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit debugreader;

interface

uses 
{$IFDEF WIN32}
  Classes, Windows, ShellAPI, Dialogs, SysUtils, version;
{$ENDIF}
{$IFDEF LINUX}
  Classes, QDialogs, SysUtils, version;
{$ENDIF}

type
  TDebugReader = class(TThread)
  public
    hPipeRead  : THandle;
    EventReady : THandle;
    Output     : string;
    Idling    : boolean;

  protected
    procedure Execute; override;

  end;

implementation

procedure TDebugReader.Execute;
var
  lpBuffer : array [0..256] of char;
  nBytesRead : DWORD;
  _output : string;
begin
  _output := '';
  while true do begin
    FillChar(lpBuffer, sizeof(lpBuffer), 0);
    if (not ReadFile(hPipeRead, lpBuffer, sizeof(lpBuffer),
        nBytesRead, nil) or (nBytesRead = 0)) then begin
      if (GetLastError() = ERROR_BROKEN_PIPE) then
        break // pipe done - normal exit path.
      else
        break; // Something bad happened.
    end;
    _output := _output + string(lpBuffer);
    if pos(GDB_PROMPT, _output) <> 0 then begin
      SetEvent(EventReady);
      Output := _output;
      _output := '';
      Idling := true;
    end;
  end;
end;

end.
