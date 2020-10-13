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

{$D+} // debugging in this unit
{$OPTIMIZATION off } // and no optimization
unit ExceptionFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, StdCtrls, Buttons, ScktComp, ExtCtrls, ShellAPI, ComCtrls;

type
  PUnitEntry = ^TUnitEntry;
  TUnitEntry = record
    Name: String;
    Start: dword;
    Len: integer;
  end;

  PFuncsEntry = ^TFuncsEntry;
  TFuncsEntry = record
    Name: String;
    Address: dword;
  end;

  PLineEntry = ^TLineEntry;
  TLineEntry = record
    Line: String;
    Address: dword;
    UnitIndex: dword;
  end;

  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

  TExceptionFrm = class(TForm)
    lblError: TLabel;
    btnContinue: TButton;
    lblTitle: TLabel;
    lblAddressTitle: TLabel;
    lblAddress: TLabel;
    lblErrorTitle: TLabel;
    btnTerminate: TButton;
    btnSend: TButton;
    Shape1: TShape;
    Bevel2: TBevel;
    Image1: TImage;
    memBugReport: TMemo;
    memUserReport: TMemo;
    lblUpdateSuggest: TLabel;
    lblUpdateLink: TLabel;
    memEmailReport: TMemo;
    btnShowReport: TButton;
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTerminateClick(Sender: TObject);
    procedure btnContinueClick(Sender: TObject);
    procedure lblUpdateLinkClick(Sender: TObject);
    procedure memUserReportEnter(Sender: TObject);
    procedure memUserReportExit(Sender: TObject);
    procedure memEmailReportEnter(Sender: TObject);
    procedure memEmailReportExit(Sender: TObject);
    procedure btnShowReportClick(Sender: TObject);
  public
    fPlatform: String;
    fBuildTime: String;
    fOSVersion: String;
    fAdditionalInfo: String;
    fProgramPath: String;
    fComputerName: String;
    fProgramVersion: String;
    fTotalPhys: String;
    fUsedPhys: String;
    fFreePhys: String;
    fTotalCache: String;
    fUsedCache: String;
    fFreeCache: String;
    fTotalVirt: String;
    fUsedVirt: String;
    fFreeVirt: String;
    fMemoryLoad: String;
    fLines: TList;
    fUnits: TList;
    fFuncs: TList;
    function StackStop: dword;
    procedure ReadMapFile(const Fname: String);
    function AddressInfo(Address: Cardinal): String;
    procedure GatherSystemInfo; // fill interface
    function GetStackReport: String; // bug report text
    function GetMemoryReport: String; // ...
    function GetMachineReport: String;
    function GetErrorReport: String;
  end;

  TEAnalyzer = class(TPersistent)
  public
    class procedure EHandler(Sender: TObject; E: Exception);
  end;

implementation

{$R *.dfm}

uses
  utils, devcfg, version, DateUtils, System.UItypes;

const
  UserReportMsg = 'Please include a description of what you were doing before the error occurred...';
const
  EmailReportMsg =
    'Please provide an email address to which the developer can send questions about the bug report...';

class procedure TEAnalyzer.EHandler(Sender: TObject; E: Exception);
begin
  with TExceptionFrm.Create(Application.MainForm) do try

    lblAddress.Caption := Format('0x%8.8x', [dword(ExceptAddr)]);
    lblError.Caption := E.Message;

    // Can't make this much more clear
    memUserReport.Text := UserReportMsg;
    memEmailReport.Text := EmailReportMsg;

    // Include memory report too?
    memBugReport.Text :=
      GetErrorReport + #13#10#13#10#13#10 +
      GetMachineReport + #13#10#13#10#13#10 +
      GetStackReport;

    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmExceptionsAnalyzer }

function TExceptionFrm.StackStop: dword;
var
  si: TSystemInfo;
  mbi: TMemoryBasicInformation;
  pagesize: dword;
  tmpSP: dword;
begin
  GetSystemInfo(si);
  pagesize := si.dwPageSize;
  asm
    mov tmpSP,esp
  end;
  tmpSP := pagesize * (tmpSP div pagesize);
  VirtualQuery(pointer(tmpSP), mbi, sizeof(mbi));
  result := mbi.RegionSize + dword(mbi.BaseAddress);
end;

procedure TExceptionFrm.ReadMapFile(const Fname: String);
var
  pUn: PUnitEntry;
  pFun: PFuncsEntry;
  pLin: PLineEntry;
  CurrUnit: integer;
  sl: TStringList;
  I: integer;
  idx: integer;
  sUnitName, s: String;
begin
  if not FileExists(Fname) then
    Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(Fname);
    if sl.Count = 0 then
      Exit;

    // find "Detailed map of segments"
    I := 0;
    while I < sl.Count - 1 do begin
      if SameStr(sl[I], 'Detailed map of segments') then
        Break;
      Inc(I);
    end;

    // if not found, show error and abort
    if I = sl.Count - 1 then
      Exit;

    // Skip blanks
    Inc(I);
    while (I < sl.Count - 1) and (sl[I] = '') do
      Inc(I);

    // look for specific address' unit
    while I < sl.Count - 1 do begin

      if sl[I] = '' then
        Break;

      if Copy(sl[I], 1, 6) = ' 0001:' then begin
        pUn := New(PUnitEntry);
        pUn^.Start := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
        pUn^.Len := StrToIntDef('$' + Copy(sl[I], 16, 8), 0);
        idx := Pos('ACBP=', sl[I]);
        if idx > 0 then
          pUn^.Name := TrimRight(Copy(sl[I], 60, idx - 60 - 1));
        fUnits.Add(pUn);
      end;
      Inc(I);
    end;

    // find "  Address         Publics by Value"
    while I < sl.Count - 1 do begin
      if SameStr(sl[I], '  Address         Publics by Value') then
        Break;
      Inc(I);
    end;

    // Skip blanks
    Inc(I);
    while (I < sl.Count - 1) and (sl[I] = '') do
      Inc(I);

    // locate function name s, ignore variables
    while I < sl.Count - 1 do begin
      if sl[I] = '' then
        Break;
      if Copy(sl[I], 1, 6) = ' 0001:' then begin
        pFun := New(PFuncsEntry);
        pFun^.Name := Trim(Copy(sl[I], 22, MaxInt));
        pFun^.Address := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
        fFuncs.Add(pFun);
      end;
      Inc(I);
    end;

    // Skip blanks
    Inc(I);
    while (I < sl.Count - 1) and (sl[I] = '') do
      Inc(I);

    // find "Line numbers for "
    while (I < sl.Count - 1) and (not SameStr(sl[I], 'Bound resource files')) do begin

      // Check for a unit section start...
      CurrUnit := -1;
      while I < sl.Count - 1 do begin
        if StartsStr('Line numbers for ', sl[I]) then begin
          idx := Pos('(', sl[I]);
          if idx > 0 then begin
            sUnitName := Copy(sl[I], 18, idx - 18);
            for idx := 0 to fUnits.Count - 1 do begin
              if SameStr(sUnitName, PUnitEntry(fUnits[idx])^.Name) then begin
                CurrUnit := idx;
                Break;
              end;
            end;
          end;
          break;
        end;
        Inc(I);
      end;

      // Skip first blank
      Inc(I);
      while (I < sl.Count - 1) and (sl[I] = '') do
        Inc(I);

      // Add all line entries
      while (I < sl.Count - 1) and (sl[I] <> '') do begin

        // Not every line has four entries!
        while Length(sl[I]) >= 20 do begin
          pLin := New(PLineEntry);
          pLin^.Line := Trim(Copy(sl[I], 1, 6));
          pLin^.Address := StrToIntDef('$' + Copy(sl[I], 13, 8), 0);
          pLin^.UnitIndex := CurrUnit;
          fLines.Add(pLin);

          // Remove item
          s := sl[i];
          Delete(s, 1, 20);
          sl[i] := s;
        end;
        Inc(I);
      end;

      // Over here, we've found the second blank...
      Inc(I);
    end;
  finally
    sl.Free;
  end;
end;

function TExceptionFrm.AddressInfo(Address: Cardinal): String;
var
  I, iUnit: integer;
  MapAddress: dword;
  sUnit, sFunction, sLine: String;
begin
  sUnit := '';
  sFunction := '';
  sLine := '';
  iUnit := -1;

  MapAddress := Address - (hInstance + $1000);
  if MapAddress >= $FF000000 then // out of scope
    Exit;

  // find unit
  for I := 0 to fUnits.Count - 1 do
    if (MapAddress >= PUnitEntry(fUnits[I])^.Start) and (MapAddress <= PUnitEntry(fUnits[I])^.Start +
      Cardinal(PUnitEntry(fUnits[I])^.Len)) then begin
      iUnit := I;
      sUnit := PUnitEntry(fUnits[I])^.Name;
      Break;
    end;

  // find function
  for I := 0 to fFuncs.Count - 2 do
    if (MapAddress >= PFuncsEntry(fFuncs[I])^.Address) and (MapAddress < PFuncsEntry(fFuncs[I + 1])^.Address) then begin
      sFunction := PFuncsEntry(fFuncs[I])^.Name;
      Break;
    end;

  // find line, if we found a unit
  if (iUnit <> -1) then
    for I := 0 to fLines.Count - 2 do
      if (integer(PLineEntry(fLines[I])^.UnitIndex) = iUnit) then
        if (MapAddress >= PLineEntry(fLines[I])^.Address) and (MapAddress < PLineEntry(fLines[I + 1])^.Address) then
          begin
          sLine := PLineEntry(fLines[I])^.Line;
          Break;
        end;

  if (sFunction <> '') and (sUnit <> '') and (sLine <> '') then // found all
    Result := Format('%8.8x (%8.8x): %s (%s - %s)'#13#10, [Address, MapAddress, sFunction, sUnit, sLine])
      //else if (sFunction <> '') and (sUnit <> '') then // couldn't find line
//	Result := Format('%8.8x (%8.8x): %s (%s)'#13#10,[Address, MapAddress, sFunction, sUnit])
//else if (sFunction <> '') then // couldn't find line, unit
//	Result := Format('%8.8x (%8.8x): %s'#13#10,[Address, MapAddress, sFunction])
  else // addresses only?
    Result := ''; //Format('%8.8x (%8.8x)'#13#10,[Address, MapAddress]);
end;

function TExceptionFrm.GetStackReport: String;
var
  {retaddr, }walker: ^pointer;
  max: Cardinal;
begin

  result := 'Stack trace' + #13#10
    + '-----------' + #13#10;

  // error function
  //result := result + AddressInfo(Cardinal(StrToInt(lblAddress.Caption)));

  // Hack fix, assume stack leftovers are not erased
  asm
		mov walker, esp
  end;
  max := StackStop;
  repeat
    result := result + AddressInfo(Cardinal(walker^));
    Inc(walker);
  until dword(walker) > max;

  // History of stack, ignore esp frame, because it'll just refer to GetStackReport
  //asm
  //	mov walker, ebp
  //end;

  // assume return address is present above ebp
  //while Cardinal(walker^) <> 0 do begin
  //	retaddr := walker;
  //	Inc(retaddr);
  //	result := result + AddressInfo(Cardinal(retaddr^));
  //	walker := walker^;
  //end;

  // Remove leftover enters
  result := Trim(result);
end;

function TExceptionFrm.GetErrorReport: String;
begin
  result := 'Error info' + #13#10
    + '----------' + #13#10
    + 'Version     : ' + DEVCPP_VERSION + #13#10
    + 'Build Time  : ' + fBuildTime + #13#10
    + 'Message     : ' + lblError.Caption + #13#10
    + 'Address     : ' + lblAddress.Caption;
end;

function TExceptionFrm.GetMachineReport: String;
begin
  Result := 'Machine info' + #13#10
    + '------------' + #13#10
    + 'Platform      : ' + fPlatform + #13#10
    + 'OS version    : ' + fOSVersion + #13#10;

  // We have room to spare with our POST approach, so include anyway
  if fAdditionalInfo <> '' then
    Result := Result + 'Service Pack  : ' + fAdditionalInfo + #13#10;

  // Idem dito
  Result := Result + 'Computer Name : ' + fComputerName;
end;

function TExceptionFrm.GetMemoryReport: String;
begin
  Result := 'Memory Status'#13#10
    + '-------------'#13#10
    + 'Physical memory, total : ' + fTotalPhys + #13#10
    + 'Physical memory, free  : ' + fUsedPhys + #13#10
    + 'Physical memory, used  : ' + fFreePhys + #13#10
    + 'Pagefile, total        : ' + fTotalCache + #13#10
    + 'Pagefile, free         : ' + fUsedCache + #13#10
    + 'Pagefile, used         : ' + fFreeCache + #13#10
    + 'Virtual memory, total  : ' + fTotalVirt + #13#10
    + 'Virtual memory, free   : ' + fUsedVirt + #13#10
    + 'Virtual memory, used   : ' + fFreeVirt;
end;

procedure TExceptionFrm.GatherSystemInfo;
var
  ms: TMemoryStatus;
  vi: TOSVersionInfo;
  FreeP, UsedP: double;
  Tot, Avail: double;
  Buf: array[0..MAX_PATH] of Char;
  BufSize: cardinal;
begin
  // Set Program tab
  fProgramVersion := DEVCPP_VERSION; //GetVersionString(ParamStr(0));
  fProgramPath := ParamStr(0);
  fBuildTime := GetBuildTime(ParamStr(0));

  // Set Machine tab
  vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(vi);
  case vi.dwPlatformId of
    VER_PLATFORM_WIN32s:
      fPlatform := 'Win3.1 with Win32s';
    VER_PLATFORM_WIN32_WINDOWS:
      fPlatform := 'Windows 95 and later';
    VER_PLATFORM_WIN32_NT:
      fPlatform := 'Windows NT';
  else
    fPlatform := 'Unknown';
  end;

  fOSversion := Format('%d.%d (build %d)', [vi.dwMajorVersion, vi.dwMinorVersion, vi.dwBuildNumber]);
  fAdditionalInfo := vi.szCSDVersion;

  BufSize := MAX_PATH;
  GetComputerName(Buf, BufSize);
  fComputerName := StrPas(Buf);

  // Set Memory tab
  ms.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(ms);

  Tot := ms.dwTotalPhys;
  Avail := ms.dwAvailPhys;
  FreeP := (Avail * 100) / Tot;
  UsedP := 100 - FreeP;
  fTotalPhys := Format('%0.0n', [Tot]);
  fUsedPhys := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
  fFreePhys := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

  Tot := ms.dwTotalPageFile;
  Avail := ms.dwAvailPageFile;
  FreeP := (Avail * 100) / Tot;
  UsedP := 100 - FreeP;
  fTotalCache := Format('%0.0n', [Tot]);
  fUsedCache := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
  fFreeCache := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

  Tot := ms.dwTotalVirtual;
  Avail := ms.dwAvailVirtual;
  FreeP := (Avail * 100) / Tot;
  UsedP := 100 - FreeP;
  fTotalVirt := Format('%0.0n', [Tot]);
  fUsedVirt := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
  fFreeVirt := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);
  fMemoryLoad := Format('%d%%', [ms.dwMemoryLoad]);
end;

procedure TExceptionFrm.btnSendClick(Sender: TObject);
var
  Socket: TClientSocket;
  I: integer;
  //	SocketResult: integer;
  //	Buffer: array[0..1024] of Char;
  Cmd, EmailBody, EmailSubject: String;
begin
  {// Move focus to other button
  btnSend.Default := false;
  btnContinue.Default := true;

  // use a blocking WinSock to send mail via PHP
  Socket := TClientSocket.Create(self);
  Socket.Port := 80;
  Socket.Host := 'www.wilcobrouwer.nl';
  Socket.ClientType := ctBlocking;
  Socket.Open;

  // Description, email, body
  EmailBody := EmailBody + 'Description' + #13#10 + '-----------' + #13#10 + memUserReport.Text + #13#10#13#10#13#10;
  EmailBody := EmailBody + 'Email' + #13#10 + '-----' + #13#10 + memEmailReport.Text + #13#10#13#10#13#10;
  EmailBody := EmailBody + memBugReport.Text;

  // And subject...
  EmailSubject := 'Dev-C++ ' + DEVCPP_VERSION + ' bug report (' + IntToStr(DateTimeToUnix(Now)) + ')';

  // Send everything messages in fully encoded form
  for I := 0 to 255 do
    if not CharInSet(Chr(I), ['a'..'z', 'A'..'Z', '0'..'9', '%']) then begin
      EmailBody := StringReplace(EmailBody, Chr(I), '%' + IntToHex(I, 2), [rfReplaceAll]);
      EmailSubject := StringReplace(EmailSubject, Chr(I), '%' + IntToHex(I, 2), [rfReplaceAll]);
    end;

  // Combine and send the really safe message
  Cmd := '/bugreporter.php?message=' + EmailBody + '&subject=' + EmailSubject;

  // Let PHP do the TLS/SMTP work
  try
    Socket.Socket.SendText(AnsiString('POST ' + Cmd + ' HTTP/1.1' + #13#10 + 'Host: www.wilcobrouwer.nl:80' + #13#10#13#10));
    //if SocketResult > 0 then begin
    //	while SocketResult > 0 do begin
    //		FillChar(Buffer,SizeOf(Buffer),0);
    //		SocketResult := Socket.Socket.ReceiveBuf(Buffer,SizeOf(Buffer));
    //		if SocketResult > 0 then
    //			memBugReport.Text := Buffer;
    //	end;
    //end;
    Socket.Close;
  except
    btnSend.Caption := 'Error sending bug report. Lol.';
    Exit;
  end;}

  ShellExecute(GetDesktopWindow(), 'open', PChar('https://github.com/Embarcadero/Dev-Cpp/issues'), nil, nil, SW_SHOWNORMAL);

  btnSend.Caption := 'Thank you!';
  btnSend.Enabled := false;
end;

procedure TExceptionFrm.FormCreate(Sender: TObject);
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  fLines := TList.Create;
  fFuncs := TList.Create;
  fUnits := TList.Create;
  ReadMapFile(ChangeFileExt(ParamStr(0), '.map'));
  GatherSystemInfo;
end;

procedure TExceptionFrm.FormDestroy(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to fLines.Count - 1 do
    Dispose(PLineEntry(fLines[i]));
  fLines.Free;

  for I := 0 to fFuncs.Count - 1 do
    Dispose(PFuncsEntry(fFuncs[i]));
  fFuncs.Free;

  for I := 0 to fUnits.Count - 1 do
    Dispose(PUnitEntry(fUnits[i]));
  fUnits.Free;
end;

procedure TExceptionFrm.btnTerminateClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to terminate the application?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    TerminateProcess(GetCurrentProcess, 0);
end;

procedure TExceptionFrm.btnContinueClick(Sender: TObject);
begin
  Close;
end;

procedure TExceptionFrm.lblUpdateLinkClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open', PChar(TLabel(Sender).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TExceptionFrm.memUserReportEnter(Sender: TObject);
begin
  if memUserReport.Text = UserReportMsg then
    memUserReport.Text := '';
end;

procedure TExceptionFrm.memUserReportExit(Sender: TObject);
begin
  if memUserReport.Text = '' then
    memUserReport.Text := UserReportMsg;
end;

procedure TExceptionFrm.memEmailReportEnter(Sender: TObject);
begin
  if memEmailReport.Text = EmailReportMsg then
    memEmailReport.Text := '';
end;

procedure TExceptionFrm.memEmailReportExit(Sender: TObject);
begin
  if memEmailReport.Text = '' then
    memEmailReport.Text := EmailReportMsg;
end;

procedure TExceptionFrm.btnShowReportClick(Sender: TObject);
begin
  if btnShowReport.Caption = 'Hide report' then begin // up arrow
    ClientHeight := 340;
    btnShowReport.Caption := 'Show report';
  end else begin
    ClientHeight := 528;
    btnShowReport.Caption := 'Hide report';
  end;
end;

initialization
  Application.OnException := TEAnalyzer.EHandler;

end.

