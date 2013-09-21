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

{$WARN SYMBOL_PLATFORM OFF}
{$D+} // debugging in this unit
{$OPTIMIZATION off } // and no optimization
unit ExceptionsAnalyzerFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, StdCtrls, Buttons, ExtCtrls, ShellAPI, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, StrUtils, QStdCtrls, QButtons, QExtCtrls, QComCtrls;
{$ENDIF}

type
  PUnitEntry = ^TUnitEntry;
  TUnitEntry = packed record
    Name: AnsiString;
    Start: dword;
    Len: integer;
  end;

  PFuncsEntry = ^TFuncsEntry;
  TFuncsEntry = packed record
    Name: AnsiString;
    Address: dword;
  end;

  PLineEntry = ^TLineEntry;
  TLineEntry = packed record
    Line: AnsiString;
    Address: dword;
    UnitIndex: dword;
  end;

  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

  TfrmExceptionsAnalyzer = class(TForm)
    lblError: TLabel;
    btnClose: TButton;
    lblTitle: TLabel;
    lblAddressTitle: TLabel;
    lblAddress: TLabel;
    lblErrorTitle: TLabel;
    btnTerminate: TButton;
    btnSend: TSpeedButton;
    btnView: TSpeedButton;
    Shape1: TShape;
    Bevel2: TBevel;
    Image1: TImage;
    PageControl1: TPageControl;
    tabStackTrace: TTabSheet;
    memStackTrace: TMemo;
    tabProgram: TTabSheet;
    tabMachine: TTabSheet;
    tabMemory: TTabSheet;
    tabFullReport: TTabSheet;
    Label1: TLabel;
    lblProgramPath: TLabel;
    Label3: TLabel;
    lblProgramVersion: TLabel;
    Label2: TLabel;
    lblPlatform: TLabel;
    Label5: TLabel;
    lblOSversion: TLabel;
    Label7: TLabel;
    lblAdditionalInfo: TLabel;
    Label9: TLabel;
    lblComputerName: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    lblTotalPhys: TLabel;
    Label8: TLabel;
    lblUsedPhys: TLabel;
    Label11: TLabel;
    lblFreePhys: TLabel;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    lblTotalVirt: TLabel;
    Label12: TLabel;
    lblUsedVirt: TLabel;
    Label14: TLabel;
    lblFreeVirt: TLabel;
    GroupBox3: TGroupBox;
    Label16: TLabel;
    lblTotalCache: TLabel;
    Label18: TLabel;
    lblUsedCache: TLabel;
    Label20: TLabel;
    lblFreeCache: TLabel;
    Label10: TLabel;
    lblMemoryLoad: TLabel;
    memBugReport: TMemo;
    btnHelp: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    fEmail: AnsiString;
    fSubject: AnsiString;
    fReportEXEversion: boolean;
    fReportComputerName: boolean;
    fReportMachine: boolean;
    fReportMemory: boolean;
    fReportStackTrace: boolean;
    fLines: TList;
    fUnits: TList;
    fFuncs: TList;
    procedure ReadMapFile(const Fname: AnsiString);
    procedure ClearModules;
    function AddressInfo(Address: dword): AnsiString;
    function GatherSystemInfo: AnsiString;
    function StackStop: dword;
  end;

  TEAnalyzer = class(TPersistent)
  public
    class procedure EHandler(Sender: TObject; E: Exception);
  end;

implementation

{$R *.dfm}

uses 
  utils, devcfg, version;

const
  CLOSED_HEIGHT = 160;
  OPENED_HEIGHT = 360;

var
  frmExceptionsAnalyzer: TfrmExceptionsAnalyzer;

class procedure TEAnalyzer.EHandler(Sender: TObject;E: Exception);
const usermsg = 'Please include a description of what you were doing before the error occured (please give as much precisions as possible) : ';
var
  target: pointer;
  p, stackstart: ^pointer;
  sl: TStringList;
  I: integer;
  S: AnsiString;
  si, Res: AnsiString;
  max: dword;
begin
	asm
		mov stackstart, esp
	end;

	// show dialog
	frmExceptionsAnalyzer := TfrmExceptionsAnalyzer.Create(Application.MainForm);
	with frmExceptionsAnalyzer do begin

		max := StackStop;

		sl := TStringList.Create;
		try
			p := stackstart;
 			repeat
				target := p^;
				sl.Add(IntToStr(dword(target)));
				inc(p);
			until dword(p) > max;

			Res := '';
			for I := 0 to sl.Count - 1 do begin
				S := AddressInfo(StrToIntDef(sl[I], -1));
				if S <> '' then
					Res := Res + S; // + #13#10;
			end;

			// wrap if it's one, very long line
			if Pos(#13#10, Res) = 0 then begin
				I := 51;
				while I < Length(Res) do begin
					Insert(#13#10, Res, I);
					I := I + 52;
				end;
			end;
		finally
			sl.Free;
		end;

		if Res = '' then
			Res := '<StackTrace info unavailable>';

		lblAddress.Caption := Format('0x%8.8x', [dword(ExceptAddr)]);
		lblError.Caption := E.Message;
		memStackTrace.Text := Res;
		si := GatherSystemInfo;
		memBugReport.Text := si
                       + #13#10
                       + 'The following error occured in version ' + DEVCPP_VERSION + ':'#13#10
                       + lblError.Caption + ' (at address ' + lblAddress.Caption + ')'#13#10#13#10#13#10
                       + usermsg + #13#10#13#10#13#10
                       + 'State information follows:'#13#10;
		if fReportStackTrace then
			memBugReport.Text := memBugReport.Text + 'Stack trace:'#13#10'------------'#13#10 + Res;
		if ShowModal = mrAbort then
			if MessageDlg('Are you sure you want to terminate the application?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
				TerminateProcess(GetCurrentProcess, 0);
	end;
	frmExceptionsAnalyzer.Free;
end;

{ TfrmExceptionsAnalyzer }

procedure TfrmExceptionsAnalyzer.ReadMapFile(const Fname: AnsiString);
var
  pUn: PUnitEntry;
  pFun: PFuncsEntry;
  pLin: PLineEntry;
  CurrUnit: integer;
  sl: TStringList;
  I: integer;
  idx: integer;
  iStart, iLen: longint;
  sUnitName: AnsiString;
begin
	if not FileExists(Fname) then
		Exit;

	sl := TStringList.Create;
	try
		sl.LoadFromFile(Fname);
		if sl.Count > 0 then begin

		// find "Detailed map of segments"
		I := 0;
		while I < sl.Count - 1 do begin
			if SameStr(sl[I],'Detailed map of segments') then
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
				iStart := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
				iLen := StrToIntDef('$' + Copy(sl[I], 16, 8), 0);

				pUn := New(PUnitEntry);
				idx := Pos('ACBP=', sl[I]);
				if idx > 0 then
					pUn^.Name := TrimRight(Copy(sl[I], 60, idx - 60 - 1));

				pUn^.Start := iStart;
				pUn^.Len := iLen;
				fUnits.Add(pUn);
			end;
			Inc(I);
		end;

		// find "  Address         Publics by Value"
		while I < sl.Count - 1 do begin
			if SameStr(sl[I],'  Address         Publics by Value') then
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
				iStart := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);
				pFun := New(PFuncsEntry);
				pFun^.Name := Trim(Copy(sl[I], 22, MaxInt));
				pFun^.Address := iStart;
				fFuncs.Add(pFun);
			end;
			Inc(I);
		end;

		// Skip blanks
		Inc(I);
		while (I < sl.Count - 1) and (sl[I] = '') do
			Inc(I);

		// find "Line numbers for "
		while not SameStr(sl[I],'Bound resource files') do begin

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

				pLin := New(PLineEntry);
				pLin^.Line := Trim(Copy(sl[I], 1, 6));
				pLin^.Address := StrToIntDef('$' + Copy(sl[I], 13, 8), 0);
				pLin^.UnitIndex := CurrUnit;
				fLines.Add(pLin);

				pLin := New(PLineEntry);
				pLin^.Line := Trim(Copy(sl[I], 21, 6));
				pLin^.Address := StrToIntDef('$' + Copy(sl[I], 33, 8), 0);
				pLin^.UnitIndex := CurrUnit;
				fLines.Add(pLin);

				pLin := New(PLineEntry);
				pLin^.Line := Trim(Copy(sl[I], 41, 6));
				pLin^.Address := StrToIntDef('$' + Copy(sl[I], 53, 8), 0);
				pLin^.UnitIndex := CurrUnit;
				fLines.Add(pLin);

				pLin := New(PLineEntry);
				pLin^.Line := Trim(Copy(sl[I], 61, 6));
				pLin^.Address := StrToIntDef('$' + Copy(sl[I], 73, 8), 0);
				pLin^.UnitIndex := CurrUnit;
				fLines.Add(pLin);

				Inc(I);
			end;

			// Over here, we've found the second blank...
			Inc(I);
		end;
	end;
	finally
		sl.Free;
	end;
end;

procedure TfrmExceptionsAnalyzer.ClearModules;
var
	I : integer;
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

function TfrmExceptionsAnalyzer.AddressInfo(Address: dword): AnsiString;
var
  I: integer;
  MapAddress: dword;
  sUnitName: AnsiString;
  sProcName: AnsiString;
  sLineNum: AnsiString;
  UnitIdx: integer;
begin
  sUnitName := '';
  sProcName := '';
  sLineNum := '';
  Result := '';
  UnitIdx := -1;
  dword(MapAddress) := dword(Address) - (dword(hInstance) + $1000);
  if MapAddress >= $FF000000 then // out of scope
    Exit;

  // find unit
  for I := 0 to fUnits.Count - 1 do
    if (MapAddress >= pUnitEntry(fUnits[I])^.Start) and
      (dword(MapAddress) <= (dword(pUnitEntry(fUnits[I])^.Start) + dword(pUnitEntry(fUnits[I])^.Len))) then begin
      sUnitName := pUnitEntry(fUnits[I])^.Name;
      UnitIdx := I;
      Break;
    end;

  // find function
  for I := 0 to fFuncs.Count - 1 do
    if MapAddress < PFuncsEntry(fFuncs[I])^.Address then begin
      if I > 0 then
        sProcName := PFuncsEntry(fFuncs[I - 1])^.Name;
      Break;
    end;

  // find line
  for I := 0 to fLines.Count - 1 do
    if integer(PLineEntry(fLines[I])^.UnitIndex) = UnitIdx then
      if PLineEntry(fLines[I])^.Address > MapAddress then begin
        if (I > 0) and (integer(PLineEntry(fLines[I - 1])^.UnitIndex) = UnitIdx) then
          sLineNum := PLineEntry(fLines[I - 1])^.Line;
        Break;
      end;
  if sLineNum <> '' then
    Result := Format('%8.8x (%8.8x): %s (%s - %s)'#13#10, [Address, MapAddress, sProcName, sUnitName, sLineNum])
  else begin
    if fUnits.Count = 0 then
      Result := Format('%8.8x', [Address]);
  end;
end;

function TfrmExceptionsAnalyzer.GatherSystemInfo: AnsiString;
var
  ms: TMemoryStatus;
  vi: TOSVersionInfo;
  FreeP, UsedP: double;
  Tot, Avail: double;
  Buf: array[0..MAX_PATH] of Char;
  BufSize: cardinal;
  sVer: AnsiString;
begin
  if fReportEXEversion then begin
    sVer := GetVersionString(ParamStr(0));
    frmExceptionsAnalyzer.lblProgramPath.Caption := ParamStr(0);
    if sVer <> '' then begin
      Result := 'Application version: ' + sVer + #13#10;
      frmExceptionsAnalyzer.lblProgramVersion.Caption := sVer;
    end
    else begin
      Result := 'Application version: <not available>'#13#10;
      frmExceptionsAnalyzer.lblProgramVersion.Caption := '<not available>';
    end;
    Result := Result + #13#10;
  end;

  if fReportMachine or fReportComputerName then begin
    Result := Result + 'Machine info'#13#10;
    Result := Result + '---------'#13#10;
    if fReportMachine then begin
      vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      GetVersionEx(vi);
      case vi.dwPlatformId of
        VER_PLATFORM_WIN32s: begin
            frmExceptionsAnalyzer.lblPlatform.Caption := 'Win3.1 with Win32s';
            Result := Result + 'Platform       : Win3.1 with Win32s'#13#10;
          end;
        VER_PLATFORM_WIN32_WINDOWS: begin
            frmExceptionsAnalyzer.lblPlatform.Caption := 'Windows 95 and later';
            Result := Result + 'Platform       : Windows 95 and later'#13#10;
          end;
        VER_PLATFORM_WIN32_NT: begin
            frmExceptionsAnalyzer.lblPlatform.Caption := 'Windows NT';
            Result := Result + 'Platform       : Windows NT'#13#10;
          end;
      else begin
          frmExceptionsAnalyzer.lblPlatform.Caption := 'Unknown';
          Result := Result + 'Platform       : Unknown'#13#10;
        end;
      end;
      Result := Result + Format('OS version     : version %d.%d (build %d)'#13#10, [vi.dwMajorVersion, vi.dwMinorVersion, vi.dwBuildNumber]);
      frmExceptionsAnalyzer.lblOSversion.Caption := Format('%d.%d (build %d)', [vi.dwMajorVersion, vi.dwMinorVersion, vi.dwBuildNumber]);
      Result := Result + Format('Additional info: %s'#13#10, [vi.szCSDVersion]);
      frmExceptionsAnalyzer.lblAdditionalInfo.Caption := vi.szCSDVersion;
    end;
    if fReportComputerName then begin
      BufSize := MAX_PATH;
      GetComputerName(Buf, BufSize);
      Result := Result + Format('Computer name  : %s'#13#10, [Buf]);
      frmExceptionsAnalyzer.lblComputerName.Caption := StrPas(Buf);
    end;

    Result := Result + #13#10;
  end;

  if fReportMemory then begin
    ms.dwLength := SizeOf(TMemoryStatus);
    GlobalMemoryStatus(ms);
    Result := Result + 'Memory Status'#13#10;
    Result := Result + '-------------'#13#10;

    Tot := ms.dwTotalPhys;
    Avail := ms.dwAvailPhys;
    FreeP := (Avail * 100) / Tot;
    UsedP := 100 - FreeP;
    Result := Result + Format('Physical memory: %13.0n total'#13#10, [Tot]);
    Result := Result + Format('                 %13.0n in use (%6.2f%%)'#13#10, [Tot - Avail, UsedP]);
    Result := Result + Format('                 %13.0n free   (%6.2f%%)'#13#10, [Avail, FreeP]);
    frmExceptionsAnalyzer.lblTotalPhys.Caption := Format('%0.0n', [Tot]);
    frmExceptionsAnalyzer.lblUsedPhys.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
    frmExceptionsAnalyzer.lblFreePhys.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

    Tot := ms.dwTotalPageFile;
    Avail := ms.dwAvailPageFile;
    FreeP := (Avail * 100) / Tot;
    UsedP := 100 - FreeP;
    Result := Result + Format('Cache          : %13.0n total'#13#10, [Tot]);
    Result := Result + Format('                 %13.0n in use (%6.2f%%)'#13#10, [Tot - Avail, UsedP]);
    Result := Result + Format('                 %13.0n free   (%6.2f%%)'#13#10, [Avail, FreeP]);
    frmExceptionsAnalyzer.lblTotalCache.Caption := Format('%0.0n', [Tot]);
    frmExceptionsAnalyzer.lblUsedCache.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
    frmExceptionsAnalyzer.lblFreeCache.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

    Tot := ms.dwTotalVirtual;
    Avail := ms.dwAvailVirtual;
    FreeP := (Avail * 100) / Tot;
    UsedP := 100 - FreeP;
    Result := Result + Format('Virtual memory : %13.0n total'#13#10, [Tot]);
    Result := Result + Format('                 %13.0n in use (%6.2f%%)'#13#10, [Tot - Avail, UsedP]);
    Result := Result + Format('                 %13.0n free   (%6.2f%%)'#13#10, [Avail, FreeP]);
    Result := Result + Format('Memory load    : %12d%%'#13#10, [ms.dwMemoryLoad]);
    frmExceptionsAnalyzer.lblTotalVirt.Caption := Format('%0.0n', [Tot]);
    frmExceptionsAnalyzer.lblUsedVirt.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
    frmExceptionsAnalyzer.lblFreeVirt.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);
    frmExceptionsAnalyzer.lblMemoryLoad.Caption := Format('%d%%', [ms.dwMemoryLoad]);
    Result := Result + #13#10;
  end;
end;

function TfrmExceptionsAnalyzer.StackStop: dword;
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

procedure TfrmExceptionsAnalyzer.FormShow(Sender: TObject);
begin
  ClientHeight := CLOSED_HEIGHT;
end;

procedure TfrmExceptionsAnalyzer.btnViewClick(Sender: TObject);
begin
  if btnView.Down then
    ClientHeight := OPENED_HEIGHT
  else
    ClientHeight := CLOSED_HEIGHT;
end;

procedure TfrmExceptionsAnalyzer.btnSendClick(Sender: TObject);
var
	Cmd: AnsiString;
	I: integer;
begin
	Cmd := 'mailto:' + fEmail + '?Subject=' + fSubject + '&Body=';
	for I := 0 to memBugReport.Lines.Count - 1 do
		Cmd := Cmd + memBugReport.Lines[I] + #13#10;
	//Delete(Cmd, 1280, MaxInt); // there is problem with bigger strings in ShellExecute
	ShellExecute(0, 'open', PAnsiChar(Cmd), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmExceptionsAnalyzer.FormCreate(Sender: TObject);
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	fEmail := 'johanmes93@gmail.com';
	fSubject := 'Orwell Dev-C++ ' + DEVCPP_VERSION + ' bug report';
	fReportEXEversion := True;
	fReportComputerName := True;
	fReportMachine := True;
	fReportMemory := True;
	fReportStackTrace := True;

	fLines := TList.Create;
	fFuncs := TList.Create;
	fUnits := TList.Create;

	lblProgramPath.Caption := '';
	lblProgramVersion.Caption := '';
	lblPlatform.Caption := '';
	lblOSversion.Caption := '';
	lblAdditionalInfo.Caption := '';
	lblComputerName.Caption := '';
	lblTotalPhys.Caption := '';
	lblUsedPhys.Caption := '';
	lblFreePhys.Caption := '';
	lblTotalVirt.Caption := '';
	lblUsedVirt.Caption := '';
	lblFreeVirt.Caption := '';
	lblTotalCache.Caption := '';
	lblUsedCache.Caption := '';
	lblFreeCache.Caption := '';
	lblMemoryLoad.Caption := '';

	lblError.Font.Style := [fsBold];

	PageControl1.ActivePageIndex := PageControl1.PageCount - 1;
	btnClose.Hint := 'Closes this window and attempts to continue the application execution';
	btnTerminate.Hint := 'Closes this window and terminates the application execution';
	btnSend.Hint := 'Sends a bug report to the application support team describing the error';
	btnView.Hint := 'View details of the system at the time of the error';

	ReadMapFile(ChangeFileExt(ParamStr(0), '.map'));
end;

procedure TfrmExceptionsAnalyzer.btnHelpClick(Sender: TObject);
var
  Msg: AnsiString;
begin
  Msg := 'An error has occured in the application and this window popped-up. ' +
    'Here is a description of the available options:'#10#10 +
    'Continue:'#10'Closes this window and attempts to continue the application execution.'#10#10 +
    'Terminate:'#10'Closes this window and terminates the application execution.'#10#10 +
    'View bug report:'#10'View details of the system at the time of the error.'#10#10 +
    'Send bug report:'#10'Sends a bug report to the application support team describing the error. ' +
    'You can see what''s in the bug report by clicking on "View bug report" and looking ' +
    'at the "Bug report" sheet. ' +
    'When you press Send, your default mail client will be launched and an e-mail with ' +
    'the bug report''s contents will be created, but will *not* be sent automatically. ' +
    'You will have to send it yourself...'#10 +
    'In the newly created e-mail, fill in any info you can, like what you were doing in the ' +
    'application when the error ocurred...'#10#10 +
    'Help:'#10'Displays this window.';

  ShowMessage(Msg);
end;

procedure TfrmExceptionsAnalyzer.FormDestroy(Sender: TObject);
begin
	ClearModules;
end;

initialization
	Application.OnException := TEAnalyzer.EHandler;

end.

