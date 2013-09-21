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
unit ExceptionFrm;

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
  TUnitEntry = record
    Name: AnsiString;
    Start: dword;
    Len: integer;
  end;

  PFuncsEntry = ^TFuncsEntry;
  TFuncsEntry = record
    Name: AnsiString;
    Address: dword;
  end;

  PLineEntry = ^TLineEntry;
  TLineEntry = record
    Line: AnsiString;
    Address: dword;
    UnitIndex: dword;
  end;

  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

  TExceptionFrm = class(TForm)
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
    Label13: TLabel;
    lblBuildTime: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    fEmail: AnsiString;
    fSubject: AnsiString;
    fLines: TList;
    fUnits: TList;
    fFuncs: TList;
    function StackStop: dword;
    procedure ReadMapFile(const Fname: AnsiString);
    function AddressInfo(Address: Cardinal): AnsiString;
    procedure GatherSystemInfo; // fill interface
    function GetStackReport: AnsiString; // bug report text
    function GetMemoryReport: AnsiString; // ...
    function GetMachineReport: AnsiString;
    function GetErrorReport: AnsiString;
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

class procedure TEAnalyzer.EHandler(Sender: TObject; E: Exception);
begin
	with TExceptionFrm.Create(Application.MainForm) do try

		lblAddress.Caption := Format('0x%8.8x', [dword(ExceptAddr)]);
		lblError.Caption := E.Message;

		// Don't use GetMemoryReport, we can only use 1280 chars
		memBugReport.Text :=
          GetErrorReport + #13#10#13#10#13#10 +
          GetMachineReport + #13#10#13#10#13#10 +
          GetStackReport;

		if ShowModal = mrAbort then
			if MessageDlg('Are you sure you want to terminate the application?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
				TerminateProcess(GetCurrentProcess, 0);
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

procedure TExceptionFrm.ReadMapFile(const Fname: AnsiString);
var
  pUn: PUnitEntry;
  pFun: PFuncsEntry;
  pLin: PLineEntry;
  CurrUnit: integer;
  sl: TStringList;
  I: integer;
  idx: integer;
  sUnitName, s: AnsiString;
begin
	if not FileExists(Fname) then
		Exit;

	sl := TStringList.Create;
	try
		sl.LoadFromFile(Fname);
		if sl.Count = 0 then Exit;

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
				pFun := New(PFuncsEntry);
				pFun^.Name := Trim(Copy(sl[I], 22, MaxInt));
				pFun^.Address := StrToIntDef('$' + Copy(sl[I], 7, 8), 0);;
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

				// Not every line has four entries!
				while Length(sl[I]) >= 20 do begin
					pLin := New(PLineEntry);
					pLin^.Line := Trim(Copy(sl[I], 1, 6));
					pLin^.Address := StrToIntDef('$' + Copy(sl[I], 13, 8), 0);
					pLin^.UnitIndex := CurrUnit;
					fLines.Add(pLin);

					// Remove item
					s := sl[i];
					Delete(s,1,20);
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

function TExceptionFrm.AddressInfo(Address: Cardinal): AnsiString;
var
  I, iUnit: integer;
  MapAddress: dword;
  sUnit, sFunction, sLine: AnsiString;
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
		if (MapAddress >= PUnitEntry(fUnits[I])^.Start) and (MapAddress <= PUnitEntry(fUnits[I])^.Start + Cardinal(PUnitEntry(fUnits[I])^.Len)) then begin
			iUnit := I;
			sUnit := PUnitEntry(fUnits[I])^.Name;
			Break;
		end;

	// find function
	for I := 0 to fFuncs.Count - 2 do
		if (MapAddress >= PFuncsEntry(fFuncs[I])^.Address) and (MapAddress < PFuncsEntry(fFuncs[I+1])^.Address) then begin
			sFunction := PFuncsEntry(fFuncs[I])^.Name;
			Break;
		end;

	// find line, if we found a unit
	if (iUnit <> -1) then
		for I := 0 to fLines.Count - 2 do
			if (integer(PLineEntry(fLines[I])^.UnitIndex) = iUnit) then
				if (MapAddress >= PLineEntry(fLines[I])^.Address) and (MapAddress < PLineEntry(fLines[I+1])^.Address) then begin
					sLine := PLineEntry(fLines[I])^.Line;
					Break;
				end;

	if (sFunction <> '') and (sUnit <> '') and (sLine <> '') then // found all
		Result := Format('%8.8x (%8.8x): %s (%s - %s)'#13#10,[Address, MapAddress, sFunction, sUnit, sLine])
	//else if (sFunction <> '') and (sUnit <> '') then // couldn't find line
	//	Result := Format('%8.8x (%8.8x): %s (%s)'#13#10,[Address, MapAddress, sFunction, sUnit])
	//else if (sFunction <> '') then // couldn't find line, unit
	//	Result := Format('%8.8x (%8.8x): %s'#13#10,[Address, MapAddress, sFunction])
	else // addresses only?
		Result := '';//Format('%8.8x (%8.8x)'#13#10,[Address, MapAddress]);
end;

function TExceptionFrm.GetStackReport: AnsiString;
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
	memStackTrace.Text := result;
end;

function TExceptionFrm.GetErrorReport: AnsiString;
begin
  result := 'Error info' + #13#10
          + '----------' + #13#10
          + 'Version     : ' + DEVCPP_VERSION + #13#10
          + 'Build Time  : ' + lblBuildTime.Caption + #13#10
          + 'Message     : ' + lblError.Caption + #13#10
          + 'Address     : ' + lblAddress.Caption;
end;

function TExceptionFrm.GetMachineReport: AnsiString;
begin
  Result := 'Machine info' + #13#10
          + '------------' + #13#10
          + 'Platform      : ' + lblPlatform.Caption + #13#10
          + 'OS version    : ' + lblOSversion.Caption + #13#10;

  // Don't waste our precious 1280 bytes!
  if lblAdditionalInfo.Caption <> '' then
    Result := Result + 'Service Pack  : ' + lblAdditionalInfo.Caption + #13#10;

  // TODO: remove this useles info?
  Result := Result + 'Computer Name : ' + lblComputerName.Caption;
end;

function TExceptionFrm.GetMemoryReport: AnsiString;
begin
  Result := 'Memory Status'#13#10
          + '-------------'#13#10
          + 'Physical memory, total : ' + lblTotalPhys.Caption + #13#10
          + 'Physical memory, free  : ' + lblUsedPhys.Caption + #13#10
          + 'Physical memory, used  : ' + lblFreePhys.Caption + #13#10
          + 'Pagefile, total        : ' + lblTotalCache.Caption + #13#10
          + 'Pagefile, free         : ' + lblUsedCache.Caption + #13#10
          + 'Pagefile, used         : ' + lblFreeCache.Caption + #13#10
          + 'Virtual memory, total  : ' + lblTotalVirt.Caption + #13#10
          + 'Virtual memory, free   : ' + lblUsedVirt.Caption + #13#10
          + 'Virtual memory, used   : ' + lblFreeVirt.Caption;
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
	lblProgramVersion.Caption := DEVCPP_VERSION;//GetVersionString(ParamStr(0));
	lblProgramPath.Caption := ParamStr(0);
	lblBuildTime.Caption := GetBuildTime(ParamStr(0));

	// Set Machine tab
	vi.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
	GetVersionEx(vi);
	case vi.dwPlatformId of
		VER_PLATFORM_WIN32s:
			lblPlatform.Caption := 'Win3.1 with Win32s';
		VER_PLATFORM_WIN32_WINDOWS:
			lblPlatform.Caption := 'Windows 95 and later';
		VER_PLATFORM_WIN32_NT:
			lblPlatform.Caption := 'Windows NT';
		else
			lblPlatform.Caption := 'Unknown';
	end;

	lblOSversion.Caption := Format('%d.%d (build %d)', [vi.dwMajorVersion, vi.dwMinorVersion, vi.dwBuildNumber]);
	lblAdditionalInfo.Caption := vi.szCSDVersion;

	BufSize := MAX_PATH;
	GetComputerName(Buf, BufSize);
	lblComputerName.Caption := StrPas(Buf);

	// Set Memory tab
	ms.dwLength := SizeOf(TMemoryStatus);
	GlobalMemoryStatus(ms);

	Tot := ms.dwTotalPhys;
	Avail := ms.dwAvailPhys;
	FreeP := (Avail * 100) / Tot;
	UsedP := 100 - FreeP;
	lblTotalPhys.Caption := Format('%0.0n', [Tot]);
	lblUsedPhys.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
	lblFreePhys.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

	Tot := ms.dwTotalPageFile;
	Avail := ms.dwAvailPageFile;
	FreeP := (Avail * 100) / Tot;
	UsedP := 100 - FreeP;
	lblTotalCache.Caption := Format('%0.0n', [Tot]);
	lblUsedCache.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
	lblFreeCache.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);

	Tot := ms.dwTotalVirtual;
	Avail := ms.dwAvailVirtual;
	FreeP := (Avail * 100) / Tot;
	UsedP := 100 - FreeP;
	lblTotalVirt.Caption := Format('%0.0n', [Tot]);
	lblUsedVirt.Caption := Format('%0.0n'#13#10'%6.2f%%', [Tot - Avail, UsedP]);
	lblFreeVirt.Caption := Format('%0.0n'#13#10'%6.2f%%', [Avail, FreeP]);
	lblMemoryLoad.Caption := Format('%d%%', [ms.dwMemoryLoad]);
end;

procedure TExceptionFrm.FormShow(Sender: TObject);
begin
  ClientHeight := CLOSED_HEIGHT;
end;

procedure TExceptionFrm.btnViewClick(Sender: TObject);
begin
  if btnView.Down then
    ClientHeight := OPENED_HEIGHT
  else
    ClientHeight := CLOSED_HEIGHT;
end;

procedure TExceptionFrm.btnSendClick(Sender: TObject);
var
	Cmd: AnsiString;
	I: integer;
begin
	Cmd := 'mailto:' + fEmail + '?Subject=' + fSubject + '&Body=';
	Cmd := Cmd + '<Please include a description of what you were doing before the error occurred>%0A%0A';
	for I := 0 to memBugReport.Lines.Count - 1 do
		Cmd := Cmd + memBugReport.Lines[I] + '%0A';
	Delete(Cmd, 1280, MaxInt); // there is problem with bigger strings in ShellExecute
	ShellExecute(0, 'open', PAnsiChar(Cmd), nil, nil, SW_SHOWNORMAL);
end;

procedure TExceptionFrm.FormCreate(Sender: TObject);
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	fEmail := 'johanmes93@gmail.com';
	fSubject := 'Orwell Dev-C++ ' + DEVCPP_VERSION + ' bug report';

	fLines := TList.Create;
	fFuncs := TList.Create;
	fUnits := TList.Create;
	ReadMapFile(ChangeFileExt(ParamStr(0), '.map'));
	GatherSystemInfo;
end;

procedure TExceptionFrm.btnHelpClick(Sender: TObject);
var
  Msg: AnsiString;
begin
  Msg := 'An error has occurred in the application and this window popped-up. ' +
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

procedure TExceptionFrm.FormDestroy(Sender: TObject);
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

initialization
	Application.OnException := TEAnalyzer.EHandler;

end.

