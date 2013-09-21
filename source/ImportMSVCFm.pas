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

unit ImportMSVCFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QButtons, QStdCtrls;
{$ENDIF}

type
  TImportMSVCForm = class(TForm)
    lbSelect: TLabel;
    txtVC: TEdit;
    btnBrowse: TSpeedButton;
    gbOptions: TGroupBox;
    lbConf: TLabel;
    cmbConf: TComboBox;
    lbDev: TLabel;
    txtDev: TEdit;
    btnBrowseDev: TSpeedButton;
    btnImport: TButton;
    btnCancel: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    XPMenu: TXPMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnBrowseClick(Sender: TObject);
    procedure txtDevChange(Sender: TObject);
    procedure btnBrowseDevClick(Sender: TObject);
  private
    { Private declarations }
    fSL: TStringList;
    fFilename: string;
    fInvalidFiles: string;
    procedure LoadText;
    procedure WriteDev(Section, Key, Value: string);
    procedure ImportFile(Filename: string);
    procedure WriteDefaultEntries;
    procedure SetFilename(Value: string);
    procedure SetDevName(Value: string);
    function ReadTargets(Targets: TStringList): boolean;
    function LocateTarget(var StartAt, EndAt: integer): boolean;
    function LocateSourceTarget(var StartAt, EndAt: integer): boolean;
    function ReadCompilerOptions(StartAt, EndAt: integer): boolean;
    function ReadLinkerOptions(StartAt, EndAt: integer): boolean;
    procedure ReadSourceFiles(StartAt, EndAt: integer);
    procedure ReadProjectType;
    function GetLineValue(StartAt, EndAt: integer; StartsWith: string): string;
    function StripQuotesIfNecessary(s: string): string;
    procedure UpdateButtons;
    function CheckVersion: boolean;
  public
    { Public declarations }
    function GetFilename: string;
  end;

var
  ImportMSVCForm: TImportMSVCForm;

implementation

uses IniFiles, StrUtils, version, MultiLangSupport, devcfg, utils;

{$R *.dfm}

{ TImportMSVCForm }

procedure TImportMSVCForm.UpdateButtons;
begin
  btnImport.Enabled := FileExists(txtVC.Text) and DirectoryExists(ExtractFilePath(txtDev.Text));
  cmbConf.Enabled := txtVC.Text <> '';
  txtDev.Enabled := txtVC.Text <> '';
  btnBrowseDev.Enabled := txtVC.Text <> '';
end;

procedure TImportMSVCForm.FormCreate(Sender: TObject);
begin
  fSL := TStringList.Create;
  LoadText;
end;

procedure TImportMSVCForm.FormDestroy(Sender: TObject);
begin
  fSL.Free;
end;

procedure TImportMSVCForm.FormShow(Sender: TObject);
begin
  txtVC.Text := '';
  txtDev.Text := '';
  cmbConf.Clear;
  UpdateButtons;
end;

function TImportMSVCForm.GetLineValue(StartAt, EndAt: integer;
  StartsWith: string): string;
var
  I: integer;
begin
  Result := '';
  if EndAt > fSL.Count - 1 then
    EndAt := fSL.Count - 1;
  I := StartAt;
  while I <= EndAt do begin
    if AnsiStartsText(StartsWith, fSL[I]) then begin
      Result := StripQuotesIfNecessary(Trim(Copy(fSL[I], Length(StartsWith) + 1, Length(fSL[I]) - Length(StartsWith))));
      Break;
    end;
    Inc(I);
  end;
end;

procedure TImportMSVCForm.ImportFile(Filename: string);
var
  Targets: TStringList;
begin
  fSL.LoadFromFile(Filename);

  Targets := TStringList.Create;
  try
    // check file for version
    if not CheckVersion then
      Exit;

    // read targets
    if not ReadTargets(Targets) then
      Exit;

    // fill the targets combo
    cmbConf.Items.Assign(Targets);
    if cmbConf.Items.Count > 0 then begin
      cmbConf.ItemIndex := 0;
    end;
  finally
    Targets.Free;
  end;
end;

function TImportMSVCForm.LocateTarget(var StartAt,
  EndAt: integer): boolean;
var
  I: integer;
begin
  Result := False;
  I := 0;
  while I < fSL.Count do begin
    if (AnsiStartsStr('!IF ', fSL[I]) or AnsiStartsStr('!ELSEIF ', fSL[I])) and AnsiContainsStr(fSL[I], cmbConf.Text) then begin
      Inc(I);
      StartAt := I;
      while not (AnsiStartsStr('!ENDIF', fSL[I]) or AnsiStartsStr('!ELSEIF', fSL[I])) and (I < fSL.Count) do
        Inc(I);
      EndAt := I - 1;
      Result := True;
      Break;
    end;
    Inc(I);
  end;
end;

function TImportMSVCForm.LocateSourceTarget(var StartAt,
  EndAt: integer): boolean;
var
  I: integer;
begin
  Result := False;
  I := 0;
  while I < fSL.Count do begin
    if (AnsiStartsStr('# Begin Target', fSL[I])) then begin
      Inc(I);
      StartAt := I;
      while not (AnsiStartsStr('# End Target', fSL[I])) do
        Inc(I);
      EndAt := I - 1;
      Result := True;
      Break;
    end;
    Inc(I);
  end;
end;

function TImportMSVCForm.ReadCompilerOptions(StartAt,
  EndAt: integer): boolean;
var
  I: integer;
  Options: TStringList;
  sCompiler: string;
  sDirs: string;
  S: string;
begin
  Result := False;
  sCompiler := '-D__GNUWIN32__ ';
  sDirs := '';
  Options := TStringList.Create;
  try
    Options.Delimiter := ' ';
    Options.DelimitedText := GetLineValue(StartAt, EndAt, '# ADD CPP');
    I := 0;
    while I < Options.Count do begin
      if AnsiCompareText('/D', Options[I]) = 0 then begin
        S := Format('-D%s ', [Options[I + 1]]);
        sCompiler := sCompiler + S;
        Inc(I);
      end
      else if AnsiCompareText('/U', Options[I]) = 0 then begin
        S := Format('-U%s ', [Options[I + 1]]);
        sCompiler := sCompiler + S;
        Inc(I);
      end
      else if AnsiCompareText('/I', Options[I]) = 0 then begin
        S := Options[I + 1];
        sDirs := sDirs + S + ';';
        Inc(I);
      end
      else if (AnsiCompareText('/W1', Options[I]) = 0) or
        (AnsiCompareText('/W2', Options[I]) = 0) or
        (AnsiCompareText('/W3', Options[I]) = 0) then begin // warning messages
        sCompiler := sCompiler + '-W ';
        Inc(I);
      end
      else if AnsiCompareText('/W4', Options[I]) = 0 then begin // all warning messages
        sCompiler := sCompiler + '-Wall ';
        Inc(I);
      end
      else if AnsiCompareText('/WX', Options[I]) = 0 then begin // warnings as errors
        sCompiler := sCompiler + '-Werror ';
        Inc(I);
      end
      else if AnsiCompareText('/GX', Options[I]) = 0 then begin // enable exception handling
        sCompiler := sCompiler + '-fexceptions ';
        Inc(I);
      end
      else if AnsiCompareText('/Ob0', Options[I]) = 0 then begin // disable inline expansion
        sCompiler := sCompiler + '-fno-inline ';
        Inc(I);
      end
      else if AnsiCompareText('/Ob2', Options[I]) = 0 then begin // auto inline function expansion
        sCompiler := sCompiler + '-finline-functions ';
        Inc(I);
      end
      else if AnsiCompareText('/Oy', Options[I]) = 0 then begin // frame pointer omission
        sCompiler := sCompiler + '-fomit-frame-pointer ';
        Inc(I);
      end
      else if AnsiCompareText('/GB', Options[I]) = 0 then begin // blend optimization
        sCompiler := sCompiler + '-mcpu=pentiumpro -D_M_IX86=500 ';
        Inc(I);
      end
      else if AnsiCompareText('/G6', Options[I]) = 0 then begin // pentium pro optimization
        sCompiler := sCompiler + '-mcpu=pentiumpro -D_M_IX86=600 ';
        Inc(I);
      end
      else if AnsiCompareText('/G5', Options[I]) = 0 then begin // pentium optimization
        sCompiler := sCompiler + '-mcpu=pentium -D_M_IX86=500 ';
        Inc(I);
      end
      else if AnsiCompareText('/G4', Options[I]) = 0 then begin // 486 optimization
        sCompiler := sCompiler + '-mcpu=i486 -D_M_IX86=400 ';
        Inc(I);
      end
      else if AnsiCompareText('/G3', Options[I]) = 0 then begin // 386 optimization
        sCompiler := sCompiler + '-mcpu=i386 -D_M_IX86=300 ';
        Inc(I);
      end
      else if AnsiCompareText('/Za', Options[I]) = 0 then begin // disable language extensions
        sCompiler := sCompiler + '-ansi ';
        Inc(I);
      end
      else if AnsiCompareText('/Zp1', Options[I]) = 0 then begin // pack structures
        sCompiler := sCompiler + '-fpack-struct ';
        Inc(I);
      end
      else if AnsiCompareText('/W0', Options[I]) = 0 then begin // no warning messages
        sCompiler := sCompiler + '-w ';
        Inc(I);
      end;
      Inc(I);
    end;
    WriteDev('Project', 'Compiler', sCompiler);
    WriteDev('Project', 'CppCompiler', sCompiler);
    if sDirs <> '' then
      sDirs := Copy(sDirs, 1, Length(sDirs) - 1);
    WriteDev('Project', 'Includes', sDirs);
  finally
    Options.Free;
  end;
end;

function TImportMSVCForm.ReadLinkerOptions(StartAt,
  EndAt: integer): boolean;
var
  I: integer;
  Options: TStringList;
  sLibs: string;
  sDirs: string;
  S: string;
begin
  Result := False;
  sLibs := '';
  sDirs := '';
  Options := TStringList.Create;
  try
    Options.Delimiter := ' ';
    Options.DelimitedText := GetLineValue(StartAt, EndAt, '# ADD LINK32');
    for I := 0 to Options.Count - 1 do
      if (Options[I][1] <> '/') and AnsiEndsText('.lib', Options[I]) then begin
        S := Copy(Options[I], 1, Length(Options[I]) - 4);
        if ExtractFilePath(S) <> '' then
          sDirs := sDirs + ExtractFilePath(S) + ';';
        S := Format('-l%s ', [ExtractFileName(S)]);
        sLibs := sLibs + S;
      end
      else if AnsiStartsText('/base:', Options[I]) then begin
        S := Copy(Options[I], 7, MaxInt);
        sLibs := sLibs + '--image-base ' + S + ' ';
      end
      else if AnsiStartsText('/implib:', Options[I]) then begin
        S := Copy(Options[I], 9, MaxInt);
        sLibs := sLibs + '--implib ' + S + ' ';
      end
      else if AnsiStartsText('/map:', Options[I]) then begin
        S := Copy(Options[I], 6, MaxInt);
        sLibs := sLibs + '-Map ' + S + '.map ';
      end
      else if AnsiStartsText('/subsystem:', Options[I]) then begin
        S := Copy(Options[I], 12, MaxInt);
        if S = 'windows' then
          WriteDev('Project', 'Type', '0') // win32 gui
        else if S = 'console' then
          WriteDev('Project', 'Type', '1'); // console app
//        sLibs := sLibs + '-Wl --subsystem ' + S + ' ';
      end
      else if AnsiStartsText('/libpath:', Options[I]) then begin
        S := Copy(Options[I], 10, MaxInt);
        sDirs := sDirs + S + ';';
      end;
    WriteDev('Project', 'Linker', sLibs);
    if sDirs <> '' then
      sDirs := Copy(sDirs, 1, Length(sDirs) - 1);
    WriteDev('Project', 'Libs', sDirs);
  finally
    Options.Free;
  end;
end;

procedure TImportMSVCForm.ReadSourceFiles(StartAt,
  EndAt: integer);
var
  flds: TStringList;
  I, C: integer;
  UnitName: string;
  folder: string;
  folders: string;
begin
  fInvalidFiles := '';
  C := 0;
  folders := '';
  flds := TStringList.Create;
  try
    flds.Delimiter := '/';
    for I := StartAt to EndAt do
      if AnsiStartsText('# Begin Group ', fSL[I]) then begin
        folder := StripQuotesIfNecessary(Copy(fSL[I], 15, MaxInt));
        flds.Add(folder);
        folders := folders + flds.DelimitedText + ',';
      end
      else if AnsiStartsText('# End Group', fSL[I]) then begin
        if flds.Count > 0 then
          flds.Delete(flds.Count - 1);
      end
      else if AnsiStartsText('SOURCE=', fSL[I]) then begin
        UnitName := Copy(fSL[I], 8, Length(fSL[I]) - 7);
        if FileExists(UnitName) then begin
          UnitName := StringReplace(UnitName, '\', '/', [rfReplaceAll]);
          WriteDev('Unit' + IntToStr(C + 1), 'FileName', UnitName);
          WriteDev('Unit' + IntToStr(C + 1), 'Folder', flds.DelimitedText);
          case GetFileTyp(UnitName) of
            utSrc, utHead: begin
                WriteDev('Unit' + IntToStr(C + 1), 'Compile', '1');
                if AnsiSameText(ExtractFileExt(UnitName), '.c') then
                  WriteDev('Unit' + IntToStr(C + 1), 'CompileCpp', '0')
                else
                  WriteDev('Unit' + IntToStr(C + 1), 'CompileCpp', '1');
                WriteDev('Unit' + IntToStr(C + 1), 'Link', '1');
              end;
            utRes: begin
                WriteDev('Unit' + IntToStr(C + 1), 'Compile', '1');
                WriteDev('Unit' + IntToStr(C + 1), 'CompileCpp', '1');
                WriteDev('Unit' + IntToStr(C + 1), 'Link', '0');
              end;
          else begin
              WriteDev('Unit' + IntToStr(C + 1), 'Compile', '0');
              WriteDev('Unit' + IntToStr(C + 1), 'CompileCpp', '0');
              WriteDev('Unit' + IntToStr(C + 1), 'Link', '0');
            end;
          end;
          WriteDev('Unit' + IntToStr(C + 1), 'Priority', '1000');
          Inc(C);
        end
        else
          fInvalidFiles := fInvalidFiles + UnitName + #13#10;
      end;
  finally
    flds.Free;
  end;

  if folders <> '' then
    Delete(folders, Length(folders), 1);
  WriteDev('Project', 'UnitCount', IntToStr(C));
  WriteDev('Project', 'Folders', folders);
end;

function TImportMSVCForm.ReadTargets(Targets: TStringList): boolean;
var
  I: integer;
  P: PChar;
begin
  Targets.Clear;
  Result := False;
  I := 0;
  while I < fSL.Count do begin
    if AnsiStartsText('# Begin Target', fSL[I]) then begin
      // got it
      Inc(I);
      repeat
        if AnsiStartsText('# Name', fSL[I]) then begin
          P := PChar(Trim(Copy(fSL[I], 7, Length(fSL[I]) - 6)));
          Targets.Add(AnsiExtractQuotedStr(P, '"'));
          Result := True;
        end;
        Inc(I);
      until (I = fSL.Count) or AnsiStartsText('# Begin Source File', fSL[I]);
      Break;
    end;
    Inc(I);
  end;
end;

procedure TImportMSVCForm.SetDevName(Value: string);
begin
  WriteDev('Project', 'Name', Value);
end;

procedure TImportMSVCForm.SetFilename(Value: string);
begin
  WriteDev('Project', 'FileName', Value);
  fFilename := Value;
end;

function TImportMSVCForm.StripQuotesIfNecessary(s: string): string;
var
  P: PChar;
begin
  if AnsiStartsText('"', s) and AnsiEndsText('"', s) then begin
    P := PChar(S);
    Result := AnsiExtractQuotedStr(P, '"');
  end
  else
    Result := S;
end;

procedure TImportMSVCForm.WriteDefaultEntries;
begin
  WriteDev('Project', 'Ver', '1');
  WriteDev('Project', 'IsCpp', '1'); // all MSVC projects are C++ (correct me if I 'm wrong)
end;

procedure TImportMSVCForm.WriteDev(Section, Key, Value: string);
var
  fIni: TIniFile;
begin
  fIni := TIniFile.Create(fFilename);
  try
    fIni.WriteString(Section, Key, Value);
  finally
    fIni.Free;
  end;
end;

procedure TImportMSVCForm.btnImportClick(Sender: TObject);
var
  StartAt, EndAt: integer;
  SrcStartAt, SrcEndAt: integer;
  sMsg: string;
begin
  if FileExists(fFilename) then begin
    if MessageDlg(fFilename + ' exists. Are you sure you want to overwrite it?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      Exit;
    DeleteFile(fFilename);
  end;

  SetFilename(fFilename);
  SetDevName(StringReplace(ExtractFileName(fFilename), DEV_EXT, '', []));
  WriteDefaultEntries;

  // locate selected target
  if not LocateTarget(StartAt, EndAt) then begin
    sMsg := Format(Lang[ID_MSVC_MSG_CANTLOCATETARGET], [cmbConf.Text]);
    MessageDlg(sMsg, mtError, [mbOK], 0);
    Exit;
  end;

//  WriteDev('Project', 'Type', '0');
  ReadProjectType;
  ReadCompilerOptions(StartAt, EndAt);
  ReadLinkerOptions(StartAt, EndAt);
  LocateSourceTarget(SrcStartAt, SrcEndAt);
  ReadSourceFiles(SrcStartAt, SrcEndAt);
  if fInvalidFiles = '' then
    sMsg := Lang[ID_MSVC_MSG_SUCCESS]
  else
    sMsg := 'Some files belonging to project could not be located.'#13#10 +
      'Please locate them and add them to the project manually...'#13#10#13#10 +
      fInvalidFiles + #13#10'Project created with errors. Do you want to open it?';
  if MessageDlg(sMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ModalResult := mrOk
  else
    Close;
end;

procedure TImportMSVCForm.ReadProjectType();
var
  I: integer;
  P: PChar;
begin
  I := 0;
  while I < fSL.Count do begin
    if AnsiStartsText('# TARGTYPE', fSL[I]) then begin
      // got it
      P := PChar(Copy(fSL[I], Length(fSL[I]) - 5, 7));
      if (P = '0x0102' ) then // "Win32 (x86) Dynamic-Link Library"
        WriteDev('Project', 'Type', '3')
      else if( P = '0x0103' ) then // "Win32 (x86) Console Application"
        WriteDev('Project', 'Type', '1')
      else if( P = '0x0104' ) then // "Win32 (x86) Static Library"
        WriteDev('Project', 'Type', '2')
      else // unknown
        WriteDev('Project', 'Type', '0');
      Break;
    end;
    Inc(I);
  end;
end;

procedure TImportMSVCForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TImportMSVCForm.btnBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filter := FLT_MSVCPROJECTS;
  OpenDialog1.Title := Lang[ID_MSVC_SELECTMSVC];
  if OpenDialog1.Execute then begin
    fFileName := StringReplace(OpenDialog1.FileName, ExtractFileExt(OpenDialog1.FileName), DEV_EXT, []);
    txtVC.Text := OpenDialog1.FileName;
    txtDev.Text := fFilename;
    ImportFile(OpenDialog1.FileName);
  end;
  UpdateButtons;
end;

function TImportMSVCForm.GetFilename: string;
begin
  Result := fFilename;
end;

procedure TImportMSVCForm.txtDevChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TImportMSVCForm.btnBrowseDevClick(Sender: TObject);
begin
  SaveDialog1.Filter := FLT_PROJECTS;
  SaveDialog1.Title := Lang[ID_MSVC_SELECTDEV];
  if SaveDialog1.Execute then
    txtDev.Text := SaveDialog1.Filename;
end;

procedure TImportMSVCForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_MSVC_MENUITEM];
  lbSelect.Caption := Lang[ID_MSVC_SELECTMSVC] + ':';
  lbConf.Caption := Lang[ID_MSVC_CONFIGURATION] + ':';
  lbDev.Caption := Lang[ID_MSVC_SELECTDEV] + ':';
  gbOptions.Caption := '  ' + Lang[ID_MSVC_OPTIONS] + '  ';
  btnImport.Caption := Lang[ID_BTN_IMPORT];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
end;

function TImportMSVCForm.CheckVersion: boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to fSL.Count - 1 do
    if AnsiContainsStr(fSL[I], 'Format Version 6.00') then begin
      Result := True;
      Break;
    end;
  if not Result then
    MessageDlg('This file''s version is not one that can be imported...', mtWarning, [mbOK], 0);
end;

end.

