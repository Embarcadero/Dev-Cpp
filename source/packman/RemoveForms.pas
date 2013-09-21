unit RemoveForms;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, IniFiles, ExtCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, QStdCtrls, QButtons, IniFiles, QExtCtrls;
{$ENDIF}

type
  TRemoveForm = class(TForm)
    Animate1: TAnimate;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    Image1: TImage;
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    Abort: Boolean;
    Entry: String;
    CloseWhenDone: Boolean;
    ReqVersion: String;
  end;

var
  RemoveForm: TRemoveForm;

implementation

uses
  PackmanUtils;

{$R *.dfm}

procedure RemoveDirsRec(dir: String);
//this file removes all empty directories up to the root of dev-c++
//for example if files are installed only in c:\dev-cpp\a\b\c\*.*
//it will attempt to delete c, b, a if not empty
var
  strDevRoot: String;
  tempDirs: TStrings;
  i: Integer;
begin
{$MESSAGE 'this will not work in Linux'}
  strDevRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  //make relative path if file is within dev-c++ root
  if Pos(strDevRoot, dir) = 1 then
    Delete(dir, 1, Length(strDevRoot));

  if Length(dir) < 1 then
    Exit
  else if Length(dir) = 1 then
  begin
    RemoveDir(dir);
    Exit;
  end
  else if (dir[1] = '\') or (dir[2] = ':') then
  begin
    //full path outside of dev-c++ root
    RemoveDir(dir);
    Exit;
  end;

  if Copy(dir, length(dir), 1) = '\' then
    Delete(dir, Length(dir), 1);

  tempDirs := TStringList.Create;
  with tempDirs do
  begin
    Delimiter := '\';
    DelimitedText := dir;

    for i := Count -1 downto 0 do
    begin
      RemoveDir(DelimitedText);
      Delete(i);
    end;

    Free;
  end;

end;

procedure TRemoveForm.Timer1Timer(Sender: TObject);
var
  Ini: TIniFile;
  Files, Dirs: TStringList;
  i, IMod: Integer;
  currdir: String;
begin
  Timer1.Enabled := False;
  Abort := False;

  Label1.Caption := 'Reading file list...';
  Application.ProcessMessages;

  { Read entry information }
  Ini := TIniFile.Create(Entry);
  Caption := Format(Caption,
    [Ini.ReadString('Setup', 'AppName', '')]);
  if (ReqVersion = '')
  or (ReqVersion = Ini.ReadString('Setup', 'AppVersion', '')) then
  begin

    Files := TStringList.Create;
    Files.LoadFromFile(Entry);
    Dirs := TStringList.Create;
    for i := 0 to Files.Count - 1 do
    begin
      if CompareText(Files.Strings[0], '[Files]') = 0 then
      begin
        Files.Delete(0);
        Break;
      end else
        Files.Delete(0);
    end;
    Application.ProcessMessages;
    ProgressBar1.Max := Files.Count;
    Label5.Caption := IntToStr(Files.Count);

    { Delete the files }
    IMod := CalcMod(Files.Count);

    currdir := GetCurrentDir;
    SetCurrentDir(ExtractFilePath(ParamStr(0)));

    for i := 0 to Files.Count - 1 do
    begin
      if Abort then
      begin
        Abort := False;
        BorderIcons := BorderIcons + [biSystemMenu];
        Caption := 'Aborted';
        Label1.Caption := 'Aborted.';
        Animate1.Active := False;
        BitBtn1.Kind := bkClose;
        BitBtn1.Default := True;
        BitBtn1.OnClick := nil;
        Exit;
      end;

      Label1.Caption := 'Deleting file ' + ExtractFileName(Files.Strings[i]);
      DeleteFile(Files.Strings[i]);
      if Dirs.IndexOf(ExtractFileDir(Files.Strings[i])) = -1 then
        Dirs.Add(ExtractFileDir(Files.Strings[i]));

      if (IMod = 0) or (i mod IMod = 0) then
      begin
        ProgressBar1.Position := i + 1;
        GroupBox1.Caption := 'Progress (' +
          IntToStr(Round(i / (Files.Count / 100)) + 1) + '%)';
        Label6.Caption := IntToStr(i + 1);
        Label7.Caption := IntToStr(Files.Count - i - 1);
        Application.ProcessMessages;
      end;
    end;

    Label1.Caption := 'Removing empty directories...';
    Dirs.CaseSensitive := False;
    for i := Dirs.Count - 1 downto 0 do
    begin
      if Abort then
      begin
        Abort := False;
        BorderIcons := BorderIcons + [biSystemMenu];
        Caption := 'Aborted';
        Label1.Caption := 'Aborted.';
        Animate1.Active := False;
        BitBtn1.Kind := bkClose;
        BitBtn1.Default := True;
        BitBtn1.OnClick := nil;
        Exit;
      end;
      RemoveDirsRec(Dirs.Strings[i]);

      if (IMod = 0) or (i mod IMod = 0) then
        Application.ProcessMessages;
    end;

    SetCurrentDir(currdir);

    Label1.Caption := 'Removing package entry...';
    DeleteFile(Entry);
    Application.ProcessMessages;

    Caption := 'Finished removing ' + Ini.ReadString('Setup', 'AppName', '');
    GroupBox1.Caption := 'Progress (100%)';
    Label1.Caption := 'Finished.';
    Animate1.Active := False;
    BorderIcons := BorderIcons + [biSystemMenu];
    Abort := False;
    Ini.Free;
    Files.Free;
    Dirs.Free;
  end; //if ReqVersion

  BitBtn1.Kind := bkOK;
  BitBtn1.OnClick := nil;
  if CloseWhenDone then
  begin
      ModalResult := mrOK;
      Close;
  end;
end;

procedure TRemoveForm.BitBtn1Click(Sender: TObject);
begin
  if Application.MessageBox('Do you really want to abort removal?' + #13#10 +
    'Because some files are already deleted, the package may not function ' +
    'correctly.', 'Confirmation', MB_ICONQUESTION + MB_YESNO) = 6 then
      Abort := True;
end;

procedure TRemoveForm.FormCreate(Sender: TObject);
begin
  CloseWhenDone := False;
  ReqVersion := '';
end;

end.
