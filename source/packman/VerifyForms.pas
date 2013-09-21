unit VerifyForms;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, IniFiles;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, QButtons, QComCtrls, IniFiles;
{$ENDIF}

type
  TVerifyForm = class(TForm)
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    MissingFiles: TStringList;
    Abort: Boolean;
  public
    Entry: String;
  end;

var
  VerifyForm: TVerifyForm;

implementation

uses
  DetailsForms, PackmanUtils;

{$R *.dfm}

procedure TVerifyForm.Timer1Timer(Sender: TObject);
var
  Ini: TIniFile;
  Files: TStringList;
  i, IMod: Integer;
begin
  Timer1.Enabled := False;
  Abort := False;

  { Read entry information }
  Ini := TIniFile.Create(Entry);
  Caption := Format(Caption,
    [Ini.ReadString('Setup', 'AppName', '')]);
  Ini.Free;

  Files := TStringList.Create;
  Files.LoadFromFile(Entry);
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

  IMod := CalcMod(Files.Count);
  ProgressBar1.Max := Files.Count;
  for i := 0 to Files.Count - 1 do
  begin
      if Abort then
      begin
          Abort := True;
          Exit;
      end;

      if not FileExists(Files.Strings[i]) then
          MissingFiles.Add(Files.Strings[i]);

      if (IMod = 0) or (i mod Imod = 0) then
      begin
          GroupBox1.Caption := 'Progress (' +
            IntToStr(Round(i / (Files.Count / 100)) + 1) + '%)';
          ProgressBar1.Position := i + 1;
          Application.ProcessMessages;
      end;
  end;

  BitBtn1.Kind := bkOK;
  BitBtn1.Default := True;
  BorderIcons := BorderIcons + [biSystemMenu];
  Caption := 'Finished';
  Label1.Caption := 'The verification has finished. ';
  GroupBox1.Caption := 'Progress (100%)';
  if MissingFiles.Count > 0 then
  begin
      Label1.Caption := Label1.Caption + 'Some files are missing. ' +
        'Please click on Details for more information.';
      BitBtn2.Enabled := True;
      ActiveControl := BitBtn2;
  end else
      Label1.Caption := Label1.Caption + 'No errors were detected.';
end;

procedure TVerifyForm.FormDestroy(Sender: TObject);
begin
  MissingFiles.Free;
  MissingFiles := nil;
end;

procedure TVerifyForm.FormCreate(Sender: TObject);
begin
  MissingFiles := TStringList.Create;
end;

procedure TVerifyForm.BitBtn1Click(Sender: TObject);
begin
  Abort := True;
end;

procedure TVerifyForm.BitBtn2Click(Sender: TObject);
begin
  DetailsForm := TDetailsForm.Create(Self);
  with DetailsForm do
  try
     Memo1.Lines.Add('The following files are missing:');
     Memo1.Lines.AddStrings(MissingFiles);
     ShowModal;
  finally
     Free;
  end;
end;

end.
