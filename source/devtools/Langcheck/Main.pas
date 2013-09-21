unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ShellAPI;

type
  TForm1 = class(TForm)
    Label2: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    Label3: TLabel;
    Edit2: TEdit;
    SpeedButton2: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Start: TBitBtn;
    Cancel: TBitBtn;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure CancelClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure StartCheck;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.StartClick(Sender: TObject);
begin
  if not FileExists(Edit1.Text) then
      Application.MessageBox(PChar('File ''' + Edit1.Text +
        ''' does not exists!'), 'Error',
        MB_ICONHAND)
  else if not FileExists(Edit2.Text) then
      Application.MessageBox(PChar('File ''' + Edit2.Text +
        ''' does not exists!'), 'Error',
        MB_ICONHAND)
  else
      StartCheck;
end;

procedure LoadList(var List: TStringList; FileName: String);
var
  F: TextFile;
  Line: String;
  i: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  while not EOF(F) do
  begin
      Readln(F, Line);
      i := Pos('=', Line);
      if i > 0 then
          List.Add(Line);
  end;
  CloseFile(F);
end;

procedure TForm1.StartCheck;
var
  i, MissingCount, UntranslatedCount: Integer;
  English: TStringList;
  Translation: TStringList;
begin
  Memo1.Clear;

  English := TStringList.Create;
  Translation := TStringList.Create;
  English.CaseSensitive := False;
  Translation.CaseSensitive := False;
  Start.Enabled := False;
  Cancel.Enabled := False;
  LoadList(English, Edit1.Text);
  LoadList(Translation, Edit2.Text);
  Application.ProcessMessages;

  MissingCount := 0;
  if CheckBox1.Checked then
  begin
      Memo1.Lines.Add('*** Missing entries:');

      Memo1.Lines.BeginUpdate;
      for i := 0 to English.Count - 1 do
      begin
          if Translation.IndexOfName(English.Names[i]) = -1 then
          begin
              Memo1.Lines.Add(English.Strings[i]);
              Inc(MissingCount);
          end;
      end;
      Memo1.Lines.EndUpdate;
  end;

  UntranslatedCount := 0;
  if CheckBox2.Checked then
  begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('*** Possibly untranslated strings:');
      English.CaseSensitive := True;
      Translation.CaseSensitive := True;

      Memo1.Lines.BeginUpdate;
      for i := 0 to English.Count - 1 do
      begin
          if Translation.IndexOf(English.Strings[i]) <> -1 then
          begin
              Memo1.Lines.Add(English.Strings[i]);
              Inc(UntranslatedCount);
          end;
      end;
      Memo1.Lines.EndUpdate;
  end;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('**** Check completed.');
  if CheckBox1.Checked then
      Memo1.Lines.Add(IntToStr(MissingCount) + ' missing entries found.');
  if CheckBox2.Checked then
      Memo1.Lines.Add(IntToStr(UntranslatedCount) + ' possibly untranslated string found.');

  Application.ProcessMessages;
  Start.Enabled := True;
  Cancel.Enabled := True;
  English.Free;
  Translation.Free;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then Edit1.Text := OpenDialog1.FileName;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then Edit2.Text := OpenDialog1.FileName;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  if not FileExists(Edit1.Text) then
      Application.MessageBox(PChar('File ''' + Edit1.Text +
        ''' does not exists!'), 'Error',
        MB_ICONHAND)
  else
      ShellExecute(Handle, nil, 'notepad', PChar(Edit1.Text), nil, SW_MAXIMIZE);
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  if not FileExists(Edit1.Text) then
      Application.MessageBox(PChar('File ''' + Edit2.Text +
        ''' does not exists!'), 'Error',
        MB_ICONHAND)
  else
      ShellExecute(Handle, nil, 'notepad', PChar(Edit2.Text), nil, SW_MAXIMIZE);
end;

end.
