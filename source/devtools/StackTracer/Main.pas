unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MapFile: string;

implementation

uses MapReader;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    MapFile := OpenDialog1.FileName;
  Label2.Caption := MapFile;
  ClearModules;
  ReadMapFile(MapFile);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Label2.Caption := '';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S: string;
  map: string;
begin
  Screen.Cursor := crHourglass;
  Memo2.Lines.BeginUpdate;
  Memo2.Lines.Clear;
  map := Memo1.Lines.Text;
  map := StringReplace(map, #13, '', [rfReplaceAll]);
  map := StringReplace(map, #10, '', [rfReplaceAll]);
  while map <> '' do begin
    S := '$' + Copy(map, 1, 8);
    Delete(map, 1, 8);
    S := AddressInfo(StrToIntDef(S, -1));
    if S <> '' then
      Memo2.Lines.Add(S);
  end;
  Memo2.Lines.EndUpdate;
  Screen.Cursor := crDefault;
end;

end.

