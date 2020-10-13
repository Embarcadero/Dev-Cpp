unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm25 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

uses
 Vcl.Styles.Utils.SystemMenu;

{$R *.dfm}

procedure TForm25.FormCreate(Sender: TObject);
var
  LVclStylesSystemMenu : TVclStylesSystemMenu;
begin
  LVclStylesSystemMenu:=TVclStylesSystemMenu.Create(Self);
  LVclStylesSystemMenu.MenuCaption:='Choose a VCL Style';

  ReportMemoryLeaksOnShutdown:=True;
end;

end.
