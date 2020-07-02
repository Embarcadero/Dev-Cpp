unit Aboutfrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TfrmAbout = class(TForm)
    Panel3: TPanel;
    Image2: TImage;
    Bevel1: TBevel;
    lblcopyright: TLabel;
    lbllicense: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Bevel2: TBevel;
    btnAcept: TBitBtn;
    Label10: TLabel;
    imgLogo: TImage;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then
    close;
end;

end.
