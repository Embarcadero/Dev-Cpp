unit AboutForms;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ShellAPI;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons, QExtCtrls;
{$ENDIF}

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn1: TBitBtn;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure Label7Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.Label7Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow, nil, 'http://www.destructor.de/',
    nil, nil, SW_NORMAL);
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow, nil,
    'http://www.geocities.com/SiliconValley/Grid/3690/index1.html',
    nil, nil, SW_NORMAL);
end;

procedure TAboutForm.Label9Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow, nil, 'http://sources.redhat.com/bzip2/',
    nil, nil, SW_NORMAL);
end;

end.
