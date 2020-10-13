unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.StdCtrls;

type
  TMySysButtonStyleHook = class(TSysButtonStyleHook)
  protected
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
  end;

type
  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    FindDialog1: TFindDialog;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FindDialog1.Execute();
end;

{ TMySysButtonStyleHook }

constructor TMySysButtonStyleHook.Create(AHandle: THandle);
begin
  inherited;
//  {$IF CompilerVersion > 23}
//  StyleElements := [sefont];
//  {$ELSE}
//  OverrideFont:=True;
//  {$IFEND}
end;

procedure TMySysButtonStyleHook.UpdateColors;
begin
  inherited;
  FontColor := clGreen;
end;

initialization

TSysStyleManager.RegisterSysStyleHook('Button', TMySysButtonStyleHook);

finalization

TSysStyleManager.UnRegisterSysStyleHook('Button', TMySysButtonStyleHook);

end.
