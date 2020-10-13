unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.Forms, Vcl.StdCtrls;

type
  TNoNCSysDialogStyleHook = class(TSysDialogStyleHook)
  public
    constructor Create(AHandle: THandle); override;
  end;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
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
  OpenDialog1.Execute();
end;

{ TNoNCSysDialogStyleHook }

constructor TNoNCSysDialogStyleHook.Create(AHandle: THandle);
begin
 inherited;
 OverridePaintNC := False;
end;

initialization

TSysStyleManager.RegisterSysStyleHook('#32770', TNoNCSysDialogStyleHook);

finalization

TSysStyleManager.UnRegisterSysStyleHook('#32770', TNoNCSysDialogStyleHook);

end.
