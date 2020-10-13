unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Themes,
  Vcl.Mask, Vcl.ExtCtrls, Vcl.DBCtrls;

type
  TFrmMain = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    MaskEdit1: TMaskEdit;
    LabeledEdit1: TLabeledEdit;
    ButtonedEdit1: TButtonedEdit;
    ButtonedEdit2: TButtonedEdit;
    LabeledEdit2: TLabeledEdit;
    MaskEdit2: TMaskEdit;
    Button1: TButton;
    DBMemo1: TDBMemo;
    procedure Button1Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  Vcl.Styles,
  Vcl.Styles.ControlColor;


procedure TFrmMain.Button1Click(Sender: TObject);
begin
 Edit1.Color:=clBlue;
end;



initialization
 TStyleManager.Engine.RegisterStyleHook(TEdit, TEditStyleHookColor);
 TStyleManager.Engine.RegisterStyleHook(TButtonedEdit, TEditStyleHookColor);
 TStyleManager.Engine.RegisterStyleHook(TMaskEdit, TEditStyleHookColor);
 TStyleManager.Engine.RegisterStyleHook(TLabeledEdit, TEditStyleHookColor);

 TStyleManager.Engine.RegisterStyleHook(TMemo, TMemoStyleHookColor);
 TStyleManager.Engine.RegisterStyleHook(TDBMemo, TMemoStyleHookColor);

end.

