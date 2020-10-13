program VisualStylesEQU;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uVclStylesEQU in 'uVclStylesEQU.pas' {FrmHueSat},
  Vcl.Styles.Utils.Graphics in '..\..\Common\Vcl.Styles.Utils.Graphics.pas',
  Vcl.Styles.Utils in '..\..\Common\Vcl.Styles.Utils.pas',
  Vcl.Styles.Ext in '..\..\Common\Vcl.Styles.Ext.pas',
  uVCLStylesInfo in 'uVCLStylesInfo.pas' {FrmVCLStyleInfoDialog},
  PngFunctions in '..\Extras\PngFunctions.pas',
  PngImageList in '..\Extras\PngImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmHueSat, FrmHueSat);
  Application.Run;
end.
