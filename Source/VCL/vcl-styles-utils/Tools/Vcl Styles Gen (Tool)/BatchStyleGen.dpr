// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program BatchStyleGen;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  Vcl.Styles.Utils.Graphics in '..\..\Common\Vcl.Styles.Utils.Graphics.pas',
  Vcl.Styles.Ext in '..\..\Common\Vcl.Styles.Ext.pas',
  Vcl.Styles.Utils in '..\..\Common\Vcl.Styles.Utils.pas',
  PngFunctions in '..\Extras\PngFunctions.pas',
  PngImageList in '..\Extras\PngImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
