// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program FoldingDemo;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {FormFoldingDemo},
  Vcl.Themes,
  uHighlighterProcs in '..\uHighlighterProcs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormFoldingDemo, FormFoldingDemo);
  Application.Run;
end.
