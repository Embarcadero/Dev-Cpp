program SVGExplorer;

uses
  Vcl.Forms,
  FExplorerSVG in 'FExplorerSVG.pas' {fmExplorerSVG};

{$R *.res}

begin
  Application.Title := 'SVG Icons Explorer - Copyright (c) 2020 Ethea S.r.l.';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmExplorerSVG, fmExplorerSVG);
  Application.Run;
end.
