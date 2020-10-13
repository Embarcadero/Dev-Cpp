program PackMaker;

uses
  Forms,
  main in 'main.pas' {MainForm},
  LibTar in 'LibTar.pas',
  bzip2 in 'bzip2.pas',
  menufrm in 'menufrm.pas' {MenuForm},
  filefrm in 'filefrm.pas' {FileForm},
  buildfrm in 'buildfrm.pas' {BuildForm},
  actionfrm in 'actionfrm.pas' {ActionForm},
  Aboutfrm in 'Aboutfrm.pas' {frmAbout},
  Config in 'Config.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Dev-C++ Package Maker';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
