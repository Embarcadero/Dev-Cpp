program PackMaker;

uses
  Forms,
  main in 'main.pas' {MainForm},
  LibTar in 'LibTar.pas',
  BZip2 in 'bzip2.pas',
  menufrm in 'menufrm.pas' {MenuForm},
  filefrm in 'filefrm.pas' {FileForm},
  buildfrm in 'buildfrm.pas' {BuildForm},
  actionfrm in 'actionfrm.pas' {ActionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Dev-C++ Package Maker';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
