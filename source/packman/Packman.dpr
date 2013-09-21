program Packman;

uses
{$IFDEF WIN32}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  InstallWizards in 'InstallWizards.pas' {InstallWizard},
  InstallFiles in 'InstallFiles.pas',
  Installers in 'Installers.pas',
  Main in 'Main.pas' {MainForm},
  PackmanUtils in 'PackmanUtils.pas',
  RemoveForms in 'RemoveForms.pas' {RemoveForm},
  VerifyForms in 'VerifyForms.pas' {VerifyForm},
  DetailsForms in 'DetailsForms.pas' {DetailsForm},
  AboutForms in 'AboutForms.pas' {AboutForm},
  BZip2 in 'bzip2.pas',
  LibTar in 'LibTar.pas',
  ExtractionProgressDialog in 'ExtractionProgressDialog.pas' {ExtractionProgress},
  PackmanExitCodesU in 'PackmanExitCodesU.pas',
  ExceptionsAnalyzer in 'ExceptionsAnalyzer.pas' {frmExceptionsAnalyzer},
  Unzip in 'unzip\UNZIP.PAS',
  ziptypes in 'unzip\ZIPTYPES.PAS';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Package Manager';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
