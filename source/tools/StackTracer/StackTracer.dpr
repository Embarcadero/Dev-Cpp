program StackTracer;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  MapReader in 'MapReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
