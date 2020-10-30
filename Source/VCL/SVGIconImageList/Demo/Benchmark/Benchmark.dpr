program Benchmark;

uses
  Vcl.Forms,
  UBenchmark in 'UBenchmark.pas' {frmBenchmark};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SVG Icons - Factories Benchmark';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBenchmark, frmBenchmark);
  Application.Run;
end.
