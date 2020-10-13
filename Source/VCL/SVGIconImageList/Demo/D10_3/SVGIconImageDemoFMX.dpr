program SVGIconImageDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  USVGIconImageFMX in '..\Source\USVGIconImageFMX.pas' {SVGIconImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSVGIconImageForm, SVGIconImageForm);
  Application.Run;
end.
