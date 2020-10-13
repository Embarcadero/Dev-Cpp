program IconPickerFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UIconPickerFMX in '..\Source\UIconPickerFMX.pas' {IconPicker},
  BitmapCodecSVG in '..\..\Svg\BitmapCodecSVG.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIconPicker, IconPicker);
  Application.Run;
end.
