unit UBenchmark;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGInterfaces,
  SVGIconImageCollection, SVGIconImage, Vcl.Samples.Spin;

type
  TfrmBenchmark = class(TForm)
    SVGIconImageCollection: TSVGIconImageCollection;
    imlIcons: TVirtualImageList;
    memOutput: TMemo;
    btnClear: TButton;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    SVGIconImage: TSVGIconImage;
    btnRunBenchmark: TButton;
    speLoops: TSpinEdit;
    lblLoops: TLabel;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRunBenchmarkClick(Sender: TObject);
  private
    FImageStream: TMemoryStream;
    FStartTick  : Int64;
    FLastTick   : Int64;
    procedure LogTicks(var AMessage: string; const ASuffix: string; ATick: Int64);
    procedure RunBenchmark(AFactoryName: string; AFactory: ISVGFactory);
  public
    { Public-Deklarationen }
  end;

var
  frmBenchmark: TfrmBenchmark;

implementation

uses
  CairoSVGFactory,
  D2DSVGFactory,
  PasSVGFactory,
  System.Types;

{$R *.dfm}


procedure TfrmBenchmark.btnClearClick(Sender: TObject);
begin
  memOutput.Clear;
  SVGIconImageCollection.ClearIcons;
end;

procedure TfrmBenchmark.btnLoadClick(Sender: TObject);
var
  LSvg: ISvg;
begin
  if OpenDialog.Execute then
    begin
      FImageStream.LoadFromFile(OpenDialog.FileName);
      FImageStream.Position := 0;

      SVGIconImageCollection.SVGIconItems.Clear;
      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.LoadFromStream(FImageStream);
      SVGIconImageCollection.Add(LSvg, '');
      SVGIconImage.ImageIndex := 0;
    end;
end;

procedure TfrmBenchmark.btnRunBenchmarkClick(Sender: TObject);
begin
  if (FImageStream.Size = 0) then
    memOutput.Lines.Add('Please load a SVG image first')
  else
    begin
      SVGIconImage.ImageIndex := -1;

      memOutput.Lines.Add(Format('Factory    | Load %3d | Draw %3d |   Total  |', [speLoops.Value, speLoops.Value]));
      RunBenchmark('Direct 2D', GetD2DSVGFactory);
      RunBenchmark('Pascal', GetPasSVGFactory);
      RunBenchmark('Cairo', GetCairoSVGFactory);

      SVGIconImage.ImageIndex := 0;
    end;
end;

procedure TfrmBenchmark.FormCreate(Sender: TObject);
begin
  FImageStream := TMemoryStream.Create;
end;

procedure TfrmBenchmark.FormDestroy(Sender: TObject);
begin
  FImageStream.Free;
end;

procedure TfrmBenchmark.LogTicks(var AMessage: string; const ASuffix: string; ATick: Int64);
var
  LCurrentTick: Int64;
begin
  LCurrentTick := GetTickCount;
  AMessage := Format('%s | %8d%s', [AMessage, LCurrentTick - ATick, ASuffix]);
  memOutput.Lines[memOutput.Lines.Count - 1] := AMessage;
  FLastTick := LCurrentTick;
end;

type
  TCanvasImage = class(TSVGIconImage)
  public
    property Canvas;
  end;

procedure TfrmBenchmark.RunBenchmark(AFactoryName: string; AFactory: ISVGFactory);
var
  LSvg : ISvg;
  I    : integer;
  LStep: real;
  LSize: real;
  LRect: TRect;
  LLine: string;
begin
  LLine := Format('%-10s', [AFactoryName]);
  memOutput.Lines.Add(LLine);

  FStartTick := GetTickCount;
  FLastTick := FStartTick;

  SVGIconImageCollection.SVGIconItems.BeginUpdate;
  try
    SVGIconImageCollection.SVGIconItems.Clear;
    for I := 1 to speLoops.Value do
      begin
        LSvg := AFactory.NewSvg;
        FImageStream.Position := 0;
        LSvg.LoadFromStream(FImageStream);
        SVGIconImageCollection.Add(LSvg, '');
      end;
  finally
    SVGIconImageCollection.SVGIconItems.EndUpdate;
  end;

  LogTicks(LLine, '', FLastTick);

  LStep := SvgIconImage.Height / 100;
  LSize := 0;
  for I := 1 to speLoops.Value do
    begin
      LSize := LSize + LStep;
      LRect := TRect.Create(0, 0, Round(LSize), Round(LSize));
      SvgIconImageCollection.Draw(TCanvasImage(SVGIconImage).Canvas, LRect, 0);
    end;
  LogTicks(LLine, '', FLastTick);
  LogTicks(LLine, ' |', FStartTick);
end;

end.
