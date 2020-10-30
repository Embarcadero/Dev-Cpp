{-----------------------------------------------------------------------------
 Unit Name: UBenchmark
 Author:    Lübbe Onken
 Purpose:   Main form for SVG Factories Benchmark
 History:
-----------------------------------------------------------------------------}
unit UBenchmark;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.ImageList, //if you are compiling with older version than XE7 remove this line
  Vcl.ImgList,
  SVGInterfaces,
  SVGIconImageCollection, SVGIconImage, Vcl.Samples.Spin, Vcl.ExtCtrls,
  SVGIconImageListBase, SVGIconVirtualImageList;

type
  TfrmBenchmark = class(TForm)
    SVGIconImageCollection: TSVGIconImageCollection;
    memOutput: TMemo;
    btnClear: TButton;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    SVGIconImage: TSVGIconImage;
    btnRunBenchmark: TButton;
    speLoops: TSpinEdit;
    lblLoops: TLabel;
    pnlButtons: TPanel;
    chkGrayScale: TCheckBox;
    chkFixedColor: TCheckBox;
    splHorizontal: TSplitter;
    pnlLoops: TPanel;
    grpFactory: TRadioGroup;
    chkDrawVisible: TCheckBox;
    SVGIconVirtualImageList: TSVGIconVirtualImageList;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnRunBenchmarkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grpFactoryClick(Sender: TObject);
  private
    FSvgSource  : string;
    FStartTick  : Int64;
    FLastTick   : Int64;
    FLine       : string;
    FInBenchmark: boolean;

    function GetFactoryName: string;
    procedure SetFactory(AIndex: integer);

    procedure BenchmarkLoad;
    procedure BenchmarkGrayScale;
    procedure BenchmarkFixedColor;
    procedure BenchmarkDraw;

    procedure LogTicks(var AMessage: string; ATick: Int64);
    procedure PrepareBenchmark(ACaption: string);
    procedure ReloadImage;
    procedure RunBenchmark(AIndex: integer);
  end;

var
  frmBenchmark: TfrmBenchmark;

implementation

uses
  CairoSVGFactory,
  D2DSVGFactory,
  PasSVGFactory,
  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.TypInfo,
  System.Types;

{$R *.dfm}


type
  TCanvasImage = class(TSVGIconImage) // Just to have something to paint on
  public
    property Canvas;
  end;

procedure TfrmBenchmark.BenchmarkDraw;

  procedure DrawOnCanvas(ACanvas: TCanvas);
  var
    I    : integer;
    LStep: real;
    LSize: real;
    LRect: TRect;
  begin
    LStep := Min(ACanvas.ClipRect.Width, ACanvas.ClipRect.Height) / speLoops.Value;
    LSize := 0;

    for I := 1 to speLoops.Value do
      begin
        LSize := LSize + LStep;
        LRect := TRect.Create(0, 0, Round(LSize), Round(LSize));

        SvgIconImageCollection.Draw(ACanvas, LRect, 0, true);
      end;
  end;

var
  LBitmap: TBitmap;
begin
  // Benchmark Draw
  if chkDrawVisible.Checked then
    DrawOnCanvas(TCanvasImage(SVGIconImage).Canvas)
  else
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width := SvgIconImage.Height;
        LBitmap.Width := SvgIconImage.Width;

        DrawOnCanvas(LBitmap.Canvas);
      finally
        LBitmap.Free;
      end;
    end;
end;

procedure TfrmBenchmark.BenchmarkFixedColor;
begin
  SVGIconImageCollection.FixedColor := clLime;
end;

procedure TfrmBenchmark.BenchmarkGrayScale;
begin
  SVGIconImageCollection.GrayScale := true;
end;

procedure TfrmBenchmark.BenchmarkLoad;
var
  I   : integer;
  LSvg: ISvg;
begin
  SVGIconImageCollection.SVGIconItems.BeginUpdate;
  try
    SVGIconImageCollection.SVGIconItems.Clear;
    SVGIconImageCollection.FixedColor := clDefault;
    SVGIconImageCollection.GrayScale := false;

    for I := 1 to speLoops.Value do
      begin
        LSvg := GlobalSvgFactory.NewSvg;
        LSvg.Source := FSvgSource;
        SVGIconImageCollection.Add(LSvg, '');
      end;
  finally
    SVGIconImageCollection.SVGIconItems.EndUpdate;
  end;
end;

procedure TfrmBenchmark.btnClearClick(Sender: TObject);
begin
  memOutput.Clear;
end;

procedure TfrmBenchmark.btnLoadClick(Sender: TObject);
var
  LSvg: ISvg;
begin
  if OpenDialog.Execute then
    begin
      FSvgSource := TFile.ReadAllText(OpenDialog.FileName);

      PrepareBenchmark('Factory    |  Load  |  Draw  | Total');

      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.Source := FSvgSource;
      LogTicks(FLine, FLastTick);

      SVGIconImageCollection.SVGIconItems.Clear;
      SVGIconImageCollection.Add(LSvg, '');
      SVGIconImage.ImageIndex := 0;

      LogTicks(FLine, FLastTick);
      LogTicks(FLine, FStartTick);
    end;
end;

procedure TfrmBenchmark.btnRunBenchmarkClick(Sender: TObject);
var
  LLine: string;
begin
  if (FSvgSource = '') then
    memOutput.Lines.Add('Please load a SVG image first')
  else
    begin
      FInBenchmark := true;
      try
        SVGIconImage.ImageIndex := -1;

        LLine := 'Factory    |  Load  |  Draw  ';
        if chkGrayScale.Checked then
          LLine := LLine + '|  Gray  |  Draw  ';
        if chkFixedColor.Checked then
          LLine := LLine + '|  Fixed |  Draw  ';
        LLine := LLine + '|  Total';

        memOutput.Lines.Add('');
        memOutput.Lines.Add(Format('Benchmark: Repeat %d times. Draw %svisible.', [speLoops.Value, IfThen(chkDrawVisible.Checked, '', 'in')]));

        memOutput.Lines.Add(LLine);
        RunBenchmark(0);
        RunBenchmark(1);
        RunBenchmark(2);

        SVGIconImage.ImageIndex := 0;
      finally
        FInBenchmark := false;
      end;
    end;
end;

procedure TfrmBenchmark.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  FInBenchmark := false;
  grpFactory.Items.Add('Pascal');
  grpFactory.Items.Add('Direct 2D');
  grpFactory.Items.Add('Cairo');
  SetFactory(0);
end;

function TfrmBenchmark.GetFactoryName: string;
begin
  if grpFactory.ItemIndex > -1 then
    Result := grpFactory.Items[grpFactory.ItemIndex]
  else
    Result := '';
end;

procedure TfrmBenchmark.grpFactoryClick(Sender: TObject);
begin
  if not FInBenchmark then
    begin
      SetFactory(grpFactory.ItemIndex);
      ReloadImage;
    end;
end;

procedure TfrmBenchmark.LogTicks(var AMessage: string; ATick: Int64);
var
  LCurrentTick: Int64;
begin
  LCurrentTick := GetTickCount;
  AMessage := Format('%s | %6d', [AMessage, LCurrentTick - ATick]);
  memOutput.Lines[memOutput.Lines.Count - 1] := AMessage;
  FLastTick := LCurrentTick;
end;

procedure TfrmBenchmark.PrepareBenchmark(ACaption: string);
begin
  if ACaption <> '' then
    memOutput.Lines.Add(ACaption);

  FLine := Format('%-10s', [GetFactoryName]);
  memOutput.Lines.Add(FLine);

  FStartTick := GetTickCount;
  FLastTick := FStartTick;
end;

procedure TfrmBenchmark.ReloadImage;
var
  LSvg: ISvg;
begin
  if FSvgSource <> '' then
    begin
      PrepareBenchmark('Factory    |  Load  |  Draw  | Total');

      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.Source := FSvgSource;
      LogTicks(FLine, FLastTick);

      SVGIconImage.ImageIndex := -1;
      SVGIconImageCollection.SVGIconItems.Clear;
      SVGIconImageCollection.Add(LSvg, '');
      SVGIconImage.ImageIndex := 0;
      LogTicks(FLine, FLastTick);
      LogTicks(FLine, FStartTick);
    end;
end;

procedure TfrmBenchmark.RunBenchmark(AIndex: integer);

  procedure Benchmark(AProc: TPRoc);
  begin
    if Assigned(AProc) then
      begin
        AProc;
        LogTicks(FLine, FLastTick);
      end;
  end;

begin
  SetFactory(AIndex);

  PrepareBenchmark('');

  Benchmark(BenchmarkLoad);
  Benchmark(BenchmarkDraw);

  if chkGrayScale.Checked then
    begin
      Benchmark(BenchmarkGrayScale);
      Benchmark(BenchmarkDraw);
    end;

  if chkFixedColor.Checked then
    begin
      Benchmark(BenchmarkFixedColor);
      Benchmark(BenchmarkDraw);
    end;

  LogTicks(FLine, FStartTick);
end;

procedure TfrmBenchmark.SetFactory(AIndex: integer);
begin
  case AIndex of
    0:
      SetGlobalSvgFactory(GetPasSVGFactory);
    1:
      SetGlobalSvgFactory(GetD2DSVGFactory);
    2:
      SetGlobalSvgFactory(GetCairoSVGFactory);
  end;
  grpFactory.ItemIndex := AIndex;
end;

end.
