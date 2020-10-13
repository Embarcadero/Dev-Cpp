unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TFrmMain = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    ImageVCLStyle: TImage;
    ComboBoxVclStyles: TComboBox;
    Label1: TLabel;
    EditPath: TEdit;
    Label2: TLabel;
    CheckBoxSepia: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxVclStylesChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function GetStyleName: string;
    property StyleName: string Read GetStyleName;
    procedure DrawSeletedVCLStyle;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses
  PngFunctions,
  PngImage,
  IOUtils,
  Generics.Collections,
  Vcl.Styles.Ext,
  Vcl.Styles.Utils,
  Vcl.Styles.Utils.Graphics,
  Vcl.Themes,
  Vcl.Styles;

procedure TFrmMain.Button1Click(Sender: TObject);
const
 HueDelta=10;
Var
  LFilters : TObjectList<TBitmapFilter>;
  VclUtils : TVclStylesUtils;
  Hue      : Integer;
  FileName : String;
  StyleInfo: TStyleInfo;
  LBitmap  : TBitmap;
  LPng     : TPngImage;
begin
  try
    Hue:=-180;
    ProgressBar1.Position:=0;
    ProgressBar1.Max:=360 div HueDelta;
    while Hue<=180 do
    begin
      LFilters:=TObjectList<TBitmapFilter>.Create;
      if CheckBoxSepia.Checked then
      LFilters.Add(TBitmap32SepiaFilter.Create(20));

      LFilters.Add(TBitmap32HueFilter.Create(Hue));
      VclUtils:=TVclStylesUtils.Create(StyleName);
      try
        StyleInfo:=VclUtils.StyleExt.StyleInfo;
        StyleInfo.Name:=StyleName+'.Hue.'+IntToHex(Hue,8);
        FileName:=IncludeTrailingPathDelimiter(EditPath.Text)+StyleInfo.Name+'.vsf';
        VclUtils.StyleExt.StyleInfo:=StyleInfo;
        VclUtils.SetFilters(LFilters);
        VclUtils.SaveToFile(FileName);

        LBitmap:=TBitmap.Create;
        try
         LBitmap.PixelFormat:=pf32bit;
         LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
         LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
         DrawSampleWindow(VclUtils.StyleExt, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleInfo.Name);
         //LBitmap.SaveToFile(ChangeFileExt(FileName,'.bmp'));
         ConvertToPNG(LBitmap, LPng);
         try
           LPng.SaveToFile(ChangeFileExt(FileName,'.png'));
         finally
           LPng.Free;
         end;
        finally
          LBitmap.Free;
        end;

      finally
        LFilters.Free;
        VclUtils.Free;
      end;
      Inc(Hue,HueDelta);
      ProgressBar1.StepBy(1);
    end;

    ShowMessage('Done');
  except
    on E: Exception do
      ShowMessage(Format('Error saving vcl style - Message : %s : Trace %s',
        [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmMain.ComboBoxVclStylesChange(Sender: TObject);
begin
 DrawSeletedVCLStyle;
end;

procedure TFrmMain.DrawSeletedVCLStyle;
var
  LBitmap   : TBitmap;
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
begin
   ImageVCLStyle.Picture:=nil;
   if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
   begin
    LBitmap:=TBitmap.Create;
    try
       LBitmap.PixelFormat:=pf32bit;
       LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
       LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
       TStyleManager.StyleNames;
       SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
       LStyle:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
       try
         DrawSampleWindow(LStyle, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleName);
         ImageVCLStyle.Picture.Assign(LBitmap);
       finally
         LStyle.Free;
       end;
    finally
      LBitmap.Free;
    end;
   end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  s : string;
begin
 ReportMemoryLeaksOnShutdown:=True;
 for s in TStyleManager.StyleNames do
  if CompareText(s,'Windows')<>0 then
   ComboBoxVclStyles.Items.Add(s);

 ComboBoxVclStyles.ItemIndex:=0;
 DrawSeletedVCLStyle;
 EditPath.Text:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Output';
 ForceDirectories(EditPath.Text);
end;

function TFrmMain.GetStyleName: string;
begin
  Result:=ComboBoxVclStyles.Text;
end;

procedure RegisterVCLStyle(const StyleFileName: string);
begin
   if TStyleManager.IsValidStyle(StyleFileName) then
     TStyleManager.LoadFromFile(StyleFileName);
end;

procedure RegisterVCLStyles;
var
  Style   : string;
begin
  for Style in TDirectory.GetFiles(ExtractFilePath(ParamStr(0))+'\Styles', '*.vsf') do
    RegisterVCLStyle(Style);
end;


initialization
 RegisterVCLStyles;
end.
