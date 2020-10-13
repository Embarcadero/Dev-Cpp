//**************************************************************************************************
//
// Unit uVclStylesEQU
// unit uVclStylesEQU  for the VCL Styles Utils  project
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uVclStylesEQU.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uVclStylesEQU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Mask, ComCtrls, Generics.Defaults, Vcl.Styles.Utils.Graphics,
  Generics.Collections, Vcl.ImgList, Vcl.ActnList, Vcl.Styles.Ext,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan;

type
  TFrmHueSat = class(TForm)
    btnApply: TButton;
    btnSave: TButton;
    ImageList1: TImageList;
    Label4: TLabel;
    ComboBoxVclStyles: TComboBox;
    Button1: TButton;
    ActionManager1: TActionManager;
    ActionApplyStyle: TAction;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheetHSL: TTabSheet;
    TrackBarHue: TTrackBar;
    EditLight: TEdit;
    UpDownLight: TUpDown;
    EditSat: TEdit;
    UpDownSat: TUpDown;
    EditHue: TEdit;
    UpDownHue: TUpDown;
    ButtonHue: TButton;
    TrackBarSaturation: TTrackBar;
    ButtonSaturation: TButton;
    TrackBarLightness: TTrackBar;
    ButtonLightness: TButton;
    Label1: TLabel;
    Bevel4: TBevel;
    Label2: TLabel;
    Bevel2: TBevel;
    Label3: TLabel;
    Bevel3: TBevel;
    TabSheetRGB: TTabSheet;
    TrackBarRed: TTrackBar;
    Label5: TLabel;
    Bevel5: TBevel;
    EditRed: TEdit;
    UpDownRed: TUpDown;
    Button2: TButton;
    TrackBarGreen: TTrackBar;
    Label6: TLabel;
    EditGreen: TEdit;
    UpDownGreen: TUpDown;
    Button3: TButton;
    Bevel6: TBevel;
    TrackBarBlue: TTrackBar;
    Label7: TLabel;
    EditBlue: TEdit;
    UpDownBlue: TUpDown;
    Button4: TButton;
    Bevel7: TBevel;
    TabSheetBlend: TTabSheet;
    ButtonApplyBlend: TButton;
    RadioButtonHSL: TRadioButton;
    RadioButtonRGB: TRadioButton;
    RadioButtonBlend: TRadioButton;
    ColorBoxblend: TColorBox;
    Button6: TButton;
    CbBlend: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    ColorDialog1: TColorDialog;
    LabelDrop: TLabel;
    CheckBoxSepia: TCheckBox;
    LinkLabel1: TLinkLabel;
    PageControl2: TPageControl;
    TabSheetPreview: TTabSheet;
    TabSheetStyleColors: TTabSheet;
    TabSheetStyleFonts: TTabSheet;
    TabSheetSystemColors: TTabSheet;
    ListViewStyleColors: TListView;
    ImageListStyleColors: TImageList;
    ListViewStyleFontColors: TListView;
    ImageListStyleFontColors: TImageList;
    ListViewSystemColors: TListView;
    ImageListSystemColors: TImageList;
    CheckBoxStyleColors: TCheckBox;
    CheckBoxStyleFontColors: TCheckBox;
    CheckBoxSystemColors: TCheckBox;
    StatusBar1: TStatusBar;
    btnSaveSettings: TButton;
    Button7: TButton;
    btnLoadSettings: TButton;
    OpenDialog1: TOpenDialog;
    TabSheetTextures: TTabSheet;
    RadioButtonTextures: TRadioButton;
    CbBlendTextures: TComboBox;
    Label10: TLabel;
    CbTextures: TComboBox;
    Label11: TLabel;
    ImageTexture: TImage;
    ImageVCLStyle: TImage;
    procedure ButtonHueClick(Sender: TObject);
    procedure ButtonSaturationClick(Sender: TObject);
    procedure ButtonLightnessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarHueChange(Sender: TObject);
    procedure TrackBarLightnessChange(Sender: TObject);
    procedure TrackBarSaturationChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure EditHueExit(Sender: TObject);
    procedure UpDownHueChanging(Sender: TObject; var AllowChange: Boolean);
    procedure EditSatExit(Sender: TObject);
    procedure UpDownSatChanging(Sender: TObject; var AllowChange: Boolean);
    procedure EditLightExit(Sender: TObject);
    procedure UpDownLightChanging(Sender: TObject; var AllowChange: Boolean);
    procedure ComboBoxVclStylesChange(Sender: TObject);
    procedure ActionApplyStyleUpdate(Sender: TObject);
    procedure ActionApplyStyleExecute(Sender: TObject);
    procedure CheckBoxSepiaClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure TrackBarRedChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonApplyBlendClick(Sender: TObject);
    procedure RadioButtonHSLClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CbBlendChange(Sender: TObject);
    procedure ColorBoxblendGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure CheckBoxStyleColorsClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnLoadSettingsClick(Sender: TObject);
    procedure CbBlendTexturesChange(Sender: TObject);
  private
    OriginalBitMap : TBitmap;
    FStyleName     : string;
    //FPreview       : TVclStylesPreview;
    procedure DrawSeletedVCLStyle;
    function GetStyleName: string;
    property StyleName: string Read GetStyleName Write FStyleName;
    procedure LoadStyle(SetDefaultValues:Boolean=True);
    procedure LoadStyleColors;
    procedure LoadStyleFontsColors;
    procedure LoadStyleSystemColors;

    procedure SetPageActive(Index:integer);
    function GetFilters  : TObjectList<TBitmapFilter>;
    procedure DropFiles(var msg : TMessage); message WM_DROPFILES;
    procedure FillListStyles;
    procedure BuildPreview;

    procedure SaveSettings;
    procedure LoadSettings;

    procedure LoadImageTexture;
  end;

var
  FrmHueSat : TFrmHueSat;

implementation

{$R *.dfm}

uses
  PngFunctions,
  System.Diagnostics,
  System.TypInfo,
  System.Rtti,
  WinAPi.ShellAPI,
  System.IOUtils,
  System.StrUtils,
  System.UITypes,
  System.Types,
  Vcl.GraphUtil,
  Vcl.Styles.Utils,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.Jpeg,
  uVCLStylesInfo;

procedure CreateArrayBitmap(Width,Height:Word;Colors: Array of TColor;var bmp : TBitmap);
Var
 i : integer;
 w : integer;
begin
  bmp.PixelFormat:=pf32bit;
  bmp.Width:=Width;
  bmp.Height:=Height;
  bmp.Canvas.Brush.Color := clBlack;
  bmp.Canvas.FillRect(Rect(0,0, Width, Height));

  w :=(Width-2) div (High(Colors)+1);
  for i:=0 to High(Colors) do
  begin
   bmp.Canvas.Brush.Color := Colors[i];
   //bmp.Canvas.FillRect(Rect((w*i),0, w*(i+1), Height));
   bmp.Canvas.FillRect(Rect((w*i)+1,1, w*(i+1)+1, Height-1))
  end;
end;

procedure TFrmHueSat.DropFiles(var msg: TMessage);
const
  cb = 255;
var
  FileIndex,
  nCount     : integer;
  FileName   : array [0..cb] of Char;
  StyleInfo  : TStyleInfo;
  LStyleName : string;
begin
  LStyleName :='';
  nCount := DragQueryFile( msg.WParam, $FFFFFFFF, FileName, cb);
  for FileIndex := 0 to nCount-1 do
  begin
    DragQueryFile( msg.WParam, FileIndex, FileName, cb );
    if TStyleManager.IsValidStyle(FileName, StyleInfo) then
    if not MatchText(StyleInfo.Name, TStyleManager.StyleNames) then
    begin
      TStyleManager.LoadFromFile(FileName);
      LStyleName:=StyleInfo.Name;
    end;
  end;


  if LStyleName<>'' then
  begin
   FillListStyles;
   ComboBoxVclStyles.ItemIndex:=ComboBoxVclStyles.Items.IndexOf(LStyleName);
   LoadStyle;
  end;

  DragFinish( msg.WParam );

  //TStyleManager.Style[''].GetStyleColor();
end;

procedure TFrmHueSat.ActionApplyStyleExecute(Sender: TObject);
begin
 TStyleManager.SetStyle(StyleName);
 DragAcceptFiles( Handle, True );
end;

procedure TFrmHueSat.ActionApplyStyleUpdate(Sender: TObject);
begin
 TCustomAction(Sender).Enabled:=(StyleName<>'') and not SameText(StyleName, TStyleManager.ActiveStyle.Name, loUserLocale);
end;


function TFrmHueSat.GetFilters: TObjectList<TBitmapFilter>;
var
  LFilter : TValue;
  ctx     : TRttiContext;
  RttiInstanceType : TRttiInstanceType;
begin
  Result:=TObjectList<TBitmapFilter>.Create;

    if RadioButtonHSL.Checked then
    begin
      If CheckBoxSepia.Checked then
        Result.Add(TBitmap32SepiaFilter.Create(20));

      If UpDownHue.Position<>0 then
      begin
        if UpDownHue.Position >= 0 then
           Result.Add(TBitmap32HueFilter.Create(UpDownHue.Position))
        else
            Result.Add(TBitmap32HueFilter.Create(360-Abs(UpDownHue.Position)));
      end;

      If UpDownSat.Position<>0 then
        //Result.Add(TBitmap32SaturationFilter.Create(UpDownSat.Position));
        Result.Add(TBitmap32SaturationFilter.Create((255 - ((UpDownSat.Position) * 255) div MaxSat)));

      If UpDownLight.Position<>0 then
        Result.Add(TBitmap32LightnessFilter.Create(UpDownLight.Position));
    end;

    if RadioButtonRGB.Checked then
    begin
      If UpDownRed.Position<>0 then
        Result.Add(TBitmap32RedFilter.Create(UpDownRed.Position));

      If UpDownGreen.Position<>0 then
        Result.Add(TBitmap32GreenFilter.Create(UpDownGreen.Position));

      If UpDownBlue.Position<>0 then
        Result.Add(TBitmap32BlueFilter.Create(UpDownBlue.Position));
    end;

    if RadioButtonBlend.Checked then
    begin
      ctx := TRttiContext.Create;
      try
        RttiInstanceType := (ctx.GetType(CbBlend.Items.Objects[CbBlend.ItemIndex]) as TRttiInstanceType);
        LFilter := RttiInstanceType.GetMethod('Create').Invoke(RttiInstanceType.MetaclassType,[ColorBoxblend.Selected]);
        Result.Add(TBitmapFilter(LFilter.AsObject));
      finally
        ctx.Free;
      end;
    end;

    if RadioButtonTextures.Checked then
    begin
      ctx := TRttiContext.Create;
      try
        RttiInstanceType := (ctx.GetType(CbBlendTextures.Items.Objects[CbBlendTextures.ItemIndex]) as TRttiInstanceType);
        //ShowMessage(IntToStr(ImageTexture.Picture.Bitmap.Width));
        LFilter := RttiInstanceType.GetMethod('CreateBitMap').Invoke(RttiInstanceType.MetaclassType,[ImageTexture.Picture.Bitmap]);

        Result.Add(TBitmapFilter(LFilter.AsObject));
      finally
        ctx.Free;
      end;
    end;

end;


procedure TFrmHueSat.btnApplyClick(Sender: TObject);
Var
  LFilters : TObjectList<TBitmapFilter>;
  VclUtils : TVclStylesUtils;
  sw: TStopWatch;
begin
  if StyleName='' then exit;
  try
    LFilters:=GetFilters;
    sw := TStopWatch.StartNew;
    VclUtils:=TVclStylesUtils.Create(StyleName);
    try
     if CheckBoxSystemColors.Checked then
       VclUtils.Elements:=VclUtils.Elements + [vseSysColors];

     if CheckBoxStyleColors.Checked then
       VclUtils.Elements:=VclUtils.Elements + [vseStyleColors];

     if CheckBoxStyleFontColors.Checked then
       VclUtils.Elements:=VclUtils.Elements + [vseStyleFontColors];

      VclUtils.SetFilters(LFilters);
      VclUtils.ApplyChanges;
    finally
      LFilters.Free;
      VclUtils.Free;
    end;

    StatusBar1.SimpleText:=(Format('ellapsed %d ms', [sw.ElapsedMilliseconds]));
    TStyleManager.ReloadStyle(StyleName);
    LoadStyle;
  except
    on E: Exception do
      MessageDlg(Format('Error saving vcl style - Message : %s : Trace %s', [E.Message, E.StackTrace]),  mtWarning, [mbOK], 0);
  end;
end;

procedure TFrmHueSat.btnLoadSettingsClick(Sender: TObject);
begin
 LoadSettings;
end;

procedure TFrmHueSat.btnSaveClick(Sender: TObject);
Var
  LFilters : TObjectList<TBitmapFilter>;
  VclUtils : TVclStylesUtils;
  Frm      : TFrmVCLStyleInfoDialog;
  NewName  : string;
  LBitmap  : TBitmap;
  LPng     : TPngImage;
  ColorName: string;
  sw       : TStopWatch;
begin
   if StyleName='' then exit;
   VclUtils:=TVclStylesUtils.Create(StyleName, True);
   try
     SaveDialog1.Filter:='Visual Style Files|*.vsf';
     SaveDialog1.DefaultExt:='*.vsf';

     NewName := VclUtils.StyleExt.StyleInfo.Name;
     if RadioButtonHSL.Checked then
     begin
      if CheckBoxSepia.Checked then
      NewName :=NewName +' Sepia';
      if (TrackBarHue.Position<>0) or (TrackBarSaturation.Position<>0)  or (TrackBarLightness.Position<>0) then
        NewName :=Format('%s H%d.S%d.L%d',[NewName,TrackBarHue.Position,TrackBarSaturation.Position, TrackBarLightness.Position]);
     end
     else
     if RadioButtonRGB.Checked then
      NewName :=Format('%s R%d.G%d.B%d',[NewName,TrackBarRed.Position,TrackBarGreen.Position, TrackBarBlue.Position])
     else
     if RadioButtonBlend.Checked then
     begin
      ColorName:=ColorBoxblend.Items[ColorBoxblend.ItemIndex];
      if StartsText('Custom',ColorName) then
       ColorName:=ColorToString(ColorBoxblend.Selected)
      else
      if StartsText('clWeb',ColorName) then
       ColorName:=StringReplace(ColorName,'clWeb','',[rfReplaceAll]);

      NewName :=Format('%s Blend %s %s',[NewName,CbBlend.Text,ColorName]);
     end;

     SaveDialog1.FileName:=NewName+'.vsf';

     if SaveDialog1.Execute then
     begin
        try
          LFilters:=GetFilters;
          try
            Frm:=TFrmVCLStyleInfoDialog.Create(Self);
            try
             Frm.StyleInfo:=VclUtils.StyleExt.StyleInfo;
             Frm.EditName.Text:=NewName;
             if Frm.Execute then
              VclUtils.StyleExt.StyleInfo:=Frm.StyleInfo;
            finally
             Frm.Free;
            end;

            if CheckBoxSystemColors.Checked then
              VclUtils.Elements:=VclUtils.Elements + [vseSysColors];

            if CheckBoxStyleColors.Checked then
              VclUtils.Elements:=VclUtils.Elements + [vseStyleColors];

            if CheckBoxStyleFontColors.Checked then
              VclUtils.Elements:=VclUtils.Elements + [vseStyleFontColors];

            sw := TStopWatch.StartNew;
            VclUtils.SetFilters(LFilters);
            VclUtils.SaveToFile(SaveDialog1.FileName);

            LBitmap:=TBitmap.Create;
            try
               LBitmap.PixelFormat:=pf32bit;
               LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
               LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
               DrawSampleWindow(VclUtils.StyleExt, LBitmap.Canvas, ImageVCLStyle.ClientRect, NewName, Icon.Handle);
               //DrawSampleWindow(VclUtils.StyleExt, ImageVCLStyle, NewName, Icon.Handle);
               //LBitmap.SaveToFile(ChangeFileExt(SaveDialog1.FileName,'.bmp'));
               ConvertToPNG(LBitmap, LPng);
               try
                 LPng.SaveToFile(ChangeFileExt(SaveDialog1.FileName,'.png'));
               finally
                 LPng.Free;
               end;
            finally
              LBitmap.Free;
            end;

            //MessageDlg('Vcl Style Saved', mtInformation, [mbOK], 0);
            StatusBar1.SimpleText:=(Format('ellapsed  %d ms', [sw.ElapsedMilliseconds]));
          finally
            LFilters.Free;
          end;

        except
          on E: Exception do
           MessageDlg(Format('Error saving vcl style - Message : %s : Trace %s', [E.Message, E.StackTrace]),  mtWarning, [mbOK], 0);
        end;
     end;
   finally
      VclUtils.Free;
   end;

end;


procedure TFrmHueSat.btnSaveSettingsClick(Sender: TObject);
begin
 SaveSettings;
end;

procedure TFrmHueSat.Button2Click(Sender: TObject);
begin
  UpDownRed.Position   := 0;
  TrackBarRed.Position := 0;
end;

procedure TFrmHueSat.Button3Click(Sender: TObject);
begin
  UpDownGreen.Position   := 0;
  TrackBarGreen.Position := 0;
end;

procedure TFrmHueSat.Button4Click(Sender: TObject);
begin
  UpDownBlue.Position   := 0;
  TrackBarBlue.Position := 0;
end;

procedure TFrmHueSat.ButtonApplyBlendClick(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.Button6Click(Sender: TObject);
begin
 if ColorDialog1.Execute then
  ColorBoxblend.Selected:=ColorDialog1.Color;
end;

procedure TFrmHueSat.ButtonHueClick(Sender: TObject);
begin
  UpDownHue.Position   := DefHue;
  TrackBarHue.Position := DefHue;
end;

procedure TFrmHueSat.ButtonLightnessClick(Sender: TObject);
begin
  UpDownLight.Position       := DefLig;
  TrackBarLightness.Position := DefLig;
end;

procedure TFrmHueSat.ButtonSaturationClick(Sender: TObject);
begin
  UpDownSat.Position := DefSat;
  TrackBarSaturation.Position := DefSat;
end;

procedure TFrmHueSat.CheckBoxSepiaClick(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.CheckBoxStyleColorsClick(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.ColorBoxblendGetColors(Sender: TCustomColorBox;
  Items: TStrings);
Var
 Item : TIdentMapEntry;
begin
  for Item in WebNamedColors do
   Items.AddObject(Item.Name,TObject(Item.Value));
end;

procedure TFrmHueSat.CbBlendChange(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.CbBlendTexturesChange(Sender: TObject);
begin
  LoadImageTexture;
  BuildPreview;
end;

procedure TFrmHueSat.ComboBoxVclStylesChange(Sender: TObject);
begin
  LoadStyle;
  BuildPreview;
end;

procedure TFrmHueSat.DrawSeletedVCLStyle;
var
  LBitmap   : TBitmap;
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
begin
   ImageVCLStyle.Picture:=nil;
   if (StyleName<>'') and (not SameText('Windows',StyleName)) then
   begin
    LabelDrop.Caption:='';
    LBitmap:=TBitmap.Create;
    try
       LBitmap.PixelFormat:=pf32bit;
       LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
       LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
       TStyleManager.StyleNames;//call DiscoverStyleResources
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

{
var
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
begin
   ImageVCLStyle.Picture:=nil;
   if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
   begin
     LabelDrop.Caption:='';
     TStyleManager.StyleNames;//call DiscoverStyleResources
     SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
     LStyle:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
     try
       FPreview.Style:=LStyle;
       FPreview.Caption:=StyleName;
       FPreview.Repaint;
     finally
       LStyle.Free;
     end;
   end;
end;
}

procedure TFrmHueSat.EditHueExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditHue.Text, Value) then
  begin
    if Value< UpDownHue.Min then
     EditHue.Text:=IntToStr(UpDownHue.Min)
    else
    if Value> UpDownHue.Max then
     EditHue.Text:=IntToStr(UpDownHue.Max);

     UpDownHueChanging(nil, Allow);
  end;
end;

procedure TFrmHueSat.EditLightExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditLight.Text, Value) then
  begin
    if Value< UpDownLight.Min then
     EditLight.Text:=IntToStr(UpDownLight.Min)
    else
    if Value> UpDownLight.Max then
     EditLight.Text:=IntToStr(UpDownLight.Max);

    UpDownLightChanging(nil, Allow);
  end;
end;


procedure TFrmHueSat.EditSatExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditSat.Text, Value) then
  begin
    if Value< UpDownSat.Min then
     EditSat.Text:=IntToStr(UpDownSat.Min)
    else
    if Value> UpDownSat.Max then
     EditSat.Text:=IntToStr(UpDownSat.Max);

    UpDownSatChanging(nil, Allow);
  end;
end;


procedure TFrmHueSat.FillListStyles;
var
  s : string;
begin
 ComboBoxVclStyles.Items.Clear;
 for s in TStyleManager.StyleNames do
  if not SameText(s,'Windows') then
   ComboBoxVclStyles.Items.Add(s);
end;

procedure TFrmHueSat.FormCreate(Sender: TObject);
var
  s : string;
begin
 {
 FPreview:=TVclStylesPreview.Create(Self);
 FPreview.Parent:=TabSheetPreview;
 FPreview.BoundsRect := TabSheetPreview.ClientRect;
 }
 DragAcceptFiles( Handle, True );
 //ReportMemoryLeaksOnShutdown:=True;

  FillListStyles;
  if ComboBoxVclStyles.Items.Count>0 then
  ComboBoxVclStyles.ItemIndex:=0;

  OriginalBitMap := TBitmap.Create;
  LoadStyle;

  With CbBlend.Items do
  begin
   AddObject('Multiply', TypeInfo(TBitmap32BlendMultiply));
   AddObject('Burn', TypeInfo(TBitmap32BlendBurn));
   AddObject('Additive', TypeInfo(TBitmap32BlendAdditive));
   AddObject('Dodge', TypeInfo(TBitmap32BlendDodge));
   AddObject('Overlay', TypeInfo(TBitmap32BlendOverlay));
   AddObject('Difference', TypeInfo(TBitmap32BlendDifference));
   AddObject('Lighten', TypeInfo(TBitmap32BlendLighten));
   AddObject('Darken', TypeInfo(TBitmap32BlendDarken));
   AddObject('Screen', TypeInfo(TBitmap32BlendScreen));
  end;
  CbBlend.ItemIndex:=0;

  With CbBlendTextures.Items do
  begin
   AddObject('Multiply', TypeInfo(TBitmap32BlendMultiply));
   AddObject('Burn', TypeInfo(TBitmap32BlendBurn));
   AddObject('Additive', TypeInfo(TBitmap32BlendAdditive));
   AddObject('Dodge', TypeInfo(TBitmap32BlendDodge));
   AddObject('Overlay', TypeInfo(TBitmap32BlendOverlay));
   AddObject('Difference', TypeInfo(TBitmap32BlendDifference));
   AddObject('Lighten', TypeInfo(TBitmap32BlendLighten));
   AddObject('Darken', TypeInfo(TBitmap32BlendDarken));
   AddObject('Screen', TypeInfo(TBitmap32BlendScreen));
  end;
  CbBlendTextures.ItemIndex:=0;

  for s in TDirectory.GetFiles(ExtractFilePath(ParamStr(0))+'Textures','*.jpg') do
   CbTextures.Items.Add(ExtractFileName(s));

  if CbTextures.Items.Count>0 then
  begin
   CbTextures.ItemIndex:=0;
   LoadImageTexture;
  end;

  SetPageActive(0);
end;

procedure TFrmHueSat.FormDestroy(Sender: TObject);
begin
  OriginalBitMap.Free;
end;

function TFrmHueSat.GetStyleName: string;
begin
  Result := ComboBoxVclStyles.Text;
end;

procedure TFrmHueSat.BuildPreview;
var
  LFilters : TObjectList<TBitmapFilter>;
  //VclUtils : TVclStylesUtils;
  LBitmap  : TBitmap;
  Filter   : TBitmapFilter;
  Index    : Integer;
  LColor   : TColor;
begin
  if StyleName='' then exit;
  LFilters:=GetFilters;
  try
    //VclUtils:=TVclStylesUtils.Create(StyleName, True);
    LBitmap:=TBitmap.Create;
    try
      LBitmap.Assign(OriginalBitMap);
      for Filter in LFilters do
       Filter.ProcessBitmap(LBitmap);

      ImageVCLStyle.Picture.Assign(LBitmap);
      {
      VclUtils.SetFilters(LFilters);
      VclUtils.ApplyChanges;
        LBitmap:=TBitmap.Create;
        try
           ImageVCLStyle.Picture:=nil;
           LBitmap.PixelFormat:=pf32bit;
           LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
           LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
           DrawSampleWindow(VclUtils.StyleExt, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleName, Icon.Handle);
           ImageVCLStyle.Picture.Assign(LBitmap);
        finally
          LBitmap.Free;
        end;
       }

    finally
      LBitmap.Free;
      //VclUtils.Free;
    end;


      ImageListStyleColors.Clear;
      for Index:=0 to ListViewStyleColors.Items.Count-1 do
      begin
        LBitmap:=TBitmap.Create;
        try
          LColor:=TColor(ListViewStyleColors.Items.Item[Index].Data);
          CreateArrayBitmap(16,16,[LColor], LBitmap);
          if CheckBoxStyleColors.Checked then
            for Filter in LFilters do
              Filter.ProcessBitmap(LBitmap);
          ImageListStyleColors.Add(LBitmap, nil);
          ListViewStyleColors.Items.Item[Index].ImageIndex:=ImageListStyleColors.Count-1;
        finally
          LBitmap.Free;
        end;
      end;

      ImageListStyleFontColors.Clear;
      for Index:=0 to ListViewStyleFontColors.Items.Count-1 do
      begin
        LBitmap:=TBitmap.Create;
        try
          LColor:=TColor(ListViewStyleFontColors.Items.Item[Index].Data);
          CreateArrayBitmap(16,16,[LColor], LBitmap);
          if CheckBoxStyleFontColors.Checked then
            for Filter in LFilters do
              Filter.ProcessBitmap(LBitmap);
          ImageListStyleFontColors.Add(LBitmap, nil);
          ListViewStyleFontColors.Items.Item[Index].ImageIndex:=ImageListStyleFontColors.Count-1;
        finally
          LBitmap.Free;
        end;
      end;

      ImageListSystemColors.Clear;
      for Index:=0 to ListViewSystemColors.Items.Count-1 do
      begin
        LBitmap:=TBitmap.Create;
        try
          LColor:=TColor(ListViewSystemColors.Items.Item[Index].Data);
          CreateArrayBitmap(16,16,[LColor], LBitmap);
          if CheckBoxSystemColors.Checked then
            for Filter in LFilters do
              Filter.ProcessBitmap(LBitmap);
          ImageListSystemColors.Add(LBitmap, nil);
          ListViewSystemColors.Items.Item[Index].ImageIndex:=ImageListSystemColors.Count-1;
        finally
          LBitmap.Free;
        end;
      end;

  finally
   LFilters.Free;
  end;
end;


procedure TFrmHueSat.SaveSettings;
Var
 LVCLStylesFilter : TVCLStylesFilter;
 LFilters         : TObjectList<TBitmapFilter>;
 LElements        : TVCLStylesElements;
begin
  if RadioButtonHSL.Checked then
    LVCLStylesFilter:=vsfHSL
  else
  if RadioButtonRGB.Checked then
    LVCLStylesFilter:=vsfRGB
  else
  if RadioButtonBlend.Checked then
    LVCLStylesFilter:=vsfBlend;

  LElements:=[];
   if CheckBoxSystemColors.Checked then
     LElements:=LElements + [vseSysColors];

   if CheckBoxStyleColors.Checked then
     LElements:=LElements + [vseStyleColors];

   if CheckBoxStyleFontColors.Checked then
     LElements:=LElements + [vseStyleFontColors];

  LFilters:=GetFilters;
  try
    SaveDialog1.Filter:='Visual Style EQ Settings|*.vseq';
    SaveDialog1.DefaultExt:='*.vseq';
    if SaveDialog1.Execute then
       TVclStylesUtils.SaveSettings(SaveDialog1.FileName, LElements, LVCLStylesFilter, LFilters);
  finally
    LFilters.Free;
  end;
end;

procedure TFrmHueSat.SetPageActive(Index: integer);
var
 i : Integer;
begin
  PageControl1.ActivePageIndex:=Index;
  for i := 0 to PageControl1.PageCount-1 do
   PageControl1.Pages[i].TabVisible:=i=Index;
end;


procedure TFrmHueSat.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil , nil, SW_SHOWNORMAL);
end;


procedure TFrmHueSat.LoadImageTexture;
var
 JpegImage : TJPEGImage;
begin
  ImageTexture.Picture.LoadFromFile(ExtractFilePath(ParamStr(0))+'textures\'+CbTextures.Text);
  if (ImageTexture.Picture.Graphic is TJPegImage) then
  begin
    JpegImage := TJpegImage.Create;
    try
     JpegImage.Assign(ImageTexture.Picture.Graphic);
     ImageTexture.Picture.Bitmap.Assign(JpegImage);
    finally
     JpegImage.Free;
    end;
  end;

  //ShowMessage(IntToStr(ImageTexture.Picture.Graphic.Width));
end;

procedure TFrmHueSat.LoadSettings;
Var
 LFilterType  : TVCLStylesFilter;
 LFilters     : TObjectList<TBitmapFilter>;
 sw           : TStopWatch;
 VclUtils     : TVclStylesUtils;
 LElements    : TVCLStylesElements;
 Filter       : TBitmapFilter;
 i            : Integer;
begin
  if StyleName='' then exit;
  OpenDialog1.Filter:='Visual Style EQ Settings|*.vseq';
  OpenDialog1.DefaultExt:='*.vseq';
  if OpenDialog1.Execute then
  begin
   LFilters:=TObjectList<TBitmapFilter>.Create;
   try
     TVclStylesUtils.LoadSettings(OpenDialog1.FileName, LElements, LFilterType, LFilters);

     case LFilterType of
       vsfHSL   : begin
                    RadioButtonHSL.Checked:=True;

                    for Filter in  LFilters do
                     if Filter is TBitmap32HueFilter then
                        TrackBarHue.Position:=Filter.ColorValue
                     else
                     if Filter is TBitmap32SaturationFilter then
                       TrackBarSaturation.Position:=Filter.ColorValue
                     else
                     if Filter is TBitmap32LightnessFilter then
                       TrackBarLightness.Position:=Filter.ColorValue;
                  end;

       vsfRGB   : begin
                    RadioButtonRGB.Checked:=True;
                    for Filter in  LFilters do
                     if Filter is TBitmap32RedFilter then
                        TrackBarRed.Position:=Filter.ColorValue
                     else
                     if Filter is TBitmap32GreenFilter then
                        TrackBarGreen.Position:=Filter.ColorValue
                     else
                     if Filter is TBitmap32BlueFilter then
                        TrackBarBlue.Position:=Filter.ColorValue;
                  end;

       vsfBlend : begin
                    RadioButtonBlend.Checked:=True;
                    Filter:=LFilters[0];
                    ColorBoxblend.Selected:=Filter.ColorValue;
                    for i:=0 to CbBlend.Items.Count-1 do
                     if Pointer(CbBlend.Items.Objects[i])=Filter.ClassInfo then
                     begin
                      CbBlend.ItemIndex:=i;
                      Break;
                     end;
                  end;
     end;

      try
        sw := TStopWatch.StartNew;
        VclUtils:=TVclStylesUtils.Create(StyleName);
        try
          VclUtils.Elements:=LElements;
          VclUtils.SetFilters(LFilters);
          VclUtils.ApplyChanges;
        finally
          VclUtils.Free;
        end;

        StatusBar1.SimpleText:=(Format('ellapsed %d ms', [sw.ElapsedMilliseconds]));
        TStyleManager.ReloadStyle(StyleName);
        LoadStyle(False);
      except
        on E: Exception do
          MessageDlg(Format('Error applying settings - Message : %s : Trace %s', [E.Message, E.StackTrace]),  mtWarning, [mbOK], 0);
      end;

   finally
     LFilters.Free;
   end;
  end;
end;

procedure TFrmHueSat.LoadStyle(SetDefaultValues:Boolean);
begin
  CheckBoxSepia.Checked:=False;
  DrawSeletedVCLStyle;
  LoadStyleColors;
  LoadStyleFontsColors;
  LoadStyleSystemColors;


  OriginalBitMap.Assign(ImageVCLStyle.Picture.Bitmap);

  if SetDefaultValues then
  begin
    UpDownHue.Position   := DefHue;
    TrackBarHue.Position := DefHue;

    UpDownSat.Position := DefSat;
    TrackBarSaturation.Position := DefSat;

    UpDownLight.Position       := DefLig;
    TrackBarLightness.Position := DefLig;

    UpDownRed.Position       := 0;
    TrackBarRed.Position     := 0;

    UpDownGreen.Position       := 0;
    TrackBarGreen.Position     := 0;

    UpDownBlue.Position       := 0;
    TrackBarBlue.Position     := 0;
  end;
end;


procedure TFrmHueSat.LoadStyleColors;
var
 StyleColor : TStyleColor;
 Item       : TListItem;
 LColor     : TColor;
 LBitmap    : TBitmap;
begin
  if StyleName='' then exit;
  ListViewStyleColors.Items.BeginUpdate;
  try
    ListViewStyleColors.Items.Clear;
    ImageListStyleColors.Clear;
    for StyleColor  := Low(TStyleColor) to High(TStyleColor) do
    begin
      LColor:=TStyleManager.Style[StyleName].GetStyleColor(StyleColor);
      Item:=ListViewStyleColors.Items.Add;
      Item.Data:=Pointer(LColor);
      Item.Caption:=GetEnumName(TypeInfo(TStyleColor),Integer(StyleColor));
      Item.SubItems.Add(ColorToString(LColor));
      Item.SubItems.Add(IntToHex(GetRValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetGValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetBValue(LColor),2));
      LBitmap:=TBitmap.Create;
      try
        CreateArrayBitmap(16,16,[LColor], LBitmap);
        ImageListStyleColors.Add(LBitmap, nil);
        Item.ImageIndex:=ImageListStyleColors.Count-1;
      finally
        LBitmap.Free;
      end;
    end;
  finally
    ListViewStyleColors.Items.EndUpdate;
  end;
end;

procedure TFrmHueSat.LoadStyleFontsColors;
var
 StyleFont : TStyleFont;
 Item       : TListItem;
 LColor     : TColor;
 LBitmap    : TBitmap;
begin
  if StyleName='' then exit;
  ListViewStyleFontColors.Items.BeginUpdate;
  try
    ListViewStyleFontColors.Items.Clear;
    ImageListStyleFontColors.Clear;
    for StyleFont  := Low(TStyleFont) to High(TStyleFont) do
    begin
      LColor:=TStyleManager.Style[StyleName].GetStyleFontColor(StyleFont);
      Item:=ListViewStyleFontColors.Items.Add;
      Item.Data:=Pointer(LColor);
      Item.Caption:=GetEnumName(TypeInfo(TStyleFont),Integer(StyleFont));
      Item.SubItems.Add(ColorToString(LColor));
      Item.SubItems.Add(IntToHex(GetRValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetGValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetBValue(LColor),2));
      LBitmap:=TBitmap.Create;
      try
        CreateArrayBitmap(16,16,[LColor], LBitmap);
        ImageListStyleFontColors.Add(LBitmap, nil);
        Item.ImageIndex:=ImageListStyleFontColors.Count-1;
      finally
        LBitmap.Free;
      end;
    end;
  finally
    ListViewStyleFontColors.Items.EndUpdate;
  end;
end;


procedure TFrmHueSat.LoadStyleSystemColors;
Var
  Element    : TIdentMapEntry;
  Item       : TListItem;
  LColor     : TColor;
  LBitmap    : TBitmap;
begin
  if StyleName='' then exit;

  ListViewSystemColors.Items.BeginUpdate;
  try
    ListViewSystemColors.Items.Clear;
    ImageListSystemColors.Clear;
    for Element in VclStyles_SysColors do
    begin
      LColor:=TStyleManager.Style[StyleName].GetSystemColor(Element.Value);
      Item:=ListViewSystemColors.Items.Add;
      Item.Data:=Pointer(LColor);
      Item.Caption:=Element.Name;
      Item.SubItems.Add(ColorToString(LColor));
      Item.SubItems.Add(IntToHex(GetRValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetGValue(LColor),2));
      Item.SubItems.Add(IntToHex(GetBValue(LColor),2));
      LBitmap:=TBitmap.Create;
      try
        CreateArrayBitmap(16,16,[LColor], LBitmap);
        ImageListSystemColors.Add(LBitmap, nil);
        Item.ImageIndex:=ImageListSystemColors.Count-1;
      finally
        LBitmap.Free;
      end;
    end;
  finally
    ListViewSystemColors.Items.EndUpdate;
  end;

  {
  ListBoxStyleSystemColors.Items.Clear;
  ListBoxStyleSystemColors.Items.BeginUpdate;
  for Element in SysColors do
  ListBoxStyleSystemColors.Items.AddObject(Element.Name,
  TObject(TStyleManager.Style[StyleName].GetSystemColor(Element.Value))
  );
  ListBoxStyleSystemColors.Items.EndUpdate;
  }
end;

procedure TFrmHueSat.RadioButtonHSLClick(Sender: TObject);
begin
  SetPageActive(TRadioButton(Sender).Tag);
  //LoadStyle;
  DrawSeletedVCLStyle;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarRedChange(Sender: TObject);
begin
  UpDownRed.Position   := TrackBarRed.Position;
  UpDownGreen.Position := TrackBarGreen.Position;
  UpDownBlue.Position  := TrackBarBlue.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarHueChange(Sender: TObject);
begin
  UpDownHue.Position := TrackBarHue.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarLightnessChange(Sender: TObject);
begin
  UpDownLight.Position := TrackBarLightness.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarSaturationChange(Sender: TObject);
begin
  UpDownSat.Position := TrackBarSaturation.Position;
  BuildPreview;
end;

procedure TFrmHueSat.UpDownHueChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarHue.Position := UpDownHue.Position;
  AllowChange:=True;
end;

procedure TFrmHueSat.UpDownLightChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarLightness.Position := UpDownLight.Position;
  AllowChange:=True;
end;

procedure TFrmHueSat.UpDownSatChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarSaturation.Position := UpDownSat.Position;
  AllowChange:=True;
end;

procedure RegisterVCLStyle(const StyleFileName: string);
begin
   if TStyleManager.IsValidStyle(StyleFileName) then
     TStyleManager.LoadFromFile(StyleFileName);
end;

procedure RegisterVCLStyles;
var
  Style     : string;
  StylesDir : string;
begin
  StylesDir:=ExpandFileName(ExtractFilePath(ParamStr(0))  + '\..\Styles');
  if DirectoryExists(StylesDir) then
    for Style in TDirectory.GetFiles(IncludeTrailingPathDelimiter(StylesDir), '*.vsf') do
      RegisterVCLStyle(Style);

  TStyleManager.SystemStyle;  //prevent bug in registration of vcl style
end;


initialization
 RegisterVCLStyles;

end.
