unit uButtonsStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls;

type
  TFrmButtonsStyles = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    cbNCBtnStyles: TComboBox;
    CheckBoxAwesome: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbNCBtnStylesChange(Sender: TObject);
    procedure CheckBoxAwesomeClick(Sender: TObject);
  private
    NCControls : TNCControls;
    procedure ButtonNCClick(Sender: TObject);
    procedure ButtonNCDropDownClick(Sender: TObject);
    procedure UpdateNCButtons;
  public
  end;


implementation

uses
 Vcl.Styles,
 Vcl.Themes,
 System.TypInfo,
 Vcl.Styles.FontAwesome,
 Vcl.Styles.Utils.Graphics,
 Vcl.GraphUtil;

{$R *.dfm}

procedure TFrmButtonsStyles.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('you clicked the %s button', [TNCButton(Sender).Caption]));
end;

procedure TFrmButtonsStyles.ButtonNCDropDownClick(Sender: TObject);
begin
  ShowMessage(Format('you clicked the DropDown of the %s button', [TNCButton(Sender).Caption]));
end;

procedure TFrmButtonsStyles.cbNCBtnStylesChange(Sender: TObject);
begin
 UpdateNCButtons;
 NCControls.Invalidate;
end;

procedure TFrmButtonsStyles.CheckBoxAwesomeClick(Sender: TObject);
begin
 UpdateNCButtons;
 NCControls.Invalidate;
end;

procedure TFrmButtonsStyles.FormCreate(Sender: TObject);
var
  LStyle : TNCButton.TNCButtonStyle;
begin
  for LStyle:= Low(TNCButton.TNCButtonStyle) to High(TNCButton.TNCButtonStyle) do
 // if not (LStyle in [nsAlpha, nsGradient]) then
    cbNCBtnStyles.Items.AddObject(GetEnumName(TypeInfo(TNCButton.TNCButtonStyle), Integer(LStyle)), TObject(LStyle));
  cbNCBtnStyles.ItemIndex:=0;

  NCControls:=TNCControls.Create(Self);
  NCControls.Images      := ImageList1;
  UpdateNCButtons;
end;

procedure TFrmButtonsStyles.UpdateNCButtons;
const
  cWidth = 95;
  ColorsPalette: Array[0..11] of TColor = (
    clWebRosyBrown, clWebYellowGreen, clGray, clWebOrange, clWebMoccasin, clWebMediumPurple,
    clWebMediumBlue, clWebMediumOrchid, clWebIvory, clWebSeashell, clWebPapayaWhip, clWebPeru);

  AwesomeIcons : Array[0..11] of Word = (
    fa_home, fa_signal, fa_search, fa_envelope_o, fa_remove, fa_gear,
    fa_home, fa_signal, fa_gear, fa_download, fa_print, fa_camera);

var
  iLeft, iSep, i, LImageIndex : Integer;
  LNCControl : TNCButton;
begin
  NCControls.Controls.Clear;
  NCControls.Controls.BeginUpdate;
  try
    iLeft:=30;
    for i := 0 to 5 do
    begin
      LNCControl := NCControls.Controls.AddEx<TNCButton>;
      LNCControl.Style       := TNCButton.TNCButtonStyle(Integer(cbNCBtnStyles.Items.Objects[cbNCBtnStyles.ItemIndex]));
      LNCControl.ImageStyle  := isNormal;

      LNCControl.UseFontAwesome:=CheckBoxAwesome.Checked;

      if LNCControl.UseFontAwesome then
        LImageIndex:= AwesomeIcons[i]
      else
        LImageIndex:=i;

      LNCControl.ImageIndex  := LImageIndex;


      LNCControl.BoundsRect  := Rect(iLeft, 5, iLeft + cWidth, 26);

      iSep:=5;
      if LNCControl.Style=nsTab then
        iSep:=0;

      inc(iLeft, cWidth + iSep);

      LNCControl.Caption     := 'NCButton ' + IntToStr(i);
      LNCControl.Name        := 'NcButton' + IntToStr(i);

      LNCControl.FontColor     := StyleServices.GetSystemColor(clBtnText);
      LNCControl.HotFontColor  := StyleServices.GetSystemColor(clHighlight);

      LNCControl.OnClick     := ButtonNCClick;
      LNCControl.OnDropDownClick := ButtonNCDropDownClick;

      if LNCControl.Style in [nsPushButton, nsSplitButton, nsSplitTrans] then
      begin
        LNCControl.FontColor := StyleServices.GetSystemColor(clBtnText);
        LNCControl.HotFontColor := StyleServices.GetSystemColor(clHighlightText);
      end
      else
      if LNCControl.Style = nsAlpha then
      begin
        LNCControl.AlphaColor    := ColorsPalette[i];
        LNCControl.AlphaHotColor := ColorsPalette[i + 2];
        LNCControl.FontColor     := clWhite;
        LNCControl.HotFontColor  := clBlack;
      end
      else
      if LNCControl.Style = nsGradient then
      begin
        LNCControl.StartColor    := ColorsPalette[i];
        LNCControl.EndColor      := ColorsPalette[i + 1];
        LNCControl.FontColor     := clWhite;
        LNCControl.HotFontColor  := clBlack;
      end
    end;
  finally
    NCControls.Controls.EndUpdate;
  end;
end;

end.
