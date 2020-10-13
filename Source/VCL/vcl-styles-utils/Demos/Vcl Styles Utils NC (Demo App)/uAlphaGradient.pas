unit uAlphaGradient;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls;

type
  TFrmAlphaGradient = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    NCControls : TNCControls;
     procedure ButtonNCClick(Sender: TObject);
 public
    { Public declarations }
  end;

implementation

uses
 Vcl.GraphUtil;


{$R *.dfm}

procedure TFrmAlphaGradient.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('You clicked the %s button', [TNCButton(Sender).Name]));
end;

procedure TFrmAlphaGradient.FormCreate(Sender: TObject);
var
 LNCControl : TNCControl;
begin
  NCControls := TNCControls.Create(Self);
  NCControls.Images      := ImageList1;
  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style       := nsAlpha;
  LNCControl.GetAs<TNCButton>.ImageStyle  := isNormal;
  LNCControl.GetAs<TNCButton>.ImageIndex  := 0;
  LNCControl.BoundsRect  := Rect(30, 5, 120, 26);
  LNCControl.Caption     := 'nsAlpha1';
  LNCControl.Name        := 'nsAlpha1';
  LNCControl.GetAs<TNCButton>.AlphaColor   := clWebLavender;
  LNCControl.GetAs<TNCButton>.AlphaHotColor:= clWebAliceBlue;
  LNCControl.GetAs<TNCButton>.FontColor   := clWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor:= clYellow;
  LNCControl.GetAs<TNCButton>.OnClick     := ButtonNCClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style       := nsAlpha;
  LNCControl.GetAs<TNCButton>.ImageStyle  := isGrayHot;
  LNCControl.GetAs<TNCButton>.ImageIndex  := 1;
  LNCControl.BoundsRect  := Rect(125, 5, 215, 26);
  LNCControl.Caption     := 'nsAlpha2';
  LNCControl.Name        := 'nsAlpha2';
  LNCControl.GetAs<TNCButton>.AlphaColor   := clWebOrange;
  LNCControl.GetAs<TNCButton>.AlphaHotColor:= clWebOrangeRed;
  LNCControl.GetAs<TNCButton>.FontColor   := clWebWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor:= clWebWhite;
  LNCControl.GetAs<TNCButton>.OnClick     := ButtonNCClick;


  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style       := nsAlpha;
  LNCControl.GetAs<TNCButton>.ImageStyle  := isGrayHot;
  LNCControl.GetAs<TNCButton>.ImageIndex  := 2;
  LNCControl.BoundsRect  := Rect(220, 5, 310, 26);
  LNCControl.Caption     := 'nsAlpha3';
  LNCControl.Name        := 'nsAlpha3';
  LNCControl.GetAs<TNCButton>.AlphaColor   := clWebGreenYellow;
  LNCControl.GetAs<TNCButton>.AlphaHotColor:= clWebDarkOrange;
  LNCControl.GetAs<TNCButton>.FontColor   := clWebWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor:= clWebWhite;
  LNCControl.GetAs<TNCButton>.OnClick     := ButtonNCClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style        := nsGradient;
  LNCControl.GetAs<TNCButton>.StartColor   := clWebSkyBlue;
  LNCControl.GetAs<TNCButton>.EndColor     := clWebChocolate;
  LNCControl.GetAs<TNCButton>.Direction    := TGradientDirection.gdHorizontal;
  LNCControl.GetAs<TNCButton>.FontColor    := clWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor := clYellow;
  LNCControl.GetAs<TNCButton>.ImageStyle  := isGrayHot;
  LNCControl.GetAs<TNCButton>.ImageIndex  := 3;
  LNCControl.BoundsRect  := Rect(315, 5, 415, 26);
  LNCControl.Caption     := 'nsGradient1';
  LNCControl.Name        := 'nsGradient1';
  LNCControl.GetAs<TNCButton>.OnClick     := ButtonNCClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style        := nsGradient;
  LNCControl.GetAs<TNCButton>.StartColor   := clWebSeashell;
  LNCControl.GetAs<TNCButton>.EndColor     := clWebGray;
  LNCControl.GetAs<TNCButton>.Direction    := TGradientDirection.gdVertical;
  LNCControl.GetAs<TNCButton>.FontColor   := clWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor:= clYellow;
  LNCControl.GetAs<TNCButton>.ImageStyle  := isGrayHot;
  LNCControl.GetAs<TNCButton>.ImageIndex  := 7;
  LNCControl.BoundsRect  := Rect(420, 5, 520, 26);
  LNCControl.Caption     := 'nsGradient2';
  LNCControl.Name        := 'nsGradient2';
  LNCControl.GetAs<TNCButton>.OnClick     := ButtonNCClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.GetAs<TNCButton>.Style        := nsGradient;
  LNCControl.GetAs<TNCButton>.StartColor   := clWebDarkOrange;
  LNCControl.GetAs<TNCButton>.EndColor     := clRed;
  LNCControl.GetAs<TNCButton>.Direction    := TGradientDirection.gdHorizontal;
  LNCControl.GetAs<TNCButton>.FontColor    := clWhite;
  LNCControl.GetAs<TNCButton>.HotFontColor := clYellow;
  LNCControl.GetAs<TNCButton>.ImageStyle   := isGrayHot;
  LNCControl.GetAs<TNCButton>.ImageIndex   := 8;
  LNCControl.GetAs<TNCButton>.BoundsRect   := Rect(525, 5, 625, 26);
  LNCControl.Caption      := 'nsGradient3';
  LNCControl.Name         := 'nsGradient3';
  LNCControl.GetAs<TNCButton>.OnClick      := ButtonNCClick;
end;

end.
