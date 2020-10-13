unit uButtonsTabsStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrmButtonsTabsStyle = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    NCControls : TNCControls;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrmButtonsTabsStyle.FormCreate(Sender: TObject);
var
  LNCControl : TNCButton;
begin
  NCControls := TNCControls.Create(Self);
  NCControls.Images      := ImageList1;
  LNCControl:= NCControls.Controls.AddEx<TNCButton>;
  LNCControl.Style       := nsTab;
  LNCControl.ImageStyle  := isGrayHot;
  LNCControl.ImageIndex  := 0;
  LNCControl.BoundsRect  := Rect(30, 5, 140, 26);
  LNCControl.Caption     := 'Text Tab1';
  LNCControl.Name        := 'nsTab1';
  //LNCControl.OnClick     := ButtonNCClick;
  //LNCControl.OnDropDownClick := ButtonNCDropDownClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.Style       := nsTab;
  LNCControl.ImageStyle  := isGrayHot;
  LNCControl.ImageIndex  := 1;
  LNCControl.BoundsRect  := Rect(141, 5, 251, 26);
  LNCControl.Caption     := 'Text Tab2';
  LNCControl.Name        := 'nsTab2';
  //LNCControl.OnClick     := ButtonNCClick;

  LNCControl := NCControls.Controls.AddEx<TNCButton>;
  LNCControl.Style       := nsTab;
  LNCControl.ImageStyle  := isGrayHot;
  LNCControl.ImageIndex  := 3;
  LNCControl.BoundsRect  := Rect(252, 5, 362, 26);
  LNCControl.Caption     := 'Text Tab3';
  LNCControl.Name        := 'nsTab3';
  //LNCControl.OnClick     := ButtonNCClick;


  //Set the Index for the active tab button
  NCControls.ActiveTabButtonIndex := 1;
end;

end.
