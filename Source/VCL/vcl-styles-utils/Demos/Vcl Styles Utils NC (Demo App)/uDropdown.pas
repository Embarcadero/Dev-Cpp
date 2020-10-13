unit uDropdown;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Styles.NC, Vcl.Dialogs, Vcl.ImgList, Vcl.Menus,
  Vcl.StdCtrls;

type
  TFrmDropDown = class(TForm)
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Repeat1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    Links1: TMenuItem;
    Object1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    NCControls: TNCControls;
  public
    { Public declarations }
    procedure ButtonNCClick(Sender: TObject);
  end;

implementation

uses
 Vcl.Themes;

{$R *.dfm}

procedure TFrmDropDown.ButtonNCClick(Sender: TObject);
begin
  ShowMessage('Hello');
end;

procedure TFrmDropDown.FormCreate(Sender: TObject);
var
  LNCButton: TNCButton;
begin
  NCControls := TNCControls.Create(Self);
  NCControls.Images := ImageList1;
  NCControls.ShowSystemMenu := False;

  LNCButton := NCControls.Controls.AddEx<TNCButton>;
  LNCButton.Style := nsSplitButton;
  LNCButton.ImageStyle := isGrayHot;
  LNCButton.ImageIndex := 3;
  LNCButton.BoundsRect := Rect(5, 5, 85, 25);
  LNCButton.Caption := 'Menu';
  LNCButton.DropDownMenu := PopupMenu1;
  LNCButton.OnClick := ButtonNCClick;

  LNCButton.FontColor := StyleServices.GetSystemColor(clBtnText);
  LNCButton.HotFontColor := StyleServices.GetSystemColor(clHighlightText);
end;

end.
