unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.Styles.NC, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
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
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Help2: TMenuItem;
    Contents2: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn2: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp2: TMenuItem;
    About2: TMenuItem;
    BtnDropDownMenu: TButton;
    ImageList1: TImageList;
    BtnStyles: TButton;
    BtnCustomStyle: TButton;
    CheckBoxNCVisible: TCheckBox;
    BtnAlpha: TButton;
    Label1: TLabel;
    BtnStyleTabs: TButton;
    CheckBoxSystemMenu: TCheckBox;
    CheckBoxShowCaption: TCheckBox;
    CheckBoxAwesome: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnDropDownMenuClick(Sender: TObject);
    procedure CheckBoxNCVisibleClick(Sender: TObject);
    procedure BtnStylesClick(Sender: TObject);
    procedure BtnCustomStyleClick(Sender: TObject);
    procedure BtnAlphaClick(Sender: TObject);
    procedure BtnStyleTabsClick(Sender: TObject);
    procedure CheckBoxSystemMenuClick(Sender: TObject);
    procedure CheckBoxShowCaptionClick(Sender: TObject);
    procedure CheckBoxAwesomeClick(Sender: TObject);
  private
    { Private declarations }
    NCControls: TNCControls;
    procedure UpdateButtons;
  public
    { Public declarations }
    procedure ButtonNCClick(Sender: TObject);
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.Utils.SystemMenu,
  Vcl.Styles.FontAwesome,
  Vcl.Styles.Utils.Graphics,
  uButtonsStyles,
  uCustomStyles,
  uDropdown,
  uAlphaGradient,
  uButtonsTabsStyles;

{$R *.dfm}

procedure TFrmMain.BtnDropDownMenuClick(Sender: TObject);
var
  LForm: TFrmDropDown;
begin
  LForm := TFrmDropDown.Create(Self);
  LForm.Show();
end;

procedure TFrmMain.BtnStylesClick(Sender: TObject);
var
  LForm: TFrmButtonsStyles;
begin
  LForm := TFrmButtonsStyles.Create(Self);
  LForm.Show();
end;

procedure TFrmMain.BtnStyleTabsClick(Sender: TObject);
var
  LForm: TFrmButtonsTabsStyle;
begin
  LForm := TFrmButtonsTabsStyle.Create(Self);
  LForm.Show();
end;

procedure TFrmMain.BtnAlphaClick(Sender: TObject);
var
  LForm: TFrmAlphaGradient;
begin
  LForm := TFrmAlphaGradient.Create(Self);
  LForm.Show();
end;

procedure TFrmMain.BtnCustomStyleClick(Sender: TObject);
var
  LForm: TFrmCustomStyles;
begin
  LForm := TFrmCustomStyles.Create(Self);
  LForm.Show();
end;

procedure TFrmMain.ButtonNCClick(Sender: TObject);
begin
  if Sender is TNCButton then
    ShowMessage(Format('You clicked the button %s', [TNCButton(Sender).Name]));
end;

procedure TFrmMain.CheckBoxAwesomeClick(Sender: TObject);
begin
  UpdateButtons;
  NCControls.Invalidate;
end;

procedure TFrmMain.CheckBoxNCVisibleClick(Sender: TObject);
begin
  NCControls.Visible := CheckBoxNCVisible.Checked;
end;

procedure TFrmMain.CheckBoxShowCaptionClick(Sender: TObject);
begin
  NCControls.ShowCaption := CheckBoxShowCaption.Checked;

end;

procedure TFrmMain.CheckBoxSystemMenuClick(Sender: TObject);
begin
  NCControls.ShowSystemMenu := CheckBoxSystemMenu.Checked;
  UpdateButtons;
  NCControls.Invalidate;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  // ReportMemoryLeaksOnShutdown:=True;
  TVclStylesSystemMenu.Create(Self);
  NCControls := TNCControls.Create(Self);
  NCControls.Images := ImageList1;
  UpdateButtons;
end;

procedure TFrmMain.UpdateButtons;
const
  cWidth = 20;
  AwesomeIcons: Array [0 .. 11] of Word = (fa_home, fa_signal, fa_search, fa_envelope_o, fa_remove, fa_gear, fa_twitter,
    fa_user, fa_arrow_circle_o_down, fa_arrow_circle_o_up, fa_check, fa_power_off);
var
  iLeft, i, LImageIndex: integer;
  LNCControl: TNCButton;
begin
  iLeft := 5;
  if NCControls.ShowSystemMenu then
    iLeft := 30;

  NCControls.Controls.Clear;
  NCControls.Controls.BeginUpdate;
  try
    for i := 0 to 10 do
    begin
      LNCControl := NCControls.Controls.AddEx<TNCButton>;
      LNCControl.Name := Format('NCButton%d', [i + 1]);
      LNCControl.Hint := Format('Hint for NCButton%d', [i + 1]);
      LNCControl.ShowHint := True;
      LNCControl.Caption := '';
      LNCControl.Style := nsTranparent;
      LNCControl.ImageStyle := isGrayHot;
      LNCControl.ImageAlignment := TImageAlignment.iaCenter;

      LNCControl.UseFontAwesome := CheckBoxAwesome.Checked;

      if LNCControl.UseFontAwesome then
      begin
        LNCControl.AwesomeFontColor := StyleServices.GetSystemColor(clBtnText);
        LNCControl.AwesomeHotFontColor := StyleServices.GetSystemColor(clHighlight);
        LImageIndex := AwesomeIcons[i]
      end
      else
        LImageIndex := i;

      LNCControl.ImageIndex := LImageIndex;

      LNCControl.ImageAlignment := TImageAlignment.iaCenter;
      LNCControl.BoundsRect := Rect(iLeft, 5, iLeft + cWidth, 25);
      inc(iLeft, cWidth + 2);
      LNCControl.OnClick := ButtonNCClick;
    end;
  finally
    NCControls.Controls.EndUpdate;
  end;

end;

end.
