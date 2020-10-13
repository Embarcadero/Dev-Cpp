unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.Imaging.jpeg, Vcl.ComCtrls;

type
  TFrmMain = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    GroupBox1: TGroupBox;
    EditNCImage: TEdit;
    RadioButtonNCImage: TRadioButton;
    RadioButtonNCColor: TRadioButton;
    ColorBoxNC: TColorBox;
    BtnSetNCImage: TButton;
    GroupBox2: TGroupBox;
    EditBackImage: TEdit;
    RadioButtonBackImage: TRadioButton;
    RadioButtonBackColor: TRadioButton;
    ColorBoxBackground: TColorBox;
    BtnSetBackImage: TButton;
    CheckBoxNC: TCheckBox;
    CheckBoxBack: TCheckBox;
    ComboBoxStyles: TComboBox;
    Label1: TLabel;
    CheckBoxMerge: TCheckBox;
    BtnSetSharedImg: TButton;
    EditSharedImage: TEdit;
    Label2: TLabel;
    procedure ColorBoxNCGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure ColorBoxNCChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonNCColorClick(Sender: TObject);
    procedure BtnSetNCImageClick(Sender: TObject);
    procedure CheckBoxNCClick(Sender: TObject);
    procedure BtnSetBackImageClick(Sender: TObject);
    procedure CheckBoxBackClick(Sender: TObject);
    procedure RadioButtonBackColorClick(Sender: TObject);
    procedure ColorBoxBackgroundChange(Sender: TObject);
    procedure ComboBoxStylesChange(Sender: TObject);
    procedure CheckBoxMergeClick(Sender: TObject);
    procedure BtnSetSharedImgClick(Sender: TObject);
  private
    procedure SetNCColor;
    procedure SetNCImage;

    procedure SetBackColor;
    procedure SetBackImage;

    procedure SetSharedImage;

    procedure RefreshControls;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Vcl.Styles.FormStyleHooks,
  Vcl.GraphUtil,
  Vcl.Styles,
  Vcl.Themes;

{$R *.dfm}

procedure TFrmMain.BtnSetNCImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
      EditNCImage.Text := OpenPictureDialog1.FileName;
      SetNCImage;
  end;
end;

procedure TFrmMain.BtnSetSharedImgClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
      EditSharedImage.Text := OpenPictureDialog1.FileName;
      SetSharedImage;
  end;

end;

procedure TFrmMain.BtnSetBackImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    EditBackImage.Text := OpenPictureDialog1.FileName;
    SetBackImage;
  end;
end;

procedure TFrmMain.CheckBoxBackClick(Sender: TObject);
begin
  TFormStyleHookBackground.BackGroundSettings.Enabled := CheckBoxBack.Checked;
  RefreshControls;
end;

procedure TFrmMain.CheckBoxMergeClick(Sender: TObject);
begin
 TFormStyleHookBackground.MergeImages:=CheckBoxMerge.Checked;
 BtnSetNCImage.Enabled  :=not CheckBoxMerge.Checked and not RadioButtonNCColor.Checked;
 BtnSetBackImage.Enabled:=not CheckBoxMerge.Checked and not RadioButtonBackColor.Checked;
 BtnSetSharedImg.Enabled:=CheckBoxMerge.Checked;
 SendMessage(Handle, WM_NCPAINT, 0, 0);
 RefreshControls;
end;

procedure TFrmMain.CheckBoxNCClick(Sender: TObject);
begin
  TFormStyleHookBackground.NCSettings.Enabled := CheckBoxNC.Checked;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.ColorBoxBackgroundChange(Sender: TObject);
begin
  SetBackColor;
end;

procedure TFrmMain.ColorBoxNCChange(Sender: TObject);
begin
  SetNCColor;
end;

procedure TFrmMain.ColorBoxNCGetColors(Sender: TCustomColorBox;
  Items: TStrings);
Var
  Item: TIdentMapEntry;
begin
  Items.Clear;
  for Item in WebNamedColors do
    Items.AddObject(Item.Name, TObject(Item.Value));
end;

procedure TFrmMain.ComboBoxStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBoxStyles.Text);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  style: string;
begin
  ReportMemoryLeaksOnShutdown := True;
  ColorBoxNC.Selected := TFormStyleHookBackground.NCSettings.Color;
  ColorBoxBackground.Selected := TFormStyleHookBackground.BackGroundSettings.Color;

  for style in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(style);

  ComboBoxStyles.ItemIndex := ComboBoxStyles.Items.IndexOf
    (TStyleManager.ActiveStyle.Name);
end;

procedure TFrmMain.RadioButtonBackColorClick(Sender: TObject);
begin
  BtnSetBackImage.Enabled := not RadioButtonBackColor.Checked and not CheckBoxMerge.Checked;
  EditBackImage.Enabled := not RadioButtonBackColor.Checked and not CheckBoxMerge.Checked;
  ColorBoxBackground.Enabled := RadioButtonBackColor.Checked;

  TFormStyleHookBackground.BackGroundSettings.UseColor :=
    RadioButtonBackColor.Checked;
  RefreshControls;
end;

procedure TFrmMain.RadioButtonNCColorClick(Sender: TObject);
begin
  BtnSetNCImage.Enabled := not RadioButtonNCColor.Checked and not CheckBoxMerge.Checked;
  EditNCImage.Enabled := not RadioButtonNCColor.Checked and not CheckBoxMerge.Checked;
  ColorBoxNC.Enabled := RadioButtonNCColor.Checked;
  TFormStyleHookBackground.NCSettings.UseColor := RadioButtonNCColor.Checked;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.RefreshControls;
Var
  LIndex: Integer;
begin
  for LIndex := 0 to ComponentCount - 1 do
    if Components[LIndex] is TWinControl then
    begin
      TWinControl(Components[LIndex]).Invalidate;
      TWinControl(Components[LIndex]).Perform(WM_PAINT, 0, 0);
    end;

  Self.Invalidate;
  Self.Perform(WM_PAINT, 0, 0);
end;

procedure TFrmMain.SetBackColor;
begin
  TFormStyleHookBackground.BackGroundSettings.UseColor := True;
  TFormStyleHookBackground.BackGroundSettings.Color := ColorBoxBackground.Selected;
  RefreshControls;
end;

procedure TFrmMain.SetBackImage;
begin
  TFormStyleHookBackground.BackGroundSettings.UseImage := True;
  TFormStyleHookBackground.BackGroundSettings.ImageLocation := EditBackImage.Text;
  RefreshControls;
end;

procedure TFrmMain.SetNCColor;
begin
  TFormStyleHookBackground.NCSettings.UseColor := True;
  TFormStyleHookBackground.NCSettings.Color := ColorBoxNC.Selected;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.SetNCImage;
begin
  TFormStyleHookBackground.NCSettings.UseImage := True;
  TFormStyleHookBackground.NCSettings.ImageLocation := EditNCImage.Text;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.SetSharedImage;
begin
  if not TFormStyleHookBackground.MergeImages then
   TFormStyleHookBackground.MergeImages := True;
  TFormStyleHookBackground.SharedImageLocation := EditSharedImage.Text;
  RefreshControls;
end;

initialization

  TStyleManager.Engine.RegisterStyleHook(TFrmMain, TFormStyleHookBackground);
  TFormStyleHookBackground.NCSettings.Color := clWebDarkSlategray;
  TFormStyleHookBackground.BackGroundSettings.Color := clWebDarkOliveGreen;
  TFormStyleHookBackground.MergeImages:=True;
                             {
  TStyleManager.Engine.RegisterStyleHook(TCustomTabControl, TTabControlStyleHookBackround);
  TStyleManager.Engine.RegisterStyleHook(TTabControl, TTabControlStyleHookBackround);
  TStyleManager.Engine.RegisterStyleHook(TPageControl, TTabControlStyleHookBackround);

  TStyleManager.Engine.RegisterStyleHook(TTabSheet, TTabControlStyleHookBackround);
                             }

end.
