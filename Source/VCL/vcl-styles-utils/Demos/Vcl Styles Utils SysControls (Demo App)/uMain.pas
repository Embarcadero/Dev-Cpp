unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Styles, Vcl.Themes, Vcl.Menus, Vcl.Buttons, Vcl.ImgList,
  Vcl.ExtDlgs, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    LblStyles: TLabel;
    ComboBoxStyles: TComboBox;
    Edit1: TEdit;
    BtnMsgBox: TSpeedButton;
    BtnRaiseException: TSpeedButton;
    BtnFontDialog: TSpeedButton;
    BtnColorDialog: TSpeedButton;
    BtnReplaceDialog: TSpeedButton;
    BtnFindDialog: TSpeedButton;
    BtnPageSetup: TSpeedButton;
    BtnPrinterSetup: TSpeedButton;
    BtnOpenDialog: TSpeedButton;
    StaticText1: TStaticText;
    CheckBoxSysControls: TCheckBox;
    OpenDialog1: TOpenDialog;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    ReplaceDialog1: TReplaceDialog;
    FindDialog1: TFindDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PageSetupDialog1: TPageSetupDialog;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    I1: TMenuItem;
    C2: TMenuItem;
    S1: TMenuItem;
    D1: TMenuItem;
    B1: TMenuItem;
    BreakItem11: TMenuItem;
    R1: TMenuItem;
    RadioItem11: TMenuItem;
    D2: TMenuItem;
    I2: TMenuItem;
    C3: TMenuItem;
    StaticText2: TStaticText;
    PopupMenu2: TPopupMenu;
    R2: TMenuItem;
    RightToLeftItem11: TMenuItem;
    RightToLeftItem12: TMenuItem;
    RightToLeftItem13: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Contents2: TMenuItem;
    Contents3: TMenuItem;
    Contents4: TMenuItem;
    BtnSelectFolder: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenTextFileDialog1: TOpenTextFileDialog;
    PrintDialog1: TPrintDialog;
    BtnPrintDialog: TSpeedButton;
    Edit2: TMenuItem;
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
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    C1: TMenuItem;
    CheckBoxModernDialogs: TCheckBox;
    CheckBoxHookDialogIcons: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxStylesSelect(Sender: TObject);
    procedure BtnOpenDialogClick(Sender: TObject);
    procedure BtnMsgBoxClick(Sender: TObject);
    procedure BtnColorDialogClick(Sender: TObject);
    procedure BtnReplaceDialogClick(Sender: TObject);
    procedure BtnFontDialogClick(Sender: TObject);
    procedure BtnFindDialogClick(Sender: TObject);
    procedure BtnPageSetupClick(Sender: TObject);
    procedure BtnPrinterSetupClick(Sender: TObject);
    procedure CheckBoxSysControlsClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure C3Click(Sender: TObject);
    procedure BtnRaiseExceptionClick(Sender: TObject);
    procedure BtnSelectFolderClick(Sender: TObject);
    procedure BtnPrintDialogClick(Sender: TObject);
    procedure CheckBoxModernDialogsClick(Sender: TObject);
    procedure CheckBoxHookDialogIconsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


uses
  Vcl.Styles.Utils.SysControls, Vcl.FileCtrl;
{$R *.dfm}

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage('EnJoY !!');
end;

procedure TForm1.BtnSelectFolderClick(Sender: TObject);
var
  FDir : string;
begin
   SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir, [sdNewUI, sdNewFolder]);
end;

procedure TForm1.C3Click(Sender: TObject);
begin
  ShowMessage('Hi');
end;

procedure TForm1.CheckBoxHookDialogIconsClick(Sender: TObject);
begin
  TSysStyleManager.HookDialogIcons := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBoxModernDialogsClick(Sender: TObject);
begin
 UseLatestCommonDialogs := CheckBoxModernDialogs.Checked;
end;

procedure TForm1.CheckBoxSysControlsClick(Sender: TObject);
begin
  TSysStyleManager.Enabled := TCheckBox(Sender).Checked;
end;

procedure TForm1.ComboBoxStylesSelect(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBoxStyles.Items[ComboBoxStyles.ItemIndex]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: string;
begin
{$IFDEF DEBUG}
 //ReportMemoryLeaksOnShutdown := True;
{$ENDIF }
  for s in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(s);

  ComboBoxStyles.ItemIndex:=ComboBoxStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);

 CheckBoxModernDialogs.Checked:= UseLatestCommonDialogs;
 CheckBoxSysControls.Checked:=  TSysStyleManager.Enabled;
 CheckBoxHookDialogIcons.Checked:=  TSysStyleManager.HookDialogIcons;
end;

procedure TForm1.BtnPrintDialogClick(Sender: TObject);
begin
  PrintDialog1.Execute(Handle);
end;

procedure TForm1.BtnMsgBoxClick(Sender: TObject);
begin
  MessageBox(Handle, 'This is a simple Yes/No MessageBox .',
    'MessageBox Caption ', MB_YESNO or MB_ICONQUESTION);
end;

procedure TForm1.BtnRaiseExceptionClick(Sender: TObject);
begin
{$IFNDEF DEBUG}
  raise Exception.Create('Error Message');
{$ENDIF}
end;

procedure TForm1.BtnFontDialogClick(Sender: TObject);
begin
  FontDialog1.Execute();
end;

procedure TForm1.BtnColorDialogClick(Sender: TObject);
begin
  ColorDialog1.Execute();
end;

procedure TForm1.BtnReplaceDialogClick(Sender: TObject);
begin
  ReplaceDialog1.Execute();
end;

procedure TForm1.BtnFindDialogClick(Sender: TObject);
begin
  FindDialog1.Execute();
end;

procedure TForm1.BtnPageSetupClick(Sender: TObject);
begin
  PageSetupDialog1.Execute();
end;

procedure TForm1.BtnPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute();
end;

procedure TForm1.BtnOpenDialogClick(Sender: TObject);
begin
 if OpenDialog1.Execute then
  ShowMessage(Format('%s', [OpenDialog1.FileName]));
end;

end.
