unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Styles, Vcl.Themes, Vcl.Menus, Vcl.Buttons, Vcl.ImgList,
  Vcl.ExtDlgs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFrmTaskDlgMain = class(TForm)
    LblStyles: TLabel;
    ComboBox1: TComboBox;
    BtnCommandLinks: TSpeedButton;
    BtnFooter: TSpeedButton;
    BtnHiperLink: TSpeedButton;
    BtnCustomIcon: TSpeedButton;
    BtnQuestion: TSpeedButton;
    BtnExpamdButton: TSpeedButton;
    BtnCheckBox: TSpeedButton;
    BtnCustomButtons: TSpeedButton;
    CheckBoxEnableSysControls: TCheckBox;
    BrnRadioButtons: TButton;
    BtnHello: TSpeedButton;
    BtnProgress: TButton;
    BtnMarquee: TButton;
    CheckBoxHookDialogsIcons: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure CheckBoxEnableSysControlsClick(Sender: TObject);
    procedure BtnHelloClick(Sender: TObject);
    procedure BtnQuestionClick(Sender: TObject);
    procedure BtnCustomButtonsClick(Sender: TObject);
    procedure BtnCommandLinksClick(Sender: TObject);
    procedure BtnExpamdButtonClick(Sender: TObject);
    procedure BtnCustomIconClick(Sender: TObject);
    procedure BtnHiperLinkClick(Sender: TObject);
    procedure BtnFooterClick(Sender: TObject);
    procedure BtnCheckBoxClick(Sender: TObject);
    procedure BrnRadioButtonsClick(Sender: TObject);
    procedure BtnProgressClick(Sender: TObject);
    procedure BtnMarqueeClick(Sender: TObject);
    procedure CheckBoxHookDialogsIconsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmTaskDlgMain: TFrmTaskDlgMain;

implementation

uses
  Vcl.Styles.Utils.SysControls;

{$R *.dfm}
{$WARN SYMBOL_PLATFORM OFF}
procedure TFrmTaskDlgMain.BrnRadioButtonsClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'A Question';
    Text := 'This is a really tough one...';
    CommonButtons := [tcbOk, tcbCancel];
    MainIcon := tdiNone;
    with RadioButtons.Add do
      Caption := 'This is one option';
    with RadioButtons.Add do
      Caption := 'This is another option';
    with RadioButtons.Add do
      Caption := 'This is a third option';
    with RadioButtons.Add do
      Caption := 'And finally another option';
    if Execute then
      if ModalResult = mrOk then
        ShowMessage(Format('You choose %d.', [RadioButton.Index]));
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnProgressClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'ProgressBar Dialog';
    Text := 'This dialog contains a normal progressbar';
    CommonButtons := [tcbYes, tcbNo];
    Flags := [tfShowProgressBar, tfAllowDialogCancellation, tfExpandFooterArea];
    MainIcon := tdiInformation;
    ProgressBar.Min:=0;
    ProgressBar.Max:=100;
    ProgressBar.Position:=50;
    ExpandButtonCaption := 'Additional information';
    ExpandedText := 'This panel shows any text which you want. And is only visible when you press the expand button. Is not that cool?';
    Execute;
  finally
    Free;
  end

end;

procedure TFrmTaskDlgMain.BtnMarqueeClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'ProgressBar Marquee Dialog';
    Text := 'This dialog contains a  Marquee progressbar';
    CommonButtons := [tcbYes, tcbNo];
    Flags := [tfShowMarqueeProgressBar, tfAllowDialogCancellation, tfExpandFooterArea];
    MainIcon := tdiShield;
    ProgressBar.Min:=0;
    ProgressBar.Max:=100;
    ProgressBar.Position:=50;
    ExpandButtonCaption := 'Additional information';
    ExpandedText := 'This panel shows any text which you want. And is only visible when you press the expand button. Is not that cool?';
    Execute;
  finally
    Free;
  end

end;


procedure TFrmTaskDlgMain.CheckBoxEnableSysControlsClick(Sender: TObject);
begin
  TSysStyleManager.Enabled := TCheckBox(Sender).Checked;
end;

procedure TFrmTaskDlgMain.CheckBoxHookDialogsIconsClick(Sender: TObject);
begin
  TSysStyleManager.HookDialogIcons := TCheckBox(Sender).Checked;
end;

procedure TFrmTaskDlgMain.ComboBox1Select(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TFrmTaskDlgMain.FormCreate(Sender: TObject);
var
  s: string;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF }
  for s in TStyleManager.StyleNames do
    ComboBox1.Items.Add(s);

  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(TStyleManager.ActiveStyle.Name);

  CheckBoxEnableSysControls.Checked := TSysStyleManager.Enabled;
  CheckBoxHookDialogsIcons.Checked  := TSysStyleManager.HookDialogIcons;

end;

procedure TFrmTaskDlgMain.BtnHelloClick(Sender: TObject);
begin
with TTaskDialog.Create(Self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'Hello World!';
    Text := 'I am a TTaskDialog, that is, a wrapper for the Task Dialog introduced ' +
            'in the Microsoft Windows Vista operating system. Am I not adorable?';
    CommonButtons := [tcbClose];
    Execute;
  finally
    Free;
  end;
end;

procedure TFrmTaskDlgMain.BtnCommandLinksClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Title := 'Command Link buttons';
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Text := 'Check these Command Link buttons';
    CommonButtons := [];
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Remove';
      CommandLinkHint := 'Remove the item from the catalogue.';
      ModalResult := mrYes;
    end;
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Keep';
      CommandLinkHint := 'Keep the item in the catalogue.';
      ModalResult := mrNo;
    end;
    Flags := [tfUseCommandLinks, tfAllowDialogCancellation];
    MainIcon := tdiNone;
    if Execute then
      if ModalResult = mrYes then
        ;
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnFooterClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'A Question';
    Text := 'This is a really tough one...';
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    FooterText := 'This text appears on the footer of the task dialog.';
    FooterIcon := tdiInformation;
    Execute;
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnHiperLinkClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'Sample Dialog with hiperlink';
    CommonButtons := [tcbClose];
    Text := 'VCL Styles Utils ' +sLineBreak +
            'The text with hiperlinks are not supported yet....'+sLineBreak +
            '<a href="https://code.google.com/p/vcl-styles-utils/">https://code.google.com/p/vcl-styles-utils</a>';
    Flags := [tfUseHiconMain, tfAllowDialogCancellation, tfEnableHyperlinks];
    CustomMainIcon := Application.Icon;
    Execute;
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnCustomIconClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'Custom Icon';
    CommonButtons := [tcbClose];
    Text := 'This dialog display a custom icon';
    Flags := [tfUseHiconMain, tfAllowDialogCancellation];
    CustomMainIcon := Application.Icon;
    Execute;
  finally
    Free;
  end;
end;

procedure TFrmTaskDlgMain.BtnQuestionClick(Sender: TObject);
begin
with TTaskDialog.Create(Self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'Question';
    Text := 'Do you want to continue even though [...]?';
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiWarning; // There is no tdiQuestion
    if Execute then
      if ModalResult = mrYes then
        beep;
  finally
    Free;
  end;
end;

procedure TFrmTaskDlgMain.BtnExpamdButtonClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Text := 'This dialog contains Two commnad link buttons and a expand button';
    CommonButtons := [];
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Remove';
      CommandLinkHint := 'Remove the item from the catalogue.';
      ModalResult := mrYes;
    end;
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Keep';
      CommandLinkHint := 'Keep the item in the catalogue.';
      ModalResult := mrNo;
    end;
    Flags := [tfUseCommandLinks, tfAllowDialogCancellation, tfExpandFooterArea];
    ExpandButtonCaption := 'Additional information';
    ExpandedText := 'This panel shows any text which you want. And is only visible when you press the expand button. Is not that cool?';
    MainIcon := tdiNone;
    if Execute then
      if ModalResult = mrYes then
        ;
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnCheckBoxClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Title := 'A Question';
    Text := 'This is a really tough one...';
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    VerificationText := 'Remember my choice';
    Execute;
  finally
    Free;
  end
end;

procedure TFrmTaskDlgMain.BtnCustomButtonsClick(Sender: TObject);
begin
with TTaskDialog.Create(self) do
  try
    Title := 'Custom Buttons';
    Caption := 'VCL Styles Utils - TTaskDialogs Demo';
    Text := 'This Dialog contains two custom buttons';
    CommonButtons := [];
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Remove';
      ModalResult := mrYes;
    end;
    with TTaskDialogButtonItem(Buttons.Add) do
    begin
      Caption := 'Keep';
      ModalResult := mrNo;
    end;
    MainIcon := tdiError;
    if Execute then
      if ModalResult = mrYes then
        ;
  finally
    Free;
  end
end;

end.
