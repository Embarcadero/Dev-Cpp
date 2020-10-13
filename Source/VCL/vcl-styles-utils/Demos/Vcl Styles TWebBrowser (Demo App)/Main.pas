unit Main;

interface

{$DEFINE USE_VCLSTYLESWB}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SHDocVw, Vcl.StdCtrls, {$IFDEF USE_VCLSTYLESWB} Vcl.Styles.WebBrowser,{$ENDIF} Vcl.OleCtrls;

type
  {$IFDEF USE_VCLSTYLESWB}
  TWebBrowser=class(TVclStylesWebBrowser);
  {$ENDIF}
  TFrmMain = class(TForm)
    WebBrowser1: TWebBrowser;
    Button1: TButton;
    EditAddress: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ComboBoxStyles: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxStylesChange(Sender: TObject);
    procedure WebBrowser1DocumentComplete(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowser1NavigateComplete2(ASender: TObject;  const pDisp: IDispatch; const URL: OleVariant);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
 Vcl.Styles,
 Vcl.Themes;


{$R *.dfm}

procedure TFrmMain.Button1Click(Sender: TObject);
begin
 WebBrowser1.Navigate(EditAddress.Text);
 //WebBrowser1.Navigate('http://www.google.com/ncr');
end;

procedure TFrmMain.ComboBoxStylesChange(Sender: TObject);
Var
 LRect: TRect;
begin
 LRect:=WebBrowser1.BoundsRect;
 TStyleManager.SetStyle(ComboBoxStyles.Text);
 WebBrowser1.BoundsRect:=LRect;
 //WebBrowser1.Navigate(EditAddress.Text);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
 Style : string;
begin

 for Style in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(Style);

 ComboBoxStyles.ItemIndex:=ComboBoxStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);

 WebBrowser1.Navigate('about:blank');
end;

procedure TFrmMain.WebBrowser1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
//EditAddress.Text:=URL;
end;

procedure TFrmMain.WebBrowser1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
// ShowMessage('aaaaa');
end;
    {
initialization
 TStyleManager.Engine.RegisterStyleHook(TWebBrowser, TStyleHook);
     }

end.
