unit uCustomStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls,
  Vcl.Grids;

type
  TFrmCustomStyles = class(TForm)
    ImageList1: TImageList;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    NCControls : TNCControls;
  public
    { Public declarations }
  end;

implementation

uses
 Vcl.Themes;

{$R *.dfm}

procedure TFrmCustomStyles.ComboBox1Change(Sender: TObject);
begin
  NCControls.StyleServices := TStyleManager.Style[ComboBox1.Text];
end;

procedure TFrmCustomStyles.FormCreate(Sender: TObject);
var
 s : string;
begin
  for s in TStyleManager.StyleNames do
   if not SameText(s, 'Windows') then
    ComboBox1.Items.Add(s);

  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf('Auric');
  NCControls:=TNCControls.Create(Self);
  NCControls.StyleServices := TStyleManager.Style[ComboBox1.Text];
end;

end.
