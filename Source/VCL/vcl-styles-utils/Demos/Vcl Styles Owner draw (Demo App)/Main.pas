unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList, Vcl.ComCtrls;

type
  TFrmMain = class(TForm)
    ListBox1: TListBox;
    ImageList1: TImageList;
    CheckBox1: TCheckBox;
    ListView1: TListView;
    ComboBox1: TComboBox;
    TreeView1: TTreeView;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    Procedure SetOwnerDraw(Value : Boolean);

  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
 Vcl.Styles.OwnerDrawFix,
 Winapi.CommCtrl,
 Vcl.Styles,
 Vcl.Themes;

{$R *.dfm}

procedure TFrmMain.CheckBox1Click(Sender: TObject);
begin
  //SetOwnerDraw(CheckBox1.Checked);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
 i,j  :  integer;
 Item : TListItem;
 Root,Node : TTreeNode;
begin


  SetOwnerDraw(CheckBox1.Checked);
  for i:=0 to 99 do
  begin
   ListBox1.Items.Add('Item '+IntToStr(i));
   ComboBox1.Items.Add('Item '+IntToStr(i));
  end;

  for i:=0 to 99 do
  begin
    Item:= ListView1.Items.Add;
    Item.Caption:='Item '+IntToStr(i);
    Item.SubItems.Add(Item.Caption+'.1');
    Item.SubItems.Add(Item.Caption+'.2');
    Item.SubItems.Add(Item.Caption+'.3');
    Item.Checked:=Odd(i);
  end;

  for j:=0 to 99 do
  begin
    Root:=TreeView1.Items.Add(nil,IntToStr(j));
    Root.ImageIndex     :=j mod 10;
    Root.SelectedIndex  :=j mod 10;
    for i:=0 to 99 do
    begin
      Node:=TreeView1.Items.AddChild(Root,Format('Child %d.%d',[j,i]));
      Node.ImageIndex     :=i mod 10;
      Node.SelectedIndex  :=i mod 10;
    end;
  end;

end;

procedure TFrmMain.SetOwnerDraw(Value: Boolean);
begin
  ComboBox1.Style:=csOwnerDrawFixed;
  ComboBox1.OnDrawItem:=VclStylesOwnerDrawFix.ComboBoxDrawItem;

  ListBox1.Style:=lbOwnerDrawFixed;
  ListBox1.OnDrawItem:=VclStylesOwnerDrawFix.ListBoxDrawItem;

  ListView1.OwnerDraw:=True;
  ListView1.OnDrawItem:=VclStylesOwnerDrawFix.ListViewDrawItem;
  ListView1.OnMouseDown:=VclStylesOwnerDrawFix.ListViewMouseDown;
end;

end.
