unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    RadioButton1: TRadioButton;
    CheckBox1: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

Uses
  Vcl.GraphUtil,
  Generics.Collections,
  Vcl.Themes,
  Vcl.Styles.Ext,
  Vcl.Styles.Utils,
  Vcl.Styles.Utils.Graphics;

{$R *.dfm}

procedure TFrmMain.Button1Click(Sender: TObject);
begin
end;

{
  This code shows how you can add a Overlay blend effect to an existing vcl style
  and then apply the changes in run-time.
}
procedure TFrmMain.Button2Click(Sender: TObject);
begin
  TCustomStyleExt(TStyleManager.ActiveStyle).SetStyleColor(scEdit, clRed);
  TStyleManager.RefreshCurrentTheme;
end;

procedure TFrmMain.Button3Click(Sender: TObject);
var
  VclStylesUtils: TVclStylesUtils;
  Filters: TObjectList<TBitmapFilter>;
begin
  // create the instance to the  TVclStylesUtils using the carbon vcl style
  VclStylesUtils := TVclStylesUtils.Create('Carbon');
  // create the filter list to apply
  Filters := TObjectList<TBitmapFilter>.Create;
  try
    // create a TBitmap32BlendOverlay filter and add to the list
    Filters.Add(TBitmap32BlendOverlay.Create(clGreen));
    // set the elements to be affected
    VclStylesUtils.Elements := VclStylesUtils.Elements + [vseBitmaps, vseSysColors, vseStyleColors];
    // set the filters
    VclStylesUtils.SetFilters(Filters);
    // Apply the changes to the style
    VclStylesUtils.ApplyChanges;
    // reload the modified style
    TStyleManager.ReloadStyle('Carbon');
  finally
    VclStylesUtils.Free;
    Filters.Free;
  end;
  TButton(Sender).Enabled := False;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  //ReportMemoryLeaksOnShutdown := True;
  TStyleManager.ReloadStyle('Carbon');
end;

end.
