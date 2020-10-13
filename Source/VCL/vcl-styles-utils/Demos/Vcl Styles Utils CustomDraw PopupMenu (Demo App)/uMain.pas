unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.Styles.Utils.Menus, Vcl.StdCtrls;

type
  TMySysPopupStyleHook = class(TSysPopupStyleHook)
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure EraseItem(Canvas: TCanvas; const Index: integer;
      const ItemRect: TRect); override;
    procedure DrawItem(Canvas: TCanvas; const Index: integer; const ItemRect: TRect;
      const  ItemText: String; const  State: TSysPopupItemState;
      const Style: TSysPopupItemStyle); override;
  end;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    I1: TMenuItem;
    I2: TMenuItem;
    I3: TMenuItem;
    I4: TMenuItem;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
 Vcl.Styles.Utils.SysControls, System.Types;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

{ TMySysPopupStyleHook }

procedure TMySysPopupStyleHook.DrawItem(Canvas: TCanvas; const Index: integer;
  const ItemRect: TRect; const ItemText: String; const State: TSysPopupItemState;
  const Style: TSysPopupItemStyle);
var
 LItemRect : TRect;
begin
  { Skip Some Menu if You want .. }
  if Menu = GetSystemMenu(Form1.Handle, False) then
  begin
    inherited;
    Exit;
  end;

  SetBkMode(Canvas.Handle, TRANSPARENT);
  SetTextColor(Canvas.Handle, clBlack);

  if isHot in State then
    Canvas.Brush.Color := clWebGold;
  if isDisabled in State then
    Canvas.Brush.Color := clWebGray;
  if isDefault in State then
  begin
    Canvas.Font.Style := [fsBold];
    SetTextColor(Canvas.Handle, clWhite);
  end;
  // ...
  // ...
  // ... BlaBla ...
  Canvas.FillRect(ItemRect);
  LItemRect:=ItemRect;
  Inc(LItemRect.Left, 30);
  Winapi.Windows.DrawText(Canvas.Handle, ItemText, -1, LItemRect,
    DT_LEFT or DT_VCENTER);
end;

procedure TMySysPopupStyleHook.EraseItem(Canvas: TCanvas; const Index: integer;
  const ItemRect: TRect);
begin
  Canvas.Brush.Color := clWebGray;
  Canvas.FillRect(ItemRect);
end;

procedure TMySysPopupStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Color := clWebGray;
  Canvas.FillRect(SysControl.ClientRect);
end;

initialization

TSysStyleManager.RegisterSysStyleHook('#32768', TMySysPopupStyleHook);

finalization

TSysStyleManager.UnRegisterSysStyleHook('#32768', TMySysPopupStyleHook);

end.
