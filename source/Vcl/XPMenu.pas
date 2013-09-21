{
XPMenu for Delphi
Author: Khaled Shagrouni
URL: http://www.shagrouni.com/english/software/xpmenu.html
e-mail: khaled@shagrouni.com

Version 3.1 - 22.02.2004



XPMenu is a Delphi component to mimic Office XP menu and toolbar style.
Copyright (C) 2001, 2003 Khaled Shagrouni.

This component is FREEWARE with source code. I still hold the copyright, but
you can use it for whatever you like: freeware, shareware or commercial software.
If you have any ideas for improvement or bug reports, don't hesitate to e-mail
me <khaled@shagrouni.com> (Please state the XPMenu version and OS information).
}

{$IFDEF VER130}
{$DEFINE VER5U}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE VER5U}
{$DEFINE VER6U}
{$ENDIF}

{$IFDEF VER150}
{$DEFINE VER5U}
{$DEFINE VER6U}
{$DEFINE VER7U}
{$ENDIF}

unit XPMenu;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, Forms,
  Menus, Commctrl, ExtCtrls, StdCtrls, Buttons;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QComCtrls, QForms,
  QMenus, Commctrl, QStdCtrls, QButtons;
{$ENDIF}

type
  TXPContainer = (xccForm, xccFrame, xccToolbar, xccCoolbar, xccControlbar, xccPanel,
                  xccScrollBox, xccGroupBox, xccTabSheet, xccPageScroller);
  TXPContainers = set of TXPContainer;

  TXPControl = (xcMainMenu, xcPopupMenu, xcToolbar, xcControlbar, xcCombo, xcListBox,
                xcEdit, xcMaskEdit, xcMemo, xcRichEdit, xcMiscEdit, xcCheckBox,
                xcRadioButton, xcButton, xcBitBtn, xcSpeedButton, xcUpDown, xcPanel,
                xcGroupBox, xcTreeView, xcListView, xcProgressBar, xcHotKey);
                {xcStringGrid, xcDrawGrid, xcDBGrid);}

  TXPControls = set of TXPControl;

  TXPMenu = class;

  TControlSubClass = class(TComponent)   //:   "Fabian Jakubowski" <fj@sambreville.com>
  private
    Control: TControl;
    FBuilding: boolean;
    FMouseInControl: boolean;
    FLButtonBressed: boolean;
    FBressed: boolean;
    FIsKeyDown: boolean;
    FIsFocused: boolean;
    orgWindowProc: TWndMethod;
    XPMenu: TXPMenu;
    FCtl3D: boolean;
    FBorderStyle: TBorderStyle;
  {FOnDrawCell: TDrawCellEvent;}
    FDefaultDrawing: boolean;
    FSelCol, FSelRow: integer;
    FMsg: Cardinal;
    procedure ControlSubClass(var Message: TMessage);
    procedure PaintControlXP;
    procedure PaintCombo;
    procedure PaintDBLookupCombo;
    procedure PaintEdit;
    procedure PaintRichEdit;
    procedure PaintCheckBox;
    procedure PaintRadio;
    procedure PaintButton;
    procedure PaintBitButn;
    procedure PaintUpDownButton;
    procedure PaintSpeedButton;
    procedure PaintPanel;
    procedure PaintGroupBox;
    procedure PaintNCWinControl;
    procedure PaintProgressBar;
    procedure PaintHotKey;
  end;

  TXPMenu = class(TComponent)
  private
    FActive: boolean;
    {Changes MMK FForm to TScrollingWinControl}
    FForm: TScrollingWinControl;
    FFont: TFont;
    FColor: TColor;
    FIconBackColor: TColor;
    FMenuBarColor: TColor;
    FCheckedColor: TColor;
    FSeparatorColor: TColor;
    FSelectBorderColor: TColor;
    FSelectColor: TColor;
    FDisabledColor: TColor;
    FSelectFontColor: TColor;
    FIconWidth: integer;
    FDrawSelect: boolean;
    FUseSystemColors: boolean;
    FColorsChanged: boolean; // +jt

    FFColor, FFIconBackColor, FFSelectColor, FFSelectBorderColor,
    FFSelectFontColor, FCheckedAreaColor, FCheckedAreaSelectColor,
    FFCheckedColor, FFMenuBarColor, FFDisabledColor, FFSeparatorColor,
    FMenuBorderColor, FMenuShadowColor: TColor;

    Is16Bit: boolean;
    FOverrideOwnerDraw: boolean;
    FGradient: boolean;
    FFlatMenu: boolean;
    FAutoDetect: boolean;
    FXPContainers: TXPContainers;
    FXPControls: TXPControls;
    FGrayLevel: byte;
    FDimLevel: byte;
    FDrawMenuBar: boolean;
    FUseDimColor: boolean;
    FDimParentColor, FDimParentColorSelect: integer;
   // FUseParentClor: boolean;
// +jt
    FSettingWindowRng: boolean;
   FIsW2k:            boolean;
   FIsWXP:            boolean;
   FIsWNT:            boolean;
//   FTransparentColor: TColor;
// +jt

   // Do not allow the component to be used for subclassing
    FDisableSubclassing: boolean;
    procedure SetDisableSubclassing(const Value: boolean);

    procedure SetActive(const Value: boolean);
    procedure SetAutoDetect(const Value: boolean);
    procedure SetForm(const Value: TScrollingWinControl);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetIconBackColor(const Value: TColor);
    procedure SetMenuBarColor(const Value: TColor);
    procedure SetCheckedColor(const Value: TColor);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetSelectColor(const Value: TColor);
    procedure SetSelectBorderColor(const Value: TColor);
    procedure SetSeparatorColor(const Value: TColor);
    procedure SetSelectFontColor(const Value: TColor);
    procedure SetIconWidth(const Value: integer);
    procedure SetDrawSelect(const Value: boolean);
    procedure SetUseSystemColors(const Value: boolean);
    procedure SetOverrideOwnerDraw(const Value: boolean);
    procedure SetGradient(const Value: boolean);
    procedure SetFlatMenu(const Value: boolean);
    procedure SetXPContainers(const Value: TXPContainers);
    procedure SetXPControls(const Value: TXPControls);
    procedure SetDrawMenuBar(const Value: boolean);
    procedure SetUseDimColor(const Value: boolean);

  protected
    procedure Loaded; override; //add by Cunha, liyang.
    procedure InitItems(wForm: TWinControl; Enable, Update: boolean);
    procedure InitItem(Comp: TComponent; Enable, Update: boolean); // Tom: "Thomas Knoblauch" <thomas@tom-the-bomb.de> 27.08
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    procedure MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    {$IFDEF VER5U}
    procedure ToolBarDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);
    {$ENDIF}
    procedure ControlBarPaint(Sender: TObject; Control: TControl;
      Canvas: TCanvas; var ARect: TRect; var Options: TBandPaintOptions);

    procedure SetGlobalColor(ACanvas: TCanvas);
    procedure DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      BckColor:Tcolor; IsRightToLeft: boolean);
    procedure DrawCheckedItem(FMenuItem: TMenuItem; Selected, Enabled,
     HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
    procedure DrawTheText(Sender: TObject; txt, ShortCuttext: string;
       ACanvas: TCanvas; TextRect: TRect;
       Selected, Enabled, Default, TopMenu, IsRightToLeft: boolean;
       var TxtFont: TFont; TextFormat: integer);
    procedure DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
     IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
     IsRightToLeft: boolean);

    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);

    //function GetImageExtent(MenuItem: TMenuItem): TPoint;
    function GetImageExtent(MenuItem: TMenuItem; FTopMenu: TMenu): TPoint; // +jt
    function TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
    procedure DrawGradient(ACanvas: TCanvas; ARect: TRect;
     IsRightToLeft: boolean);

    procedure DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitComponent(Comp: TComponent); // Tom: Added for usage by the main program ."Thomas Knoblauch" <thomas@tom-the-bomb.de> 27.08
    procedure ActivateMenuItem(MenuItem: TMenuItem; SubMenus: boolean); // +jt
    property Form: TScrollingWinControl read FForm write SetForm;
// +jt
    property IsWXP: boolean read FIsWXP;
   property IsW2k: boolean read FIsW2k;
   property IsWNT: boolean read FIsWNT;
//   property TransparentColor: TColor read FTransparentColor write FTransparentColor;
// +jt
  published
    property DimLevel: Byte read FDimLevel write FDimLevel;
    property GrayLevel: Byte read FGrayLevel write FGrayLevel;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
    property DrawMenuBar: boolean read FDrawMenuBar write SetDrawMenuBar;
    property IconBackColor: TColor read FIconBackColor write SetIconBackColor;
    property MenuBarColor: TColor read FMenuBarColor write SetMenuBarColor;
    property SelectColor: TColor read FSelectColor write SetSelectColor;
    property SelectBorderColor: TColor read FSelectBorderColor
     write SetSelectBorderColor;
    property SelectFontColor: TColor read FSelectFontColor
     write SetSelectFontColor;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor;
    property SeparatorColor: TColor read FSeparatorColor
     write SetSeparatorColor;
    property CheckedColor: TColor read FCheckedColor write SetCheckedColor;
    property IconWidth: integer read FIconWidth write SetIconWidth;
    property DrawSelect: boolean read FDrawSelect write SetDrawSelect;
    property UseSystemColors: boolean read FUseSystemColors
     write SetUseSystemColors;
    property UseDimColor: boolean read FUseDimColor write SetUseDimColor;
    property OverrideOwnerDraw: boolean read FOverrideOwnerDraw
     write SetOverrideOwnerDraw;

    property Gradient: boolean read FGradient write SetGradient;
    property FlatMenu: boolean read FFlatMenu write SetFlatMenu;
    property AutoDetect: boolean read FAutoDetect write SetAutoDetect;
    property XPContainers: TXPContainers read FXPContainers write SetXPContainers
      default [xccForm, xccFrame, xccToolbar, xccCoolbar, xccControlbar, xccPanel,
                  xccScrollBox, xccGroupBox, xccTabSheet, xccPageScroller];
    property XPControls :TXPControls read FXPControls write SetXPControls
      default [xcMainMenu, xcPopupMenu, xcToolbar, xcControlbar, xcCombo, xcListBox,
               xcEdit, xcMaskEdit, xcMemo, xcRichEdit, xcMiscEdit, xcCheckBox,
               xcRadioButton, xcButton, xcBitBtn, xcSpeedButton, xcUpDown, xcPanel,
               xcGroupBox, xcTreeView, xcListView, xcProgressBar, xcHotKey];
               {xcStringGrid, xcDrawGrid, xcDBGrid];}

    property Active: boolean read FActive write SetActive;
    property DisableSubclassing: boolean read FDisableSubclassing write SetDisablesubclassing
      default false;
  end;

  TXPMenuManager = class(TPersistent)
  private
    FXPMenuList: TList;
    FPendingFormsList: TList;
    FFormList: TList;
    FActiveXPMenu: TXPMenu;
    FDisableSubclassing: boolean;

    function MainWindowHook(var Message: TMessage): boolean;
    procedure CollectForms;
    procedure RemoveChildSubclassing(AForm: TCustomForm);
    procedure SetDisableSubclassing(AValue: boolean);
    function FindSubclassingXPMenu(Exclude: TXPMenu): TXPMenu;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AXPMenu: TXPMenu);
    procedure Delete(AXPMenu: TXPMenu);
    procedure UpdateActiveXPMenu(AXPMenu: TXPMenu);
    procedure AddForm(AForm: TCustomForm);
    procedure RemoveForm(AForm: TCustomForm);
    function IsFormSubclassed(AForm: TCustomForm): boolean;
    function IsComponentSubclassed(AComponent: TComponent): boolean;

    property ActiveXPMenu: TXPMenu read FActiveXPMenu;
    property DisableSubclassing: boolean read FDisableSubclassing write SetDisableSubclassing
      default false;
  end;

function GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
function MergColor(Colors: Array of TColor): TColor;
function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure DimBitmap(ABitmap: TBitmap; Value: integer);

procedure DrawArrow(ACanvas: TCanvas; X, Y: integer); overload;
procedure DrawArrow(ACanvas: TCanvas; X, Y, Orientation: integer); overload;
function GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
function GetInverseColor(AColor: TColor): TColor;

procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);
procedure DrawCheckMark(ACanvas: TCanvas; X, Y: integer);

procedure GetSystemMenuFont(Font: TFont);
procedure Register;

const
  WM_DRAWMENUBORDER     = CN_NOTIFY + 101;   // +jt
  WM_DRAWMENUBORDER2    = CN_NOTIFY + 102;   // +jt

// Gloabal access to the XPMenuManager
var
  XPMenuManager: TXPMenuManager;

implementation

procedure Register;
begin
  RegisterComponents('XP', [TXPMenu]);
end;

// Set up the global variable that represents the XPMenuManager
procedure InitControls;
begin
  if XPMenuManager = nil then
    XPMenuManager := TXPMenuManager.Create;
end;

// Delete the global variable that represents the XPMenuManager
procedure DoneControls;
begin
  if (XPMenuManager <> nil) then
  begin
    XPMenuManager.Free;
    XPMenuManager := nil;
  end;
end;

// Test if mouse cursor is in the given rect of the application's main form
function IsMouseInRect(TheForm: TScrollingWinControl; DestRect: TRect): boolean;
var
  p: TPoint;

begin

  if Assigned(TheForm) then
  begin
    p := Mouse.CursorPos;
    p.x := p.x - TheForm.Left;
    p.y := p.y - TheForm.Top;

    Dec(DestRect.Right);
    Dec(DestRect.Bottom, 2);
    Result := (p.x >= DestRect.Left) and (p.x <= DestRect.Right) and
              (p.y >= DestRect.Top) and (p.y <= DestRect.Bottom);
  end
  else Result := false;
end;

{ TXPMenue }

constructor TXPMenu.Create(AOwner: TComponent);
var
  OSVersionInfo: TOSVersionInfo; // +jt
begin
  inherited Create(AOwner);
  FFont := TFont.Create;

  FDisableSubclassing := false;        // enable XPMenu to be used for global subclassing


{$IFDEF VER5U}
  FFont.Assign(Screen.MenuFont);
{$ELSE}
  GetSystemMenuFont(FFont);
{$ENDIF}
  FForm := Owner as TScrollingWinControl;

  FUseSystemColors := true;

  FColor := clBtnFace;
  FIconBackColor := clBtnFace;
  FSelectColor := clHighlight;
  FSelectBorderColor := clHighlight;
  FMenuBarColor := clBtnFace;
  FDisabledColor := clInactiveCaption;
  FSeparatorColor := clBtnFace;
  FCheckedColor := clHighlight;
  FSelectFontColor := FFont.Color;
  FGrayLevel := 10;
  FDimLevel := 30;
  FIconWidth := 24;
  FDrawSelect := true;
  XPContainers := [xccForm, xccFrame, xccToolbar, xccCoolbar, xccControlbar, xccPanel,
                  xccScrollBox, xccGroupBox, xccTabSheet, xccPageScroller];
  XPControls := [xcMainMenu, xcPopupMenu, xcToolbar, xcControlbar, xcCombo, xcListBox,
                xcEdit, xcMaskEdit, xcMemo, xcRichEdit, xcMiscEdit, xcCheckBox,
                xcRadioButton, xcButton, xcBitBtn, xcSpeedButton, xcUpDown, xcPanel,
                xcGroupBox, xcTreeView, xcListView, xcProgressBar, xcHotKey];
            {xcStringGrid, xcDrawGrid, xcDBGrid];}

  if Assigned(FForm) then
    SetGlobalColor(TForm(FForm).Canvas);

// +jt
// FTransparentColor := clFuchsia;
 FColorsChanged    := false;
 OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
 GetVersionEx(OSVersionInfo);
 FIsWXP:=false;
 FIsW2k:=false;
 FIsWNT:=false;
 if OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
 begin
   FIsWNT:=true;
   if (OSVersionInfo.dwMajorVersion = 5) and (OSVersionInfo.dwMinorVersion = 0) then FIsW2k:=true;
   if (OSVersionInfo.dwMajorVersion = 5) and (OSVersionInfo.dwMinorVersion = 1) then FIsWXP:=true;
 end;
// +jt

  if not(csDesigning in ComponentState) then
    InitControls;
end;

destructor TXPMenu.Destroy;
begin
  if Assigned(FForm) then    //oleg oleg@vdv-s.ru  Mon Oct  7
    InitItems(FForm, false, false);

// Remove XPMenu from XPMenuManager
  if Assigned(XPMenuManager) and not(csDesigning in ComponentState) then
  begin
    XPMenuManager.Delete(Self);
    FForm.Update;
    if XPMenuManager.FXPMenuList.Count = 0 then
      DoneControls;
  end;

  FFont.Free;
  inherited;
end;

//add by:
//liyang <liyang@guangdainfo.com> ,2002-07-19
//Pedro Miguel Cunha <PCunha@codeware.pt>- 02 Apr 2002
procedure TXPMenu.Loaded;
begin
  inherited Loaded;

// Add the XPMenu to the XPMenuManager
  if Assigned(XPMenuManager) and not(csDesigning in ComponentState) then
    XPMenuManager.Add(Self);
end;

{to check for new sub items}
procedure TXPMenu.ActivateMenuItem(MenuItem: TMenuItem; SubMenus: boolean); // +jt

  procedure Activate(MenuItem: TMenuItem);
  begin
    if (MenuItem.Tag <> 999) then
    if addr(MenuItem.OnDrawItem) <> addr(TXPMenu.DrawItem) then
    begin
      if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnDrawItem := DrawItem;
      if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnMeasureItem := MeasureItem;
    end
  end;

var
  i{, j}: integer;
begin

  Activate(MenuItem);
  if (SubMenus=true) then // +jt
  begin
   for i := 0 to MenuItem.Count -1 do
   begin
     ActivateMenuItem(MenuItem.Items[i],true);
   end;
 end;
end;

procedure TXPMenu.InitItems(wForm: TWinControl; Enable, Update: boolean );
var
  i: integer;
  Comp: TComponent;
begin
  for i := 0 to wForm.ComponentCount - 1 do
  begin
    Comp := wForm.Components[i];
    InitItem(Comp, Enable, Update); // Tom: "Thomas Knoblauch" <thomas@tom-the-bomb.de> 27.08
  end;
end;

procedure TXPMenu.InitComponent(Comp: TComponent); // Tom: for external (by the main program) use without parameters. "Thomas Knoblauch" <thomas@tom-the-bomb.de> 27.08
begin
  if FActive then InitItem(Comp, true, true);
end;



// Tom: "Thomas Knoblauch" <thomas@tom-the-bomb.de> 27.08
procedure TXPMenu.InitItem(Comp: TComponent; Enable, Update: boolean );
  procedure Activate(MenuItem: TMenuItem);
  begin
    if Enable then
    begin
      if (MenuItem.Tag <> 999) then
      begin
        if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
          MenuItem.OnDrawItem := DrawItem;
        if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
          MenuItem.OnMeasureItem := MeasureItem;
      end;
    end
    else
    begin
      if addr(MenuItem.OnDrawItem) = addr(TXPMenu.DrawItem) then
        MenuItem.OnDrawItem := nil;
      if addr(MenuItem.OnMeasureItem) = addr(TXPMenu.MeasureItem) then
        MenuItem.OnMeasureItem := nil;
    end;
  end;

  procedure ItrateMenu(MenuItem: TMenuItem);
  var
    i: integer;
  begin
    Activate(MenuItem);
    for i := 0 to MenuItem.Count - 1 do
    begin
      ItrateMenu(MenuItem.Items[i]);
    end;
  end;

var
  x: integer;
  s: string;

begin
  if (Comp is TMainMenu) and (xcMainMenu in XPControls) and (TMainMenu(Comp).Tag <> 999)then
  begin
    for x := 0 to TMainMenu(Comp).Items.Count - 1 do
    begin
      TMainMenu(Comp).OwnerDraw := Enable;
      //Activate(TMainMenu(Comp).Items[x]);
      ItrateMenu(TMainMenu(Comp).Items[x]);
    end;
   // Selly way to force top menu in other forms to repaint
    S := TMainMenu(Comp).Items[0].Caption;
    TMainMenu(Comp).Items[0].Caption := '';
    TMainMenu(Comp).Items[0].Caption := S;
  end;

  if (Comp is TPopupMenu) and (xcPopupMenu in XPControls) then
  begin
    for x := 0 to TPopupMenu(Comp).Items.Count - 1 do
    begin
      TPopupMenu(Comp).OwnerDraw := Enable;
      ItrateMenu(TPopupMenu(Comp).Items[x]);

    end;
  end;

  {$IFDEF VER5U}
  if (Comp is TToolBar) and (xcToolBar in FXPControls) then
    if not (csDesigning in ComponentState) then
    begin
      if not TToolBar(Comp).Flat then
        TToolBar(Comp).Flat := true;

      if Enable then
      begin
        for x := 0 to TToolBar(Comp).ButtonCount - 1 do
          if (not assigned(TToolBar(Comp).OnCustomDrawButton))
            or (FOverrideOwnerDraw) then
          begin
            TToolBar(Comp).OnCustomDrawButton :=
              ToolBarDrawButton;

          end;
      end
      else
      begin
        if addr(TToolBar(Comp).OnCustomDrawButton) =
          addr(TXPMenu.ToolBarDrawButton) then
        TToolBar(Comp).OnCustomDrawButton := nil;
      end;
      if Update then
        TToolBar(Comp).Invalidate;
    end;
  {$ENDIF}

  if (Comp is TControlBar) and (xcControlBar in FXPControls) then
    if not (csDesigning in ComponentState) then
    begin
      if Enable then
      begin
        if (not assigned(TControlBar(Comp).OnBandPaint))
          or (FOverrideOwnerDraw) then
        begin
          TControlBar(Comp).OnBandPaint := ControlBarPaint;
        end;
      end
      else
      begin
        if addr(TControlBar(Comp).OnBandPaint) =
          addr(TXPMenu.ControlBarPaint) then
        TControlBar(Comp).OnBandPaint := nil;
      end;
      if Update then
        TControlBar(Comp).Invalidate;
    end;

  if not (csDesigning in ComponentState) then
    if {$IFDEF VER6U}
       ((Comp is TCustomCombo) and (xcCombo in FXPControls)) or
       ((Comp is TCustomLabeledEdit) and (xcEdit in FXPControls)) or

       {$ELSE}
       ((Comp is TCustomComboBox) and (xcCombo in FXPControls)) or
       {$ENDIF}
       ((Comp is TEdit) and (xcEdit in FXPControls)) or
       ((Comp.ClassName = 'TMaskEdit') and (xcMaskEdit in FXPControls)) or
       ((Comp.ClassName = 'TDBEdit') and (xcMaskEdit in FXPControls)) or
       ((Comp is TCustomMemo) and (xcMemo in FXPControls)) or
       ((Comp is TCustomRichEdit) and (xcRichEdit in FXPControls)) or
       ((Comp is TCustomCheckBox) and (xcCheckBox in FXPControls)) or
       ((Comp is TRadioButton) and (xcRadioButton in FXPControls)) or
       ((Comp.ClassName = 'TBitBtn') and (xcBitBtn in FXPControls)) or
       ((Comp.ClassName = 'TButton') and (xcButton in FXPControls)) or
       ((Comp.ClassName = 'TUpDown') and (xcUpDown in FXPControls)) or
       ((Comp is TSpeedButton) and (xcSpeedButton in FXPControls)) or
       ((Comp is TCustomPanel) and (xcPanel in FXPControls)) or
       ((Comp.ClassName = 'TDBNavigator') and (xcButton in FXPControls)) or
       ((Comp.ClassName = 'TDBLookupComboBox') and (xcButton in FXPControls)) or
       ((Comp is TCustomGroupBox) and (xcGroupBox in FXPControls)) or
       ((Comp is TCustomListBox) and (xcListBox in FXPControls)) or
       ((Comp is TCustomTreeView) and (xcTreeView in FXPControls)) or
       ((Comp is TCustomListView) and (xcListView in FXPControls)) or
       ((Comp is TProgressBar) and (xcProgressBar in FXPControls)) or
       ((Comp is TCustomHotKey) and (xcHotKey in FXPControls))
       then
      if ((TControl(Comp).Parent is TToolbar) and (xccToolBar in FXPContainers))or
         ((TControl(Comp).Parent is TCoolbar) and (xccCoolbar in FXPContainers)) or
         ((TControl(Comp).Parent is TCustomPanel) and (xccPanel in FXPContainers)) or
         ((TControl(Comp).Parent is TControlbar) and (xccControlbar in FXPContainers)) or
         ((TControl(Comp).Parent is TScrollBox) and (xccScrollBox in FXPContainers)) or
         ((TControl(Comp).Parent is TCustomGroupBox) and (xccGroupBox in FXPContainers)) or
         ((TControl(Comp).Parent is TTabSheet) and (xccTabSheet in FXPContainers)) or
         ((TControl(Comp).Parent is TTabControl) and (xccTabSheet in FXPContainers)) or
         ((TControl(Comp).Parent.ClassName = 'TdxTabSheet') and (xccTabSheet in FXPContainers)) or //DeveloperExpress
         ((TControl(Comp).Parent is TPageScroller)  and (xccPageScroller in FXPContainers)) or
         {$IFDEF VER5U}
         ((TControl(Comp).Parent is TCustomFrame)  and (xccFrame in FXPContainers)) or
         {$ENDIF}
         ((TControl(Comp).Parent.ClassName = 'TDBCtrlPanel')  and (xccFrame in FXPContainers)) or
         ((TControl(Comp).Parent is TCustomForm) and (xccForm in FXPContainers))


         then
      begin
        if (Enable) and (Comp.Tag <> 999) and (TControl(Comp).Parent.Tag <> 999) then
                                    {skip if Control/Control.parent.tag = 999}
          with TControlSubClass.Create(Self)  do
          begin
            Control := TControl(Comp);
            if Addr(Control.WindowProc) <> Addr(TControlSubClass.ControlSubClass) then
            begin
              orgWindowProc := Control.WindowProc;
              Control.WindowProc := ControlSubClass;
            end;
            XPMenu := self;

            if (Control is TCustomEdit) then
            begin
              FCtl3D := TEdit(Control).Ctl3D;
              FBorderStyle := TRichEdit(Control).BorderStyle;
            end;
            if Control.ClassName = 'TDBLookupComboBox' then
            begin
              FCtl3D := TComboBox(Control).Ctl3D;
            end;
            if (Control is TCustomListBox) then
            begin
              FCtl3D := TListBox(Control).Ctl3D;
              FBorderStyle := TListBox(Control).BorderStyle;
            end;
            if (Control is TCustomListView) then begin
              FCtl3D := TListView(Control).Ctl3D;
              FBorderStyle := TListView(Control).BorderStyle;
            end;
            if (Control is TCustomTreeView) then begin
              FCtl3D := TTreeView(Control).Ctl3D;
              FBorderStyle := TTreeView(Control).BorderStyle;
            end;

          end;

        if Update then
        begin
          TControl(Comp).invalidate    //in TControlSubClass.ControlSubClass
        end;

      end;

  // Recursive call for possible containers.


  // Do recursive call for RadioGroups
  if (((Comp is TCustomRadioGroup)) and (xccGroupBox in FXPContainers)) then
    self.InitItems(Comp as TWinControl, Enable, Update);


  if {$IFDEF VER5U}((Comp is TCustomFrame) and (xccFrame in FXPContainers))
     or {$ENDIF}(Comp.ClassName = 'TDBNavigator')
   or (Comp is TCustomForm) then  //By Geir Wikran <gwikran@online.no>
    self.InitItems(Comp as TWinControl, Enable, Update);


end;

procedure TXPMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
begin
  try  //"Steve Rice" <srice@pclink.com>
    if FActive then
      MenueDrawItem(Sender, ACanvas, ARect, Selected);
  except
  end;
end;

function TXPMenu.GetImageExtent(MenuItem: TMenuItem; FTopMenu: TMenu): TPoint;
var
  HasImgLstBitmap: boolean;
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.Width := 0;
    B.Height := 0;
    Result.x := 0;
    Result.Y := 0;
    HasImgLstBitmap := false;
// +jt
    if Assigned(FTopMenu) then
   begin
     if FTopMenu.Images <> nil then
       if MenuItem.ImageIndex <> -1 then
         HasImgLstBitmap := true;
    end;

    if (MenuItem.Parent.GetParentMenu.Images <> nil)
    {$IFDEF VER5U}
    or (MenuItem.Parent.SubMenuImages <> nil)
    {$ENDIF}
    then
    begin
      if MenuItem.ImageIndex <> -1 then
        HasImgLstBitmap := true
      else
        HasImgLstBitmap := false;
    end;

    if HasImgLstBitmap then
    begin
    {$IFDEF VER5U}
      if MenuItem.Parent.SubMenuImages <> nil then
        MenuItem.Parent.SubMenuImages.GetBitmap(MenuItem.ImageIndex, B)
      else
    {$ENDIF}
        MenuItem.Parent.GetParentMenu.Images.GetBitmap(MenuItem.ImageIndex, B)
    end
    else
      if MenuItem.Bitmap.Width > 0 then
        B.Assign(TBitmap(MenuItem.Bitmap));

    Result.x := B.Width;
    Result.Y := B.Height;

    if not Assigned(FTopMenu) then // +jt
      if Result.x < FIconWidth then
        Result.x := FIconWidth;
  finally
    B.Free;
  end;
end;

procedure TXPMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  s: string;
  W, H: integer;
  P: TPoint;
  IsLine: boolean;
  FTopMenu: boolean; // +jt
  FMenu: TMenu; // +jt
  i: integer; // +jt
begin
 
 FTopMenu:=false; //+jt
  if FActive then
  begin
    S := TMenuItem(Sender).Caption;

    if S = '-' then IsLine := true else IsLine := false;
    if IsLine then
      S := '';

    if Trim(ShortCutToText(TMenuItem(Sender).ShortCut)) <> '' then
      S := S + ShortCutToText(TMenuItem(Sender).ShortCut) + 'WWW';

    ACanvas.Font.Assign(FFont);
    W := ACanvas.TextWidth(s);
    Inc(W, 5);
    if pos('&', s) > 0 then
      W := W - ACanvas.TextWidth('&');

// +jt
    FMenu := TMenuItem(Sender).Parent.GetParentMenu;
    if FMenu is TMainMenu then
   begin
     for i := 0 to TMenuItem(Sender).GetParentMenu.Items.Count - 1 do
       if TMenuItem(Sender).GetParentMenu.Items[i] = TMenuItem(Sender) then
       begin
         FTopMenu := True;
         break;
       end
   end;
   if not FTopMenu then FMenu := nil;
   if(not FTopMenu) and (TMenuItem(Sender).Count>0) then Inc(W,6); // +jt
// +jt

    P := GetImageExtent(TMenuItem(Sender), FMenu); // +jt
    W := W + P.x ;


    if Width < W then
      Width := W;

    if IsLine then
      Height := 4
    else
    begin
      H := ACanvas.TextHeight(s) + Round(ACanvas.TextHeight(s) * 0.75);
      if P.y + 6 > H then
        H := P.y + 6;

      if Height < H then
        Height := H;
    end;
  end;

end;

procedure TXPMenu.MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
var
  txt: string;
  B: TBitmap;
  IconRect, TextRect, CheckedRect: TRect;
  FillRect: TRect; // +jt
  i, X1, X2: integer;
  TextFormat: integer;
  HasImgLstBitmap: boolean;
  HasBitmap: boolean;
  FMenuItem: TMenuItem;
  FMenu: TMenu;
  FTopMenu: boolean;
  IsLine: boolean;
  ImgListHandle: HImageList;  {Commctrl.pas}
  ImgIndex: integer;
  hWndM: HWND;
  hDcM: HDC;
  DrawTopMenuBorder: boolean;
  msg: TMSG; // +jt
  buff: TBitmap; // +jt
 OrigRect: TRect; // +jt
 OrigCanvas: TCanvas; // +jt
begin


  OrigCanvas:= nil;

  FTopMenu := false;
  FMenuItem := TMenuItem(Sender);

// +jt
  B := TBitmap.Create;
  buff := TBitmap.Create;
  try
   origrect:= ARect;
   Dec(origrect.Left,4);
   origcanvas:=ACanvas;
   ARect.Right:=(ARect.Right-ARect.Left)+4;
   ARect.Bottom:=ARect.Bottom-ARect.Top;
   ARect.Left:=4;
   ARect.Top:=0;
   buff.Width := ARect.Right;
   buff.Height := ARect.Bottom;
   ACanvas:=buff.Canvas;
  // +jt
  //SetGlobalColor(ACanvas);

  if FMenuItem.Caption = '-' then IsLine := true else IsLine := false;

  FMenu := FMenuItem.Parent.GetParentMenu;

  if FMenu is TMainMenu then
    for i := 0 to FMenuItem.GetParentMenu.Items.Count - 1 do
      if FMenuItem.GetParentMenu.Items[i] = FMenuItem then
      begin
        FTopMenu := True;
  // +jt
        ARect.Left:=0;
       Inc(origrect.Left,4);
       Dec(ARect.Right,4);
       buff.Width:=ARect.Right;
       Dec(ARect.Bottom,1);
  // +jt
        break;
      end;
  if(FColorsChanged) then SetGlobalColor(ACanvas); // +jt

  ACanvas.Font.Assign(FFont);

  Inc(ARect.Bottom, 1);
  TextRect := ARect;
  txt := ' ' + FMenuItem.Caption;

//  B := TBitmap.Create;     //Leslie Cutting lesnes@absamail.co.za  Jul 8 2003
  HasBitmap := false;
  HasImgLstBitmap := false;


  if (FMenuItem.Parent.GetParentMenu.Images <> nil)
  {$IFDEF VER5U}
  or (FMenuItem.Parent.SubMenuImages <> nil)
  {$ENDIF}
  then
  begin
    if FMenuItem.ImageIndex <> -1 then
      HasImgLstBitmap := true
    else
      HasImgLstBitmap := false;
  end;

  if FMenuItem.Bitmap.Width  > 0 then
    HasBitmap := true;

  //-------
  if HasBitmap then
    begin
      B.Width := FMenuItem.Bitmap.Width;
      B.Height := FMenuItem.Bitmap.Height;
  // +jt
     //B.Canvas.Brush.Color := FTransparentColor; // ACanvas.Brush.Color;
     B.Canvas.Brush.Color := B.Canvas.Pixels[0, B.Height - 1];//"Todd Asher" <ashert@yadasystems.com>
     B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
     FMenuItem.Bitmap.Transparent := true;
     FMenuItem.Bitmap.TransparentMode := tmAuto;
      B.Canvas.Draw(0,0,FMenuItem.Bitmap);
  // +jt
    end;


  if HasImgLstBitmap then
  begin
  {$IFDEF VER5U}
    if FMenuItem.Parent.SubMenuImages <> nil then
    begin
      ImgListHandle := FMenuItem.Parent.SubMenuImages.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.SubMenuImages.Width;
      B.Height := FMenuItem.Parent.SubMenuImages.Height;
     // B.Canvas.Brush.Color := FTransparentColor; // ACanvas.Brush.Color; // +jt
      B.Canvas.Brush.Color := B.Canvas.Pixels[0, B.Height - 1];//"Todd Asher" <ashert@yadasystems.com>
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end
    else
  {$ENDIF}
    if FMenuItem.Parent.GetParentMenu.Images <> nil then
    begin
      ImgListHandle := FMenuItem.Parent.GetParentMenu.Images.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.GetParentMenu.Images.Width;
      B.Height := FMenuItem.Parent.GetParentMenu.Images.Height;
      //B.Canvas.Brush.Color := FTransparentColor; //ACanvas.Pixels[2,2]; // +jt
      B.Canvas.Brush.Color := B.Canvas.Pixels[0, B.Height - 1];//"Todd Asher" <ashert@yadasystems.com>
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end;

  end;

  //-----

  if FMenu.IsRightToLeft then
  begin
    X1 := ARect.Right - FIconWidth;
    X2 := ARect.Right;
  end
  else
  begin
    X1 := ARect.Left;
    X2 := ARect.Left + FIconWidth;
  end;
  IconRect := Rect(X1, ARect.Top, X2, ARect.Bottom);


  if HasImgLstBitmap or HasBitmap then
  begin
    CheckedRect := IconRect;
    Inc(CheckedRect.Left, 1);
    Inc(CheckedRect.Top, 2);
    Dec(CheckedRect.Right, 3);
    Dec(CheckedRect.Bottom, 2);
  end
  else
  begin
    CheckedRect.Left := IconRect.Left +
      (IConRect.Right - IconRect.Left - 10) div 2;
    CheckedRect.Top := IconRect.Top +
      (IConRect.Bottom - IconRect.Top - 10) div 2;
    CheckedRect.Right := CheckedRect.Left + 10;
    CheckedRect.Bottom := CheckedRect.Top + 10;
  end;

  if B.Width > FIconWidth then
    if FMenu.IsRightToLeft then
      CheckedRect.Left := CheckedRect.Right - B.Width
    else
      CheckedRect.Right := CheckedRect.Left + B.Width;

  if FTopMenu then Dec(CheckedRect.Top, 1);


  if FMenu.IsRightToLeft then
  begin
    X1 := ARect.Left;
    if not FTopMenu then
      Dec(X2, FIconWidth)
    else
      Dec(X2, 4);
    if (ARect.Right - B.Width) < X2 then
      X2 := ARect.Right - B.Width - 8;
  end
  else
  begin
    X1 := ARect.Left ;
    if not FTopMenu then
      Inc(X1, FIconWidth)
    else
      Inc(X1, 4);

    if (ARect.Left + B.Width) > X1 then
      X1 := ARect.Left + B.Width + 4;
    X2 := ARect.Right;
  end;

  TextRect := Rect(X1, ARect.Top, X2, ARect.Bottom);
  // +jt
  FillRect := ARect;
  Dec(FillRect.Left,4);
  // +jt

  if FTopMenu then
  begin
    if not (HasImgLstBitmap or HasBitmap) then
    begin
      TextRect := ARect;
    end
    else
    begin
      if FMenu.IsRightToLeft then
        TextRect.Right := TextRect.Right + 5
      else
        TextRect.Left := TextRect.Left - 5;
    end

  end;

  if FTopMenu then
  begin
    if FDrawMenuBar then
      FFMenuBarColor := FMenuBarColor;
    ACanvas.brush.color := FFMenuBarColor;
    ACanvas.Pen.Color := FFMenuBarColor;
  //    Inc(ARect.Bottom, 2);
    ACanvas.FillRect(ARect);

  //--
    if FDrawMenuBar then
    begin
      if FMenuItem.GetParentMenu.Items[FMenuItem.GetParentMenu.Items.Count-1] =
         FMenuItem then
      begin
        if FMenu.IsRightToLeft then
          ACanvas.Rectangle(3, ARect.Top, ARect.Right, ARect.Bottom)
        else
          ACanvas.Rectangle(ARect.Left, ARect.Top, TScrollingWinControl(FMenu.Owner).ClientWidth+5{FForm.ClientWidth+5},
           ARect.Bottom);
      end
      else
        if FMenu.IsRightToLeft then
          ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right+7, ARect.Bottom);
    end;
  //--
  end
  else
  begin
    if (Is16Bit and FGradient) then
    begin
      inc(ARect.Right,2);  //needed for RightToLeft
      DrawGradient(ACanvas, ARect, FMenu.IsRightToLeft);
      Dec(ARect.Right,2);

    end
    else
    begin
      ACanvas.brush.color := FFColor;
      ACanvas.FillRect(FillRect); // +jt
      ACanvas.brush.color := FFIconBackColor;
      ACanvas.FillRect(IconRect);
    end;


  //------------
  end;


  if FMenuItem.Enabled then
    ACanvas.Font.Color := FFont.Color
  else
    ACanvas.Font.Color := FDisabledColor;

  DrawTopMenuBorder := false;
  if Selected and FDrawSelect then
  begin
    ACanvas.brush.Style := bsSolid;
    if FTopMenu then
    begin
      DrawTopMenuItem(FMenuItem, ACanvas, ARect, FMenuBarColor, FMenu.IsRightToLeft);
    end
    else
    if FMenuItem.Enabled then
    begin
      Inc(ARect.Top, 1);
      Dec(ARect.Bottom, 1);
      if FFlatMenu then
        Dec(ARect.Right, 1);
      ACanvas.brush.color := FFSelectColor;
      ACanvas.FillRect(ARect);
      ACanvas.Pen.color := FFSelectBorderColor;
      ACanvas.Brush.Style := bsClear;
      ACanvas.RoundRect(Arect.Left, Arect.top, Arect.Right, Arect.Bottom, 0, 0);
      Dec(ARect.Top, 1);
      Inc(ARect.Bottom, 1);
      if FFlatMenu then
        Inc(ARect.Right, 1);
    end;
    DrawTopMenuBorder := true;
  end

  // Draw the menubar in XP Style when hovering over an main menu item
  else
  begin
    //if FMenuItem.Enabled and FTopMenu and IsMouseInRect( TScrollingWinControl(FMenu.Owner), ARect) then
    if FMenuItem.Enabled and FTopMenu and IsWNT and
       IsMouseInRect( TScrollingWinControl(FMenu.Owner), origrect) then // +jt
    begin
      ACanvas.brush.Style := bsSolid;
      ACanvas.brush.color := FFSelectColor;
      DrawTopMenuBorder := true;
      ACanvas.Pen.color := FFSelectBorderColor;
      ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right - 7, ARect.Bottom);
    end;
  end;


  if (FMenuItem.Checked) or (FMenuItem.RadioItem ) then  //x
    DrawCheckedItem(FMenuItem, Selected, FMenuItem.Enabled, HasImgLstBitmap or HasBitmap,
                    ACanvas, CheckedRect);

  if (B <> nil) and (B.Width > 0) then  // X
    DrawIcon(FMenuItem, ACanvas, B, IconRect,
      Selected or DrawTopMenuBorder, False, FMenuItem.Enabled, FMenuItem.Checked,
      FTopMenu, FMenu.IsRightToLeft);



  if not IsLine then
  begin

    if FMenu.IsRightToLeft then
    begin
      TextFormat := DT_RIGHT + DT_RTLREADING;
      Dec(TextRect.Right, 3);
    end
    else
    begin
      TextFormat := 0;
      Inc(TextRect.Left, 3);
    end;
    TextRect.Top := TextRect.Top +
        ((TextRect.Bottom - TextRect.Top) - ACanvas.TextHeight('W')) div 2;
    DrawTheText(FMenuItem, txt, ShortCutToText(FMenuItem.ShortCut),
      ACanvas, TextRect,
      Selected, FMenuItem.Enabled, FMenuItem.Default,
      FTopMenu, FMenu.IsRightToLeft, FFont, TextFormat);

  end
  else
  begin
    if FMenu.IsRightToLeft then
    begin
      X1 := TextRect.Left;
      X2 := TextRect.Right - 7;
    end
    else
    begin
      X1 := TextRect.Left + 7;
      X2 := TextRect.Right;
    end;

    ACanvas.Pen.Color := FFSeparatorColor;
    ACanvas.MoveTo(X1,
      TextRect.Top +
      Round((TextRect.Bottom - TextRect.Top) / 2));
    ACanvas.LineTo(X2,
      TextRect.Top +
      Round((TextRect.Bottom - TextRect.Top) / 2))
  end;

  // +jt
    BitBlt(origcanvas.Handle,origrect.Left,origrect.Top,buff.Width,buff.Height,ACanvas.Handle,0,0,SRCCOPY);
  finally
   B.free;
   buff.free;
   ACanvas := OrigCanvas;
   ARect:=origrect;
  end;
// +jt

  if not (csDesigning in ComponentState) then
  begin
    if (FFlatMenu) and (not FTopMenu) then
    begin
      hDcM := ACanvas.Handle;
      hWndM := WindowFromDC(hDcM);
// +jt
      if (hWndM=0) and (Application.Handle<>0) then
     begin
       if not PeekMessage(msg,Application.Handle,WM_DRAWMENUBORDER,WM_DRAWMENUBORDER2,PM_NOREMOVE) then
         PostMessage(Application.Handle,WM_DRAWMENUBORDER,0,Integer(FMenuItem));
     end
     else
     if hWndM <> FForm.Handle then
     begin
       if not PeekMessage(msg,Application.Handle,WM_DRAWMENUBORDER,WM_DRAWMENUBORDER2,PM_NOREMOVE) then
         PostMessage(Application.Handle,WM_DRAWMENUBORDER2,integer(FMenu.IsRightToLeft),Integer(hWndM));
     end;
   end;
  end;

//-----

end;

{$IFDEF VER5U}
procedure TXPMenu.ToolBarDrawButton(Sender: TToolBar;
  Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);

var
  ACanvas: TCanvas;

  ARect, HoldRect: TRect;
  B: TBitmap;
  HasBitmap: boolean;
  {Sylvain ...}
  HasHotBitMap : Boolean;
  HasDisBitMap : Boolean;
  ImglstHand   : THandle;
  CanDraw      : Boolean;
  {... Sylvain}
  BitmapWidth: integer;
  TextFormat: integer;
  XButton: TToolButton;
  HasBorder: boolean;
  HasBkg: boolean;
  IsTransparent: boolean;
  FBSelectColor: TColor;

  procedure DrawBorder;
  var
    BRect, WRect: TRect;
    procedure DrawRect;
    begin
      ACanvas.Pen.color := FFSelectBorderColor;
      ACanvas.MoveTo(WRect.Left, WRect.Top);
      ACanvas.LineTo(WRect.Right, WRect.Top);
      ACanvas.LineTo(WRect.Right, WRect.Bottom);
      ACanvas.LineTo(WRect.Left, WRect.Bottom);
      ACanvas.LineTo(WRect.Left, WRect.Top);
    end;

  begin
    BRect := HoldRect;
    Dec(BRect.Bottom, 1);
    Inc(BRect.Top, 1);
    Dec(BRect.Right, 1);

    WRect := BRect;
    if Button.Style = tbsDropDown then
    begin
      Dec(WRect.Right, 13);
      DrawRect;

      WRect := BRect;
      Inc(WRect.Left, WRect.Right - WRect.Left - 13);
      DrawRect;
    end
    else
    begin

      DrawRect;
    end;
  end;

begin

  B := nil;

  {Added By Sylvain ...}
  HasHotBitmap := (Sender.HotImages <> nil) and
                  (Button.ImageIndex <> -1) and
                  (Button.ImageIndex <= Sender.HotImages.Count - 1);


  HasDisBitmap := (Sender.DisabledImages <> nil) and
                  (Button.ImageIndex <> -1) and
                  (Button.ImageIndex <= Sender.DisabledImages.Count - 1);
  {...Sylvain}

  HasBitmap := (Sender.Images <> nil) and
    (Button.ImageIndex <> -1) and
    (Button.ImageIndex <= Sender.Images.Count - 1);


  IsTransparent := Sender.Transparent;

  ACanvas := Sender.Canvas;

  //SetGlobalColor(ACanvas);
  if(FColorsChanged) then SetGlobalColor(ACanvas); // +jt

  if (Is16Bit) and (not UseSystemColors) then
    FBSelectColor := NewColor(ACanvas, FSelectColor, 68)
  else
    FBSelectColor := FFSelectColor;


  HoldRect := Button.BoundsRect;

  ARect := HoldRect;

  if Is16Bit then
    ACanvas.brush.color := NewColor(ACanvas, Sender.Color, 16)
  else
    ACanvas.brush.color := Sender.Color;

  if not IsTransparent then
    ACanvas.FillRect(ARect);

  HasBorder := false;
  HasBkg := false;

  if (cdsHot in State) then
  begin
    if (cdsChecked in State) or (Button.Down) or (cdsSelected in State) then
      ACanvas.Brush.Color := FCheckedAreaSelectColor
    else
      ACanvas.brush.color := FBSelectColor;
    HasBorder := true;
    HasBkg := true;
  end;

  if ((cdsChecked in State) and not (cdsHot in State)) then
  begin
    ACanvas.Brush.Color := FCheckedAreaColor;
    HasBorder := true;
    HasBkg := true;
  end;

  if (cdsIndeterminate in State) and not (cdsHot in State) then
  begin
    ACanvas.Brush.Color := FBSelectColor;
    HasBkg := true;
  end;


  if (Button.MenuItem <> nil) and (State = []) then
  begin
    ACanvas.brush.color := Sender.Color;
    if not IsTransparent then
      HasBkg := true;
  end;


  Inc(ARect.Top, 1);



  if HasBkg then
    ACanvas.FillRect(ARect);

  if HasBorder then
    DrawBorder;


  if ((Button.MenuItem <> nil) or (Button.DropdownMenu <> nil))
    and (cdsSelected in State) then
  begin
    DrawTopMenuItem(Button, ACanvas, ARect, Sender.Color ,false);
    DefaultDraw := false;
  end;

  ARect := HoldRect;
  DefaultDraw := false;


  if Button.Style = tbsDropDown then
  begin
    ACanvas.Pen.Color := clBlack;
    DrawArrow(ACanvas, (ARect.Right - 14) + ((14 - 5) div 2),
      ARect.Top + ((ARect.Bottom - ARect.Top - 3) div 2) + 1);
  end;

  BitmapWidth := 0;
{ Rem by Sylvain ...
  if HasBitmap then
  begin
... Sylvain}
    try
      B := TBitmap.Create;
      CanDraw := False;
      ImglstHand:=0;
      if (cdsHot in State) AND HasHotBitmap then
      begin
        B.Width := Sender.HotImages.Width;
        B.Height := Sender.HotImages.Height;
        ImglstHand := Sender.HotImages.Handle;
        CanDraw := True;
      end
      else if (cdsDisabled in State) and HasDisBitmap then
      begin
        B.Width := Sender.DisabledImages.Width;
        B.Height := Sender.DisabledImages.Height;
        ImglstHand := Sender.DisabledImages.Handle;
        CanDraw := True;
      end
      else if HasBitMap then
      begin
        B.Width := Sender.Images.Width;
        B.Height := Sender.Images.Height;
        ImglstHand := Sender.Images.Handle;
        CanDraw := True;
      end;
      if CanDraw then
      begin {CanDraw}
       // B.Canvas.Brush.Color := TransparentColor; // ACanvas.Brush.Color; // +jt
        B.Canvas.Brush.Color := B.Canvas.Pixels[0, B.Height - 1];//"Todd Asher" <ashert@yadasystems.com>
        B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
        ImageList_DrawEx(ImglstHand, Button.ImageIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

        BitmapWidth := b.Width;

        if Button.Style = tbsDropDown then
          Dec(ARect.Right, 12);


        if TToolBar(Button.Parent).List then
        begin

          if Button.BiDiMode = bdRightToLeft then
          begin
            Dec(ARect.Right, 3);
            ARect.Left := ARect.Right - BitmapWidth;

          end
          else
          begin
            Inc(ARect.Left, 3);
            ARect.Right := ARect.Left + BitmapWidth
          end


        end
        else
          ARect.Left := Round(ARect.Left + (ARect.Right - ARect.Left - B.Width)/2);

        inc(ARect.Top, 2);
        ARect.Bottom := ARect.Top + B.Height + 6;

        DrawIcon(Button, ACanvas, B, ARect, (cdsHot in State),
         (cdsSelected in State), Button.Enabled, (cdsChecked in State), false,
         false);

      end; {CanDraw}
    finally
      B.Free;
    end;
    ARect := HoldRect;
    DefaultDraw := false;
{rem by sylvain ...
  end;
...Sylvain}
//-----------

  if Sender.ShowCaptions then
  begin

    if Button.Style = tbsDropDown then
      Dec(ARect.Right, 12);


    if not TToolBar(Button.Parent).List then
    begin
      TextFormat := DT_Center;

      ARect.Top := ARect.Bottom - ACanvas.TextHeight(Button.Caption) - 6;
    end
    else
    begin
      TextFormat := DT_VCENTER;
      if Button.BiDiMode = bdRightToLeft then
      begin
        TextFormat := TextFormat + DT_Right;
        Dec(ARect.Right, BitmapWidth + 7);
      end
      else
      begin
        if BitmapWidth > 0 then //"Dan Downs" <dan@laserformsinc.com>
         if Sender.List then     //Michaël Moreno <michael@weatherderivs.com>
           Inc(ARect.Left, BitmapWidth + 6)
          else
           Inc(ARect.Left, BitmapWidth);
      end

    end;

    if (Button.MenuItem <> nil) then
    begin
      TextFormat := DT_Center;
      //Inc(ARect.Left, 1);
    end;

    if Button.BiDiMode = bdRightToLeft then
      TextFormat := TextFormat + DT_RTLREADING;

    if Button.Down and not Button.Enabled then //"felix" <felix@unidreamtech.com>  23/5
      InflateRect(ARect, -1, -1);

    {alexs alexs75@hotbox.ru}
    ARect.Top := ARect.Top + ((ARect.Bottom - ARect.Top) - ACanvas.TextHeight('W')) div 2;

    DrawTheText(Button, Button.Caption, '',
      ACanvas, ARect,
      (cdsSelected in State), Button.Enabled, false,
      (Button.MenuItem <> nil),
      (Button.BidiMode = bdRightToLeft), FFont, TextFormat);

    ARect := HoldRect;
    DefaultDraw := false;
  end;


  if Button.Index > 0 then
  begin
    XButton := {TToolBar(Button.Parent)}Sender.Buttons[Button.Index - 1];
    if (XButton.Style = tbsDivider) or (XButton.Style = tbsSeparator) then
    begin
      ARect := XButton.BoundsRect;
      if Is16Bit then
        ACanvas.brush.color := NewColor(ACanvas, Sender.Color, 16)
      else
        ACanvas.brush.color := Sender.Color;

      if not IsTransparent then
        ACanvas.FillRect(ARect);
     // if (XButton.Style = tbsDivider) then  // Can't get it.
      if XButton.Tag > 0 then
      begin
        Inc(ARect.Top, 2);
        Dec(ARect.Bottom, 1);

        ACanvas.Pen.color := GetShadeColor(ACanvas,Sender.Color,30);
        ARect.Left := ARect.Left + (ARect.Right - ARect.Left) div 2;
        ACanvas.MoveTo(ARect.Left, ARect.Top);
        ACanvas.LineTo(ARect.Left, ARect.Bottom);

      end;
      ARect := Button.BoundsRect;
      DefaultDraw := false;
    end;

  end;

  {if Button.MenuItem <> nil then
    if (xcMainMenu in XPControls) then
      ActivateMenuItem(Button.MenuItem);}
end;
{$ENDIF}

// Controlbar Paint. Added by Michiel van Oudheusden (27 sep 2001)
// Paints the bands of a controlbar like the office XP style
procedure TXPMenu.ControlBarPaint(Sender: TObject; Control: TControl;
  Canvas: TCanvas; var ARect: TRect; var Options: TBandPaintOptions);
var
  i: Integer;
  intInc: integer;
begin

  if(FColorsChanged) then SetGlobalColor(Canvas); // +jt
  // No frame and grabber drawing. We do it ourselfes
  Options := [];

  // First background

  if Is16Bit then
    Canvas.brush.color := NewColor(Canvas, TControlBar(Sender).Color , 6)
  else
    Canvas.brush.color := TControlBar(Sender).Color;

  Canvas.FillRect(ARect);

  intInc := 30;
  for i := (ARect.Top + 5) to (ARect.Bottom - 5)do
  begin
    Canvas.Pen.Color := GetShadeColor(Canvas, TControlBar(Sender).Color, intInc);
    if i mod 2 = 0 then
    begin
      Canvas.MoveTo(ARect.Left + 3, i);
      Canvas.LineTo(ARect.Left + 6, i);
      Inc(intInc, 7);
    end;
  end;

end;

procedure TXPMenu.SetGlobalColor(ACanvas: TCanvas);
begin
//-----
  FColorsChanged:=false; // +jt

  if GetDeviceCaps(ACanvas.Handle, BITSPIXEL) < 16 then
    Is16Bit := false
  else
    Is16Bit := true;


  FDimParentColor := 16;
  FDimParentColorSelect := 40;

  FFColor := FColor;
  FFIconBackColor := FIconBackColor;

  if Is16Bit then
  begin
    if FUseDimColor then
    begin
      FFSelectColor := NewColor(ACanvas, FSelectColor, 68);
      FCheckedAreaColor := NewColor(ACanvas, FSelectColor, 80);
      FCheckedAreaSelectColor := NewColor(ACanvas, FSelectColor, 50);
    end
    else
    begin
      FFSelectColor := FSelectColor;
      FCheckedAreaColor := FSelectColor;
      FCheckedAreaSelectColor := FSelectColor;
    end;

    FMenuBorderColor := GetShadeColor(ACanvas, clBtnFace, 90);
    FMenuShadowColor := GetShadeColor(ACanvas, clBtnFace, 76);
  end
  else
  begin
    FFSelectColor := FSelectColor;
    FCheckedAreaColor := clWhite;
    FCheckedAreaSelectColor := clSilver;
    FMenuBorderColor := clBtnShadow;
    FMenuShadowColor := clBtnShadow;
  end;

  FFSelectBorderColor := FSelectBorderColor;
  FFSelectFontColor := FSelectFontColor;
  FFMenuBarColor := FMenuBarColor;
  FFDisabledColor := FDisabledColor;
  FFCheckedColor := FCheckedColor;
  FFSeparatorColor := FSeparatorColor;



  if FUseSystemColors then
  begin
//    GetSystemMenuFont(FFont);
    FFSelectFontColor := FFont.Color;
    if not Is16Bit then
    begin
      FFColor := clWhite;
      FFIconBackColor := clBtnFace;
      FFSelectColor := clWhite;
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := FFIconBackColor;
      FFDisabledColor := clBtnShadow;
      FFCheckedColor := clHighlight;
      FFSeparatorColor := clBtnShadow;
      FCheckedAreaColor := clWhite;
      FCheckedAreaSelectColor := clWhite;

    end
    else
    begin
      FFColor := NewColor(ACanvas, clBtnFace, 86);
      FFIconBackColor := NewColor(ACanvas, clBtnFace, 16);
      FFSelectColor := NewColor(ACanvas, clHighlight, 68);
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := clBtnFace;

      FFDisabledColor := NewColor(ACanvas, clBtnShadow, 10);
      FFSeparatorColor := NewColor(ACanvas, clBtnShadow, 25);
      FFCheckedColor := clHighlight;
      FCheckedAreaColor := NewColor(ACanvas, clHighlight, 80);
      FCheckedAreaSelectColor := NewColor(ACanvas, clHighlight, 50);

    end;
  end;

end;

procedure TXPMenu.DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; BckColor:Tcolor; IsRightToLeft: boolean);
var
  X1, X2: integer;
  DefColor, HoldColor: TColor;
begin
  X1 := ARect.Left;
  X2 := ARect.Right;


  ACanvas.brush.Style := bsSolid;
  ACanvas.brush.color :=  FFSelectColor;

  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := FFSelectBorderColor;

  if (not IsRightToLeft) and (Is16Bit) and (Sender is TMenuItem) then
  begin
    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Bottom);
//    ACanvas.LineTo(X1, ARect.Bottom);

    DefColor := FFMenuBarColor;


    HoldColor := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;

    ACanvas.FillRect(Rect(X2 - 7, ARect.Top, X2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 3, X2 - 2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 40 + 20);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 5, X2 - 3, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 60 + 40);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 6, X2 - 5, ARect.Bottom));

    //---

    ACanvas.Pen.Color := DefColor;
    ACanvas.MoveTo(X2 - 5, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 6);

    ACanvas.MoveTo(X2 - 3, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);



    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 3);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.MoveTo(X2 - 5, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 9);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 40);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 5);
    ACanvas.LineTo(X2 - 6, ARect.Top + 7);

  end
  else
  begin

    ACanvas.Pen.Color := FFSelectBorderColor;
    ACanvas.Brush.Color := GetShadeColor(ACanvas, BckColor, 70);

    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Bottom);


    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.FillRect(Rect(X2 - 2, ARect.Top + 2, X2, ARect.Bottom));

    ACanvas.Brush.Color := BckColor;
    ACanvas.FillRect(Rect(X2 - 2, ARect.Top , X2, ARect.Top + 2));


  end;

end;


procedure TXPMenu.DrawCheckedItem(FMenuItem: TMenuItem; Selected, Enabled,
 HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
var
  X1, X2: integer;
begin
  if FMenuItem.RadioItem then
  begin
    if FMenuItem.Checked then
    begin
      if Enabled then
      begin
        ACanvas.Pen.color := FFSelectBorderColor;
        if selected then
          ACanvas.Brush.Color := FCheckedAreaSelectColor
        else
          ACanvas.Brush.Color := FCheckedAreaColor;
      end
      else
        ACanvas.Pen.color := FFDisabledColor;

      ACanvas.Brush.Style := bsSolid;
      if HasImgLstBitmap then
      begin
        ACanvas.RoundRect(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom,
          6, 6);
      end
      else
      begin
        ACanvas.Ellipse(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
// +jt
        InflateRect(CheckedRect, -2, -2);
        ACanvas.Brush.color := ACanvas.Pen.Color;
       ACanvas.Ellipse(CheckedRect.Left, CheckedRect.Top,
         CheckedRect.Right, CheckedRect.Bottom);
// +jt
      end;
    end;
  end
  else
  begin
    if (FMenuItem.Checked) then
      if (not HasImgLstBitmap) then
      begin
        if Enabled then
        begin
          ACanvas.Pen.color := FFCheckedColor;
          if selected then
            ACanvas.Brush.Color := FCheckedAreaSelectColor
          else
            ACanvas.Brush.Color := FCheckedAreaColor; ;
        end
        else
          ACanvas.Pen.color := FFDisabledColor;

        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
        if Enabled then
          ACanvas.Pen.color := clBlack
        else
          ACanvas.Pen.color := FFDisabledColor;
        x1 := CheckedRect.Left + 1;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.LineTo(x1, x2);
        //--
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);
        //--
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 4;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 5;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);
        //-----------------

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right + 2;
        x2 := CheckedRect.Top - 1;
        ACanvas.LineTo(x1, x2);
        //--
        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right - 2;
        x2 := CheckedRect.Top + 3;
        ACanvas.LineTo(x1, x2);

      end
      else
      begin


        if Enabled then
        begin
          ACanvas.Pen.color := FFSelectBorderColor;
          if selected then
            ACanvas.Brush.Color := FCheckedAreaSelectColor
          else
            ACanvas.Brush.Color := FCheckedAreaColor; ;
        end
        else
          ACanvas.Pen.color := FFDisabledColor;

        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
      end;
  end;

end;

procedure TXPMenu.DrawTheText(Sender: TObject; txt, ShortCuttext: string;
    ACanvas: TCanvas; TextRect: TRect;
    Selected, Enabled, Default, TopMenu, IsRightToLeft: boolean;
    var TxtFont: TFont; TextFormat: integer);
var
  DefColor: TColor;
  B: TBitmap;
  BRect: TRect;
begin
  TextFormat := TextFormat + DT_EXPANDTABS;
  DefColor := TxtFont.Color;

  ACanvas.Font.Assign (TxtFont);

  if Selected then
    DefColor := FFSelectFontColor;

  If not Enabled then
  begin
    DefColor := FFDisabledColor;

    if (Sender is TToolButton) then
    begin
      TextRect.Top := TextRect.Top +
        ((TextRect.Bottom - TextRect.Top) - ACanvas.TextHeight('W')) div 2;
      B := TBitmap.Create;
      try
        B.Width := TextRect.Right - TextRect.Left;
        B.Height := TextRect.Bottom - TextRect.Top;
        BRect := Rect(0,0,B.Width, B.Height);


        B.Canvas.Brush.Color := ACanvas.Brush.Color;
        B.Canvas.FillRect (BRect);
  //      B.Canvas.Font := FFont; //felix added for resolving font problems in Win98
                                //27.08
        B.Canvas.Font.color := DefColor;

        DrawtextEx(B.Canvas.Handle,
          PChar(txt),
          Length(txt),
          BRect, TextFormat + DT_VCENTER, nil);
        ACanvas.CopyRect(TextRect, B.Canvas, BRect);
      finally
        B.Free;
      end;
      exit;
    end;

  end;

  if (TopMenu and Selected) then
    if FUseSystemColors then
      DefColor := TopMenuFontColor(ACanvas, FFIconBackColor);

  ACanvas.Font.color := DefColor;    // will not affect Buttons

  SetBkMode(ACanvas.Handle, TRANSPARENT);


  if Default and Enabled then
  begin

    Inc(TextRect.Left, 1);
    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);
    Dec(TextRect.Left, 1);


    Inc(TextRect.Top, 2);
    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);


    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


    Dec(TextRect.Top, 1);
    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 40);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
                              ACanvas.Pixels[TextRect.Left, TextRect.Top], 60);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);

    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);
    Dec(TextRect.Top, 1);

    ACanvas.Font.color := DefColor;
  end;

  DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);


  txt := ShortCutText + ' ';
  {
  if not Is16Bit then
    ACanvas.Font.color := DefColor
  else
    ACanvas.Font.color := GetShadeColor(ACanvas, DefColor, -40);
  }


  if IsRightToLeft then
  begin
    Inc(TextRect.Left, 10);
    TextFormat := DT_LEFT
  end
  else
  begin
    Dec(TextRect.Right, 10);
    TextFormat := DT_RIGHT;
  end;


  DrawtextEx(ACanvas.Handle,
    PChar(txt),
    Length(txt),
    TextRect, TextFormat, nil);

end;

procedure TXPMenu.DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
 IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
 IsRightToLeft: boolean);
var
  DefColor: TColor;
  X, Y: integer;
begin

  if (B <> nil) and (B.Width > 0) then
  begin
    X := IconRect.Left;
    Y := IconRect.Top + 1;

    if (Sender is TMenuItem) then
    begin
      inc(Y, 2);
      if FIconWidth > B.Width then
        X := X + ((FIconWidth - B.Width) div 2) - 1
      else
      begin
        if IsRightToLeft then
          X := IconRect.Right - b.Width - 2
        else
          X := IconRect.Left + 2;
      end;
    end;

    if FTopMenu then
    begin
      if IsRightToLeft then
        X := IconRect.Right - b.Width - 5
      else
        X := IconRect.Left + 1;
    end;

    if (Hot) and (FTopMenu) and (Enabled) then
      if not Selected then
      begin
        dec(X, 1);
        dec(Y, 2);
      end;

    if (Hot) and (not FTopMenu) and (Enabled) and (not Checked) then
      if not Selected then
      begin
        dec(X, 1);
        dec(Y, 1);
      end;

    if (not Hot) and (Enabled) and (not Checked) then
      if Is16Bit then
        DimBitmap(B, FDimLevel{30});


    if not Enabled then
    begin
      GrayBitmap(B, FGrayLevel );
      DimBitmap(B, 40);
    end;

    if (Hot) and (Enabled) and (not Checked) then
    begin
      if (Is16Bit) and (not UseSystemColors) and (Sender is TToolButton) then
        DefColor := NewColor(ACanvas, FSelectColor, 68)
      else
        DefColor := FFSelectColor;

      DefColor := GetShadeColor(ACanvas, DefColor, 50);
      DrawBitmapShadow(B, ACanvas, X + 2, Y + 2, DefColor);
    end;

    B.Transparent := true;
    ACanvas.Draw(X, Y, B);
  end;

end;


function TXPMenu.TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
var
  r, g, b, avg: integer;
begin

  Color := ColorToRGB(Color);
  r := Color and $000000FF;
  g := (Color and $0000FF00) shr 8;
  b := (Color and $00FF0000) shr 16;

  Avg := (r + b) div 2;

  if (Avg > 150) or (g > 200) then
    Result := FFont.Color
  else
    Result := NewColor(ACanvas, Color, 90);

end;

procedure TXPMenu.SetDisableSubclassing(const Value: boolean);
begin
  if Value = FDisableSubclassing then Exit;
  if XPMenuManager.ActiveXPMenu = Self then
    XPMenuManager.UpdateActiveXPMenu(Self)
  else
    if (XPMenuManager.ActiveXPMenu = nil) and not(FDisableSubclassing) then
      XPMenuManager.UpdateActiveXPMenu(nil);
end;


procedure TXPMenu.SetActive(const Value: boolean);
begin
  if Value = FActive then exit;

  FActive := Value;
  if FActive then
    InitItems(FForm, true, true)
  else
    InitItems(FForm, false, true);

  if FForm.Handle <> 0 then
    Windows.DrawMenuBar(FForm.Handle);
end;

procedure TXPMenu.SetAutoDetect(const Value: boolean);
begin
  FAutoDetect := Value;
end;

procedure TXPMenu.SetForm(const Value: TScrollingWinControl);
var
  Hold: boolean;
begin
  if Value <> FForm then
  begin
    Hold := Active;
    Active := false;
    FForm := Value;
    if Hold then
      Active := True;
  end;
end;

procedure TXPMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Windows.DrawMenuBar(FForm.Handle);

end;

procedure TXPMenu.SetColor(const Value: TColor);
begin
  FColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetIconBackColor(const Value: TColor);
begin
  FIconBackColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetMenuBarColor(const Value: TColor);
begin
  FMenuBarColor := Value;
  FColorsChanged := true; // +jt
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure TXPMenu.SetCheckedColor(const Value: TColor);
begin
  FCheckedColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetSeparatorColor(const Value: TColor);
begin
  FSeparatorColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetSelectBorderColor(const Value: TColor);
begin
  FSelectBorderColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetSelectColor(const Value: TColor);
begin
  FSelectColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetSelectFontColor(const Value: TColor);
begin
  FSelectFontColor := Value;
  FColorsChanged := true; // +jt
end;

procedure TXPMenu.SetIconWidth(const Value: integer);
begin
  FIconWidth := Value;
end;

procedure TXPMenu.SetDrawSelect(const Value: boolean);
begin
  FDrawSelect := Value;
end;



procedure TXPMenu.SetOverrideOwnerDraw(const Value: boolean);
begin
  FOverrideOwnerDraw := Value;
  if FActive then
    Active := True;
end;


procedure TXPMenu.SetUseSystemColors(const Value: boolean);
begin
  FUseSystemColors := Value;
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure TXPMenu.SetGradient(const Value: boolean);
begin
  FGradient := Value;
end;

procedure TXPMenu.SetFlatMenu(const Value: boolean);
begin
  FFlatMenu := Value;
end;

procedure TXPMenu.SetXPContainers(const Value: TXPContainers);
begin
  if Value <> FXPContainers then
  begin
    if FActive then
    begin
      FActive := false;
      InitItems(FForm, false, true);
      FActive := true;
      FXPContainers := Value;
      InitItems(FForm, true, true);
    end;
  end;
  FXPContainers := Value;

end;

procedure TXPMenu.SetXPControls(const Value: TXPControls);
begin
  if Value <> FXPControls then
  begin
    if FActive then
    begin
      FActive := false;
      InitItems(FForm, false, true);
      FActive := true;
      FXPControls := Value;
      InitItems(FForm, true, true);
    end;
  end;
  FXPControls := Value;

end;

procedure TXPMenu.SetDrawMenuBar(const Value: boolean);
begin
  FDrawMenuBar := Value;
end;

procedure TXPMenu.SetUseDimColor(const Value: boolean);
begin
  FUseDimColor := Value;
end;

procedure GetSystemMenuFont(Font: TFont);
var
  FNonCLientMetrics: TNonCLientMetrics;
begin
  FNonCLientMetrics.cbSize := Sizeof(TNonCLientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @FNonCLientMetrics,0) then
  begin
    Font.Handle := CreateFontIndirect(FNonCLientMetrics.lfMenuFont);
    Font.Color := clMenuText;
  end;
end;

procedure TXPMenu.DrawGradient(ACanvas: TCanvas; ARect: TRect;
 IsRightToLeft: boolean);
var
  i: integer;
  v: integer;
  FRect: TRect;
begin

  fRect := ARect;
  V := 0;
  if IsRightToLeft then
  begin
    fRect.Left := fRect.Right - 1;
    for i := ARect.Right Downto ARect.Left do
    begin
      if (fRect.Left < ARect.Right)
        and (fRect.Left > ARect.Right - FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      ACanvas.Brush.Color := NewColor(ACanvas, FFIconBackColor, v);
      ACanvas.FillRect(fRect);

      fRect.Left := fRect.Left - 1;
      fRect.Right := fRect.Left - 1;
    end;
  end
  else
  begin
    fRect.Right := fRect.Left + 1;
    for i := ARect.Left to ARect.Right do
    begin
      if (fRect.Left > ARect.Left)
        and (fRect.Left < ARect.Left + FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      ACanvas.Brush.Color := NewColor(ACanvas, FFIconBackColor, v);
      ACanvas.FillRect(fRect);

      fRect.Left := fRect.Left + 1;
      fRect.Right := fRect.Left + 1;
    end;
  end;
end;

function MenuWindowProc(hwnd: HWND; uMsg: integer; WParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
 oldproc: integer;
 r: TRect;
 pt: TPoint;
begin
 if(uMsg=WM_ERASEBKGND) then
 begin
   Result:=1;
   exit;
end;
 if((uMsg=WM_SHOWWINDOW) and (not Boolean(WParam))) or (uMsg=WM_CLOSE) or (uMsg=WM_DESTROY) then
 begin
   SetWindowRgn(hwnd, 0,false);
   oldproc:=GetWindowLong(hWnd,GWL_USERDATA);
   SetWindowLong(hWnd,GWL_WNDPROC,oldproc);
   SetWindowLong(hWnd,GWL_USERDATA,0);
   Result:=CallWindowProc(Pointer(oldproc), hwnd, uMsg, wParam, lParam);
    GetWindowRect(hWnd, r);
   pt.x:=r.Right+2;
   pt.y:=r.Top+2;
   hWnd :=WindowFromPoint(pt);
   if GetWindowLong(hWnd,GWL_WNDPROC)<>integer(@MenuWindowProc) then
   begin
     pt.x:=r.Right+2;
     pt.y:=r.Bottom-2;
     hWnd :=WindowFromPoint(pt);
     if GetWindowLong(hWnd,GWL_WNDPROC)<>integer(@MenuWindowProc) then exit;
   end;
   InvalidateRect(hwnd,nil,false);
 end
 else Result:=CallWindowProc(Pointer(GetWindowLong(hWnd,GWL_USERDATA)), hwnd, uMsg, wParam, lParam);
end;

procedure TXPMenu.DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);
var
  WRect: TRect;
  dCanvas: TCanvas;
  wDC: HDC; // +jt

 regiontype: integer; // +jt
 r1,r2,wr,region: HRGN; // +jt
 rgnr: TRect; // +jt
begin


  if (hWnd <= 0) or (FSettingWindowRng) then
 begin
  exit;
 end;
// +jt
 wDC := GetWindowDC(hWnd); //GetDesktopWindow
 if wDC=0 then exit;
// +jt
  FSettingWindowRng :=true; // +jt
 dCanvas := TCanvas.Create;
 try
   dCanvas.Handle := wDC; // +jt
    GetWindowRect(hWnd, WRect);
  // +jt
   WRect.Right := WRect.Right-WRect.Left;
   WRect.Bottom := WRect.Bottom-WRect.Top;
   WRect.Top:=0;
   WRect.Left:=0;
    if GetWindowLong(hWnd,GWL_WNDPROC)<>integer(@MenuWindowProc) then
   begin
     SetWindowLong(hWnd,GWL_USERDATA,GetWindowLong(hWnd,GWL_WNDPROC));
     SetWindowLong(hWnd,GWL_WNDPROC,integer(@MenuWindowProc));
   end;
    if not IsWXP then
   begin
      wr:= CreateRectRgn(0,0,0,0);
     regiontype := GetWindowRgn(hWnd, wr);
     GetRgnBox(wr,rgnr);
     DeleteObject(wr);
      if (regionType = ERROR) or (abs(rgnr.Right-WRect.Right)>5) or (abs(rgnr.Bottom-WRect.Bottom)>5) then
     begin
       region:= CreateRectRgn(0,0,0,0);
       r1:=CreateRectRgn(WRect.Left,WRect.Top,WRect.Right-2,WRect.Bottom-2);
       r2:=CreateRectRgn(WRect.Left+2,WRect.Top+2,WRect.Right,WRect.Bottom);
       CombineRgn(region,r1,r2,RGN_OR);
       DeleteObject(r1);
       DeleteObject(r2);
        SetWindowRgn(hWnd,region,true);
      end;
 // +jt
      Dec(WRect.Right, 2);
     Dec(WRect.Bottom, 2);
   end; // +jt
    dCanvas.Brush.Style := bsClear;
    dCanvas.Pen.Color := FMenuBorderColor;
   dCanvas.Rectangle(WRect.Left, WRect.Top, WRect.Right, WRect.Bottom);
    if IsRightToLeft then
   begin
      dCanvas.Pen.Color := FFIconBackColor;
     dCanvas.MoveTo(WRect.Right - 3, WRect.Top + 2);
     dCanvas.LineTo(WRect.Right - 2, WRect.Bottom - 1);
    end
   else
   begin
     dCanvas.Pen.Color := FFIconBackColor;
     dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 2,
                       WRect.Left + 3, WRect.Bottom - 1);
   end;
// +jt
       StretchBlt(dCanvas.Handle,WRect.Left + 1,WRect.Top + 1,WRect.Right - WRect.Left-1,2,
                  dCanvas.Handle,WRect.Left + 1,WRect.Top + 3,WRect.Right - WRect.Left-1,1,SRCCOPY);
        if IsWXP then
       begin
         StretchBlt(dCanvas.Handle,WRect.Left + 1,WRect.Bottom - 3,WRect.Right - WRect.Left-1,2,
                    dCanvas.Handle,WRect.Left + 1,WRect.Top + 3,WRect.Right - WRect.Left-1,1, SRCCOPY);
         dCanvas.Pen.Color := FFColor;
         dCanvas.Rectangle(WRect.Right - 3, WRect.Top+1, WRect.Right - 1,
                           WRect.Bottom-1);
       end;
// +jt
    Inc(WRect.Right, 2);
   Inc(WRect.Bottom, 2);
    if not IsWXP then // +jt
   begin // +jt
     dCanvas.Pen.Color := FMenuShadowColor;
     dCanvas.Rectangle(WRect.Left + 2, WRect.Bottom, WRect.Right, WRect.Bottom - 2);
     dCanvas.Rectangle(WRect.Right - 2, WRect.Bottom, WRect.Right, WRect.Top + 2);
   end; // +jt

 finally
   ReleaseDC(hWnd, wDC); // +jt
   dCanvas.Free;
 FSettingWindowRng :=false;
 end;

end;

procedure TXPMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if not Assigned(XPMenuManager) then Exit;
// Pass the notification information to the XPMenuManager
  if not(csDesigning in ComponentState) then
    XPMenuManager.Notification(AComponent, Operation);

  inherited Notification(AComponent, Operation);
  if not FActive then exit;
  if not FAutoDetect then exit;
  if (Operation = opInsert) and
     ((AComponent is TMenuItem) or (AComponent is TToolButton) or
      (AComponent is TControlBar)) then
  begin
    if not (csDesigning in ComponentState) then
      InitItem(AComponent, true, true);  // Tom: This will process this new component
  end;
end;


function GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b: integer;

begin
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  r := (r - value);
  if r < 0 then r := 0;
  if r > 255 then r := 255;

  g := (g - value) + 2;
  if g < 0 then g := 0;
  if g > 255 then g := 255;

  b := (b - value);
  if b < 0 then b := 0;
  if b > 255 then b := 255;

  //Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
  Result := RGB(r, g, b);
end;

function MergColor(Colors: Array of TColor): TColor;
var
  r, g, b, i: integer;
  clr: TColor;
begin
  r := 0; g:= 0; b:= 0;

  for i := 0 to High(Colors) do
  begin

    clr := ColorToRGB(Colors[i]);
    r := r + (Clr and $000000FF) div High(Colors);
    g := g + ((Clr and $0000FF00) shr 8) div High(Colors);
    b := b + ((Clr and $00FF0000) shr 16) div High(Colors);
  end;

  Result := RGB(r, g, b);

end;
function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b: integer;

begin
  if Value > 100 then Value := 100;
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;


  r := r + Round((255 - r) * (value / 100));
  g := g + Round((255 - g) * (value / 100));
  b := b + Round((255 - b) * (value / 100));

  //Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
  Result := RGB(r, g, b);

end;

function GetInverseColor(AColor: TColor): TColor;
begin
  Result := ColorToRGB(AColor) xor $FFFFFF;
end;

function GrayColor(ACanvas: TCanvas; Clr: TColor; Value: integer): TColor;
var
  r, g, b, avg: integer;

begin

  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  Avg := (r + g + b) div 3;
  Avg := Avg + Value;

  if Avg > 240 then Avg := 240;
  //if ACanvas <> nil then
  //  Result := Windows.GetNearestColor (ACanvas.Handle,RGB(Avg, avg, avg));
   Result := RGB(Avg, avg, avg);
end;

procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  LastColor1 := 0;
  LastColor2 := 0;

  for y := 0 to ABitmap.Height do
    for x := 0 to ABitmap.Width do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := GrayColor(ABitmap.Canvas , Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;
{Modified  by felix@unidreamtech.com}
{
procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
var
  Pixel: PRGBTriple;
  w, h: Integer;
  x, y: Integer;
  avg: integer;
begin
  ABitmap.PixelFormat := pf24Bit;
  w := ABitmap.Width;
  h := ABitmap.Height;
  for y := 0 to h - 1 do
  begin
    Pixel := ABitmap.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      avg := ((Pixel^.rgbtRed + Pixel^.rgbtGreen + Pixel^.rgbtBlue) div 3)
        + Value;
      if avg > 240 then avg := 240;
      Pixel^.rgbtRed := avg;
      Pixel^.rgbtGreen := avg;
      Pixel^.rgbtBlue := avg;
      Inc(Pixel);
    end;
  end;
end;
}

procedure DimBitmap(ABitmap: TBitmap; Value: integer);
var
 x, y: integer;
 LastColor1, LastColor2, Color: TColor;
begin
 if Value > 100 then Value := 100;
 LastColor1 := -1;
 LastColor2 := -1;
  for y := 0 to ABitmap.Height - 1 do
   for x := 0 to ABitmap.Width - 1 do
   begin
     Color := ABitmap.Canvas.Pixels[x, y];
     if Color = LastColor1 then
       ABitmap.Canvas.Pixels[x, y] := LastColor2
     else
     begin
       LastColor2 := NewColor(ABitmap.Canvas, Color, Value);
       ABitmap.Canvas.Pixels[x, y] := LastColor2;
       LastColor1 := Color;
     end;
   end;
end;

// LIne 2647
{Modified  by felix@unidreamtech.com}
{works  fine for 24 bit color
procedure DimBitmap(ABitmap: TBitmap; Value: integer);
var
  Pixel: PRGBTriple;
  w, h: Integer;
  x, y, c1, c2: Integer;
begin
  ABitmap.PixelFormat := pf24Bit;
  w := ABitmap.Width;
  h := ABitmap.Height;

  c1 := Value * 255;
  c2 := 100 - Value;
  for y := 0 to h - 1 do
  begin
    Pixel := ABitmap.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      Pixel^.rgbtRed := ((c2 * Pixel^.rgbtRed) + c1) div 100;
      Pixel^.rgbtGreen := ((c2 * Pixel^.rgbtGreen) + c1) div 100;
      Pixel^.rgbtBlue := ((c2 * Pixel^.rgbtBlue) + c1) div 100;
      Inc(Pixel);
    end;
  end;
end;
}
procedure DrawArrow(ACanvas: TCanvas; X, Y: integer);
begin
  ACanvas.MoveTo(X, Y);
  ACanvas.LineTo(X + 5, Y);

  ACanvas.MoveTo(X + 1, Y + 1);
  ACanvas.LineTo(X + 4, Y);

  ACanvas.MoveTo(X + 2, Y + 2);
  ACanvas.LineTo(X + 3, Y);

end;

procedure DrawArrow(ACanvas: TCanvas; X, Y, Orientation: integer);
begin
  case Orientation of
    0:
    begin

      ACanvas.MoveTo(X, Y);
      ACanvas.LineTo(X, Y-1);

      ACanvas.MoveTo(X + 1, Y + 1);
      ACanvas.LineTo(X + 4, Y + 4);

      ACanvas.MoveTo(X, Y + 1);
      ACanvas.LineTo(X + 3, Y + 4);

      ACanvas.MoveTo(X, Y + 2);
      ACanvas.LineTo(X + 2, Y + 4);


      ACanvas.MoveTo(X - 1, Y + 1);
      ACanvas.LineTo(X - 4, Y + 4);

      ACanvas.MoveTo(X, Y + 2);
      ACanvas.LineTo(X - 3, Y + 4);

      ACanvas.MoveTo(X, Y + 1);
      ACanvas.LineTo(X - 2, Y + 4);

    end;
    1:
    begin
      ACanvas.MoveTo(X, Y+3);
      ACanvas.LineTo(X, Y+4);

      ACanvas.MoveTo(X + 1, Y + 2);
      ACanvas.LineTo(X + 4, Y - 1);

      ACanvas.MoveTo(X, Y + 2);
      ACanvas.LineTo(X + 3, Y - 1);

      ACanvas.MoveTo(X, Y + 1);
      ACanvas.LineTo(X + 2, Y + 0);



      ACanvas.MoveTo(X - 1, Y + 2);
      ACanvas.LineTo(X - 4, Y - 1);

      ACanvas.MoveTo(X, Y + 2);
      ACanvas.LineTo(X - 3, Y - 1);

      ACanvas.MoveTo(X, Y + 1);
      ACanvas.LineTo(X - 2, Y + 0);


    end;
  end;
end;
procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
 ShadowColor: TColor);
var
 BX, BY: integer;
 TransparentColor: TColor;
begin
 TransparentColor := B.Canvas.Pixels[0, B.Height - 1];
 for BY := 0 to B.Height - 1 do
   for BX := 0 to B.Width - 1 do
   begin
     if B.Canvas.Pixels[BX, BY] <> TransparentColor then
       ACanvas.Pixels[X + BX, Y + BY] := ShadowColor;
   end;
end;

procedure DrawCheckMark(ACanvas: TCanvas; X, Y: integer);
begin
 Inc(X, 2);
 Dec(Y, 3);
 ACanvas.MoveTo(X , Y - 2);
 ACanvas.LineTo(X + 2, Y );
  ACanvas.LineTo(X + 7, Y - 5);

  ACanvas.MoveTo(X , Y - 3);
  ACanvas.LineTo(X + 2, Y - 1);
  ACanvas.LineTo(X + 7, Y - 6);

  ACanvas.MoveTo(X , Y - 4);
  ACanvas.LineTo(X + 2, Y - 2);
  ACanvas.LineTo(X + 7, Y - 7);
end;


{ TCustomComboSubClass }
//By Heath Provost (Nov 20, 2001)
// ComboBox Subclass WndProc.
// Message processing to allow control to repond to
// messages needed to paint using Office XP style.
procedure TControlSubClass.ControlSubClass(var Message: TMessage);

begin
  //Call original WindowProc FIRST. We are trying to emulate inheritance, so
  //original WindowProc must handle all messages before we do.


  if ((Message.Msg = WM_PAINT) and ((Control is TGraphicControl))) or
    ((Control.ClassName = 'TDBLookupComboBox') and (Message.Msg = WM_NCPAINT)) then
     Message.Result := 1
  else
       //: "Marcus Paulo Tavares" <marcuspt@terra.com.br>
    orgWindowProc(Message);

  if (XPMenu <> nil) and (not XPMenu.FActive)  then
  begin
    try
      Message.Result := 1;
      if Control <> nil then
      begin
        Control.WindowProc := orgWindowProc;
        if Control is TCustomEdit then
          TEdit(Control).Ctl3D := FCtl3D;
        if Control is TCustomRichEdit then
          TRichEdit(Control).BorderStyle := FBorderStyle;
        if Control.ClassName = 'TDBLookupComboBox' then
          TComboBox(Control).Ctl3D := FCtl3D;
        if Control is TCustomListBox then
          TListBox(Control).BorderStyle := FBorderStyle;
        if Control is TCustomListView then
          TListView(Control).BorderStyle := FBorderStyle;
        if Control is TCustomTreeView then
          TTreeView(Control).BorderStyle := FBorderStyle;
        Control := nil;
        Free;
      end;
      exit;
    except
      exit;
    end;
  end;

  FMsg := Message.Msg;
  case Message.Msg of


    EM_GETMODIFY, // For edit
    CM_INVALIDATE:
      begin
        FBuilding := true
      end;

    CM_PARENTCOLORCHANGED:
    begin
      PaintControlXP;
    end;

    WM_DESTROY:
      begin
       if not FBuilding then
       begin
         try
          if Control <> nil then
           begin
             Control.WindowProc := orgWindowProc;
             FBuilding := false;
             Free;
           end;
         except
         end;
         //FBuilding := false;
       end;
       Exit;
      end;

    WM_PAINT:
      begin
        FBuilding := false;
        PaintControlXP;
      end;

    CM_MOUSEENTER:
      if TControl(Control).Enabled then
       begin
//        if FmouseInControl then exit;
        FmouseInControl := true;
        if Control is TGraphicControl then
        begin
          Control.Repaint;
          exit;
        end;
        PaintControlXP;


        {if Control is TGraphicControl then
        begin
          if not FMouseInControl and Control.Enabled
            and (GetCapture = 0) then
          begin
            FMouseInControl := True;
            Control.Repaint;
          end;
        end
        else
        begin
          FmouseInControl := true;
          PaintControlXP;
        end;}


      end;
    CM_MOUSELEAVE:
     if TControl(Control).Enabled then
      begin
        FmouseInControl := false;
        if Control is TGraphicControl then
        begin
          Control.Invalidate;
          exit;
        end;
        PaintControlXP;


        {if Control is TGraphicControl then
        begin
          if FMouseInControl and Control.Enabled then
          begin
            FMouseInControl := False;
            Control.Invalidate;
          end;
        end
        else
        begin
          FmouseInControl := false;
          PaintControlXP;
        end;}
      end;

    WM_MOUSEMOVE:
      begin
        if TControl(Control).Enabled and (Control.ClassName = 'TUpDown') then
          PaintControlXP;
      end;
    WM_LBUTTONDOWN:
      begin
        FLButtonBressed := true;
        PaintControlXP;
      end;

    WM_LBUTTONUP:
      begin
       FLButtonBressed := false;
       if Control is TGraphicControl then
       begin
         Control.Repaint;
         exit;
       end;
       PaintControlXP;
      end;

    WM_KEYDOWN:
      if Message.WParam = VK_SPACE then
      begin
       FBressed := true;
       if not FIsKeyDown then
         PaintControlXP;
       FIsKeyDown := true;
      end;

    WM_KEYUP:
      if Message.WParam = VK_SPACE then
      begin
        FBressed := false;
        FIsKeyDown := false;
        PaintControlXP;
      end;

    WM_SETFOCUS:
      begin
        FmouseInControl := true;
        PaintControlXP;
      end;
    WM_KILLFOCUS:
      begin
        FmouseInControl := false;
        PaintControlXP;
      end;
    CM_FOCUSCHANGED:    //??
      PaintControlXP;

    CM_EXIT:
      begin
        FmouseInControl := false;
        PaintControlXP;
      end;

    BM_SETCHECK:
      begin
        FmouseInControl := false;
        PaintControlXP;
      end;
    BM_GETCHECK:
      begin
        FmouseInControl := false;
        PaintControlXP;
      end;
    CM_ENABLEDCHANGED:
      begin
        if (Message.WParam = 0) then FmouseInControl := false;//Dirk Bottcher <dirk.boettcher@gmx.net>
        PaintControlXP;
      end;

    CM_TEXTCHANGED:
      begin
        PaintControlXP;
      end;


    CM_CTL3DCHANGED, CM_PARENTCTL3DCHANGED:
      begin
        FBuilding := true;
      end;
    WM_LBUTTONDBLCLK:    //for button, check
      begin
        if (Control is TButton) or
           (Control is TSpeedButton) or
           (Control is TCheckBox)  then
          Control.Perform(WM_LBUTTONDOWN, Message.WParam , Longint(Message.LParam));
      end;
    {CN_DRAWITEM,} BM_SETSTATE:
      begin
        PaintControlXP;   // button
      end;
    WM_WINDOWPOSCHANGED, CN_PARENTNOTIFY:     // Moving From parent to other
      begin
        FBuilding := true
      end;
    WM_NCPAINT:
      begin
        if (Control is TCustomListBox) or (Control is TCustomTreeView) or
         (Control is TCustomListBox) 
        then
          PaintNCWinControl;
      end;
  end;

end;

// changes added by Heath Provost (Nov 20, 2001)
{ TCustomComboSubClass }
// paints an overlay over the control to make it mimic
// Office XP style.

procedure TControlSubClass.PaintControlXP;
begin

  If Control is TWinControl then
    FIsFocused := TWinControl(Control).Focused
  else
    FIsFocused := false;
  {$IFDEF VER6U}
  if (Control is TCustomCombo) then
    PaintCombo;
  {$ELSE}
  if (Control is TCustomComboBox) then
    PaintCombo;
  {$ENDIF}
  if Control.ClassName = 'TDBLookupComboBox' then
   PaintDBLookupCombo;

  if Control is TCustomRichEdit then
    PaintRichEdit
  else
  if Control is TCustomEdit then
    PaintEdit;

  if Control is TCustomCheckBox then
    PaintCheckBox;
  if Control is TRadioButton then
    PaintRadio;

  if Control is TBitBtn then
    PaintBitButn
  else
  if Control is TButton then
    PaintButton;

  if Control is TUpDown then
    PaintUpDownButton;

  if Control is TSpeedButton then
    if Control.Visible then
      PaintSpeedButton;

  if Control is TCustomPanel then
    PaintPanel;

  if Control is TCustomGroupBox then
    PaintGroupBox;

  if (Control is TCustomListBox) or (Control is TCustomTreeView) or
    (Control is TCustomListView) 
  then
    PaintNCWinControl;

  if Control is TProgressBar then
    PaintProgressBar;

  if Control is TCustomHotKey then
    PaintHotKey;
{
  if Control is TDrawGrid then
    PaintGrid;
}
end;

procedure TControlSubClass.PaintCombo;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor, ArrowColor: TColor;
  X: integer;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

//    XPMenu.SetGlobalColor(C);
    if Control.Enabled then ArrowColor := clBlack else ArrowColor := clWhite;


    if (FmouseinControl) then
    begin
      borderColor := XPMenu.FFSelectBorderColor;
      SelectColor := XPMenu.FFSelectColor;
    end
    else
    begin
      borderColor := TComboBox(Control).Color;
      if Control.Tag = 1000 then
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColor)
      else
        selectColor := clBtnFace;
    end;

    if (not FmouseinControl) and (FIsFocused) then
    begin
      borderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
      SelectColor := XPMenu.FCheckedAreaColor;
    end;

    R := Control.ClientRect;

    C.Brush.Color := Control.Parent.Brush.Color;
    C.FrameRect(R);
    InflateRect(R, -1, -1);

    C.Pen.Color := C.Brush.Color;
    C.MoveTo(R.Left, R.Top);
    C.LineTo(R.Right, R.Top);

    InflateRect(R, 0, -1);

    if ( FmouseinControl or FIsFocused) then
      InflateRect(R, 1, 1);

    C.Brush.Color := TComboBox(Control).Color;;
    C.FrameRect(R);

    Inc(R.Bottom,1);
    C.Brush.Color := BorderColor;
    C.FrameRect(R);

    {$IFDEF VER6U}
    if TCustomCombo(Control).DroppedDown then
    {$ELSE}
    if TCustomComboBox(Control).DroppedDown then
    {$ENDIF}
    begin
      BorderColor := XPMenu.FFSelectBorderColor;
      ArrowColor := clWhite;
      SelectColor := XPMenu.FCheckedAreaSelectColor ;
    end;

    if TComboBox(Control).style <> csSimple then
    begin

      InflateRect(R, -1, -1);

      if Control.BiDiMode = bdRightToLeft then
        R.Right := R.Left + GetSystemMetrics(SM_CXHTHUMB) + 1
      else
        R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) - 1;

      if ( FmouseinControl or FIsFocused) then
      begin
        if Control.BiDiMode = bdRightToLeft then
          Inc(R.Right, 2)
        else
          Dec(R.Left, 1);
      end;

      C.Brush.Color := SelectColor;
      C.FillRect(R);

      if Control.BiDiMode = bdRightToLeft then
        R.Left := R.Right - 5
      else
        R.Right := R.Left + 5;

      C.Brush.Color := TComboBox(Control).Color;
      C.FillRect(R);

      C.Pen.Color := BorderColor;

      if Control.BiDiMode = bdRightToLeft then
      begin
        C.Moveto(R.Left, R.Top);
        C.LineTo(R.Left, R.Bottom);
      end
      else
      begin
        C.Moveto(R.Right, R.Top);
        C.LineTo(R.Right, R.Bottom);
      end;
      C.Pen.Color := arrowColor;

      R := Control.ClientRect;

      if Control.BiDiMode = bdRightToLeft then
        X := R.Left + 5
      else
        X := R.Right - 10;

      C.Moveto(X + 0, R.Top + 10);
      C.LineTo(X + 5, R.Top + 10);
      C.Moveto(X + 1, R.Top + 11);
      C.LineTo(X + 4, R.Top + 11);
      C.Moveto(X + 2, R.Top + 12);
      C.LineTo(X + 3, R.Top + 12);
    end;
  finally
    C.Free;
  end;

end;

procedure TControlSubClass.PaintDBLookupCombo;
var
  C: TControlCanvas;
  R: TRect;
  FrameColor, SelectColor, BorderColor, ArrowColor: TColor;
  X: integer;
  DC: HDC;

begin
  C := TControlCanvas.Create;
  DC := GetWindowDC(TWinControl(Control).Handle);
  try
    C.Control := Control;
    C.Handle := DC;
    if TComboBox(Control).Ctl3D then
    begin
     FBuilding := true;
     TComboBox(Control).Ctl3D := false;
    end;

    //XPMenu.SetGlobalColor(C);
    if Control.Enabled then ArrowColor := clBlack else ArrowColor := clWhite;


    if (FmouseinControl) then
    begin
      FrameColor := XPMenu.FFSelectBorderColor;
      borderColor := XPMenu.FFSelectBorderColor;
      SelectColor := XPMenu.FFSelectColor;
    end
    else
    begin
      FrameColor := GetShadeColor(C, Control.Parent.Brush.Color, 60);
      borderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
      selectColor := clBtnFace;
    end;
    if (not FmouseinControl) and (FIsFocused) then
    begin
      FrameColor := GetShadeColor(C, Control.Parent.Brush.Color, 60);
      borderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
      SelectColor := XPMenu.FCheckedAreaColor;
    end;


    R := Rect(0, 0, Control.Width, Control.Height);
    C.Brush.Color := TComboBox(Control).Color;
    C.Brush.Color := FrameColor;
    C.FrameRect(R);


    R := Control.ClientRect;

    // Move the thumb one pixel to the right and one pixel down
    OffsetRect(R, 1, 1);


    C.Brush.Color := TComboBox(Control).Color;
    C.FrameRect(R);

    {$IFDEF VER6U}
    if TCustomCombo(Control).DroppedDown then
    {$ELSE}
    if TCustomComboBox(Control).DroppedDown then
    {$ENDIF}
    begin
      BorderColor := XPMenu.FFSelectBorderColor;
      ArrowColor := clWhite;
      SelectColor := XPMenu.FCheckedAreaSelectColor ;
    end;

    if TComboBox(Control).style <> csSimple then
    begin

      InflateRect(R, -1, -1);

      if Control.BiDiMode = bdRightToLeft then
        R.Right := R.Left + GetSystemMetrics(SM_CXHTHUMB) + 1
      else
        R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) - 1;

      if ( FmouseinControl or FIsFocused) then
      begin
        if Control.BiDiMode = bdRightToLeft then
          Inc(R.Right, 1)
        else
          Dec(R.Left, 1);
      end;


      C.Brush.Color := SelectColor;
      C.FillRect(R);
      C.Brush.Color := BorderColor;
      C.FrameRect(R);

      if Control.BiDiMode = bdRightToLeft then
        R.Left := R.Right - 5
      else
        R.Right := R.Left + 5;

      C.Brush.Color := TComboBox(Control).Color;
      C.FillRect(R);

      C.Pen.Color := BorderColor;

      if Control.BiDiMode = bdRightToLeft then
      begin
        C.Moveto(R.Left, R.Top);
        C.LineTo(R.Left, R.Bottom);
      end
      else
      begin
        C.Moveto(R.Right, R.Top);
        C.LineTo(R.Right, R.Bottom);
      end;
      C.Pen.Color := arrowColor;

      R := Control.ClientRect;

      if Control.BiDiMode = bdRightToLeft then
        X := R.Left + 5
      else
        X := R.Right - 9; // Changed by Uwe Runkel, uwe@runkel.info
                          // Changed value from 10 to 9 because the thumb has
                          // moved one pixel to the right

      C.Moveto(X + 0, R.Top + 8);
      C.LineTo(X + 5, R.Top + 8);
      C.Moveto(X + 1, R.Top + 9);
      C.LineTo(X + 4, R.Top + 9);
      C.Moveto(X + 2, R.Top + 10);
      C.LineTo(X + 3, R.Top + 10);
    end;
  finally
    C.Free;
    ReleaseDC(TWinControl(Control).Handle, DC);
  end;

end;

procedure TControlSubClass.PaintEdit;
var
  C: TControlCanvas;
  R: TRect;
  BorderColor: TColor;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    //XPMenu.SetGlobalColor(C);

    if TEdit(Control).Ctl3D <> false then
    begin
      FBuilding := true;
      TEdit(Control).Ctl3D := false;
    end;

    if (FmouseinControl) or (FIsFocused) then
      borderColor := NewColor(C, XPMenu.FFSelectBorderColor, 60)
    else
      borderColor := GetShadeColor(C, Control.Parent.Brush.Color, 60);


    if FBorderStyle = bsNone then
    begin
      if (FmouseinControl) and (not FIsFocused) then
        //borderColor := NewColor(C, Control.Parent.Brush.Color, 60)
        borderColor := NewColor(C, MergColor([TEdit(Control).Color,Control.Parent.Brush.Color]), 40)

      else
        borderColor := TEdit(Control).Color;
    end;


    R := Control.ClientRect;

    C.Pen.Color := BorderColor;
    C.Brush.Style := bsClear;

    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  finally
    C.Free;
  end;
end;

procedure TControlSubClass.PaintRichEdit;
var
  C: TControlCanvas;
  R: TRect;
  BorderColor: TColor;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control.Parent;

    R := Control.BoundsRect;
    InflateRect(R, 1, 1);

    if FBorderStyle  = bsSingle then
    begin
      FBuilding := true;
      TRichEdit(Control).BorderStyle := bsNone;
      if TRichEdit(Control).BorderWidth < 2 then
        TRichEdit(Control).BorderWidth := 2;
    end;

    if (FmouseinControl) or (FIsFocused) then
      borderColor := NewColor(C, XPMenu.FFSelectBorderColor,60)


    else
    begin
      if FBorderStyle = bsSingle then
        borderColor := GetShadeColor(C, Control.Parent.Brush.Color, 60)
      else
        borderColor := Control.Parent.Brush.Color;
    end;

    Frame3D(C, R, borderColor, borderColor, 1);

  finally
    C.Free;
  end;

end;

procedure TControlSubClass.PaintCheckBox;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor: TColor;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    if FMouseInControl then
    begin
      SelectColor := XPMenu.FFSelectColor;
      BorderColor := xpMenu.FFSelectBorderColor;
    end
    else
    begin
      SelectColor := clWindow;
      BorderColor := clBtnShadow;
    end;

    if (FIsFocused) then
    begin
      SelectColor := XPMenu.FFSelectColor;
      BorderColor := xpMenu.FFSelectBorderColor;
    end;
    if (FBressed) or (FLButtonBressed ) then
      SelectColor := XPMenu.FCheckedAreaSelectColor ;

    if TCheckBox(Control).State = cbGrayed then
       SelectColor := clSilver ;
    R := Control.ClientRect;
    InflateRect(R, 0, -3);
    R.Top := R.Top + ((R.Bottom - R.Top - GetSystemMetrics(SM_CXHTHUMB)) div 2);
    R.Bottom := R.Top + GetSystemMetrics(SM_CXHTHUMB);

    if ((Control.BiDiMode = bdRightToLeft) and
       (TCheckBox(Control).Alignment = taRightJustify)) or
       ((Control.BiDiMode = bdLeftToRight) and
       (TCheckBox(Control).Alignment = taLeftJustify))
        then
      R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) + 1
    else
    if ((Control.BiDiMode = bdLeftToRight) and
       (TCheckBox(Control).Alignment  = taRightJustify)) or
       ((Control.BiDiMode = bdRightToLeft) and
       (TCheckBox(Control).Alignment  = taLeftJustify)) then
      R.Right := R.Left + GetSystemMetrics(SM_CXHTHUMB) - 1;



    C.Brush.Color := TCheckBox(Control).Color;
    C.FillRect(R);
    InflateRect(R, -2, -2);
    C.Brush.Color := SelectColor;
    C.Pen.Color := BorderColor;
    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

    if (TCheckBox(Control).Checked) or
       (TCheckBox(Control).State = cbGrayed) then
    begin
      if Control.Enabled then
      begin
        if (FBressed) or (FLButtonBressed ) then
          C.Pen.color := clWindow
        else
        begin
          if TCheckBox(Control).State = cbGrayed then
            C.Pen.color := clGray
          else
            C.Pen.color := clHighlight;
        end;
      end
      else
        C.Pen.color := xpMenu.FFDisabledColor;

      DrawCheckMark(C, R.Left, R.Bottom )
    end;

  finally
    C.Free;
  end;


end;

procedure TControlSubClass.PaintRadio;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor: TColor;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    if FMouseInControl then
    begin
      SelectColor := XPMenu.FFSelectColor;
      BorderColor := xpMenu.FFSelectBorderColor;;
    end
    else
    begin
      SelectColor := clWindow;
      BorderColor := clBtnShadow;
    end;
    if  (FIsFocused) then
      SelectColor := XPMenu.FFSelectColor;

    R := Control.ClientRect;
    InflateRect(R, 0, -4);

    R.Top := R.Top + ((R.Bottom - R.Top - GetSystemMetrics(SM_CXHTHUMB)) div 2);
    R.Bottom := R.Top + GetSystemMetrics(SM_CXHTHUMB)-1;


    if ((Control.BiDiMode = bdRightToLeft) and
       (TCheckBox(Control).Alignment = taRightJustify)) or
       ((Control.BiDiMode = bdLeftToRight) and
       (TCheckBox(Control).Alignment = taLeftJustify))
        then
      R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) + 1
    else
    if ((Control.BiDiMode = bdLeftToRight) and
       (TCheckBox(Control).Alignment  = taRightJustify)) or
       ((Control.BiDiMode = bdRightToLeft) and
       (TCheckBox(Control).Alignment  = taLeftJustify)) then
      R.Right := R.Left + GetSystemMetrics(SM_CXHTHUMB) - 1;

    C.Brush.Color := TCheckBox(Control).Color;
    C.FillRect(R);


    InflateRect(R, -2, -2);
    C.Brush.Color := SelectColor;
    C.Pen.Color := BorderColor;


    C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
    if TRadioButton(Control).Checked then
    begin
      InflateRect(R, -2, -2);

      if Control.Enabled then
        C.Brush.Color := clHighlight
      else
        C.Brush.color := xpMenu.FFDisabledColor;

      C.Pen.Color := C.Brush.Color;
      C.Ellipse(R.Left, R.Top, R.Right, R.Bottom);
    end;
  finally
    C.Free;
  end;


end;

procedure TControlSubClass.PaintButton;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor: TColor;
  Txt: string;
  TextRect: TRect;
  TxtFont: TFont;

  CWidth, CHeight, TWidth, THeight: integer;
  TextFormat: integer;

begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    if (FMouseInControl) then
    begin
      if Control.Tag = 1000 then // UseParentColor
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColorSelect)
      else
        SelectColor := NewColor(C, clBtnFace, xpMenu.FDimParentColorSelect);

      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
    end
    else
    begin
      if Control.Tag = 1000 then
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColor)
      else
        SelectColor := XPMenu.FFIconBackColor;
      BorderColor := clBtnShadow;
    end;


    if (not FmouseinControl) and (FIsFocused) then
    begin
      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
    end;

    TextFormat := DT_CENTER + DT_VCENTER;
    R := Control.ClientRect;

    CWidth := (R.Right - R.Left);
    CHeight := (R.Bottom - R.Top);

    C.Brush.Color := Control.Parent.Brush.Color;
    C.FillRect(R);

    C.Brush.Color := SelectColor;

    C.Pen.Color := NewColor(C, BorderColor, 30);
    C.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 4, 4);

    if TControl(Control).Enabled then
      if FBressed or (FLButtonBressed and FmouseinControl) {or FBressed} then
      begin
        C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
        C.MoveTo(R.Left , R.Bottom - 2);
        C.LineTo(R.Left , R.Top + 1);
        C.LineTo(R.Left + 1, R.Top );
        C.LineTo(R.Right - 1 , R.Top );
      end
      else
      begin
        C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
        C.MoveTo(R.Right - 1, R.Top + 1);
        C.LineTo(R.Right - 1, R.Bottom - 2);
        C.LineTo(R.Right - 2, R.Bottom - 1);
        C.LineTo(R.Left , R.Bottom - 1);
      end;

    Txt := TButton(Control).Caption;

    TextRect := R;

    TxtFont := TButton(Control).Font;
    C.Font.Assign (TxtFont);


    if TButton(Control).IsRightToLeft then
      TextFormat := TextFormat + DT_RTLREADING;

//--- //"Holger Lembke" <holger@hlembke.de>

    if (Txt <> '') then
    begin
      FillChar(TextRect, SizeOf(TextRect),0);
      DrawText(C.Handle,
               PChar(Txt), Length(Txt),
               TextRect,
               DT_CALCRECT + control.DrawTextBiDiModeFlags(0));
      TWidth := TextRect.Right;
      THeight := TextRect.Bottom;
    end
    else
    begin
      TWidth := 0;
      THeight := 0;
    end;

//---
    TextRect.Left := (CWidth - (TWidth)) div 2;
    TextRect.Right := TextRect.Left + TWidth;
    TextRect.Top := (CHeight - (THeight)) div 2;
    TextRect.Bottom := TextRect.Top + THeight;


    XPMenu.DrawTheText(Control,
                       Txt, '', C,
                       TextRect, false,
                       TControl(Control).Enabled,
                       TButton(Control).Default,
                       false,
                       TControl(Control).IsRightToLeft,
                       TxtFont,
                       TextFormat);

  finally
    C.Free;
  end;

end;

procedure TControlSubClass.PaintSpeedButton;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor: TColor;
  Txt: string;
  TextRect, IconRect: TRect;
  TxtFont: TFont;
  B, BF: TBitmap;
  CWidth, CHeight, BWidth, BHeight, TWidth, THeight, Space,
  NumGlyphs, Offset: integer;
  TextFormat: integer;
  FDown, FFlat, FTransparent: boolean;
  FLayout: TButtonLayout;
  P: TPoint;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    FDown := TSpeedButton(Control).Down;
    FFlat := TSpeedButton(Control).Flat;
    FTransparent := TSpeedButton(Control).Transparent;
    NumGlyphs := TSpeedButton(Control).NumGlyphs;

//----------
    if FFlat then
    if FMouseInControl then
    begin
      p := Mouse.CursorPos;
      P := Control.ScreenToClient(P);
      R := Control.ClientRect;
      FMouseInControl := (p.x >= R.Left) and (p.x <= R.Right) and
                (p.y >= R.Top) and (p.y <= R.Bottom);
    end;

//----------
    if (FMouseInControl) then
    begin
      if Control.Tag = 1000 then // UseParentColor
      begin
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColorSelect);
        if FFlat then
          SelectColor := xpMenu.FFSelectColor ;
      end
      else
      begin
        SelectColor := NewColor(C, clBtnFace, xpMenu.FDimParentColorSelect);
        if FFlat then
          SelectColor := xpMenu.FFSelectColor ;
      end;
      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
    end
    else
    begin
      if Control.Tag = 1000 then
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColor)
      else
        SelectColor := XPMenu.FFIconBackColor;
        if FFlat then
          SelectColor := TControl(Control).Parent.Brush.Color;

        if (Control.ClassName = 'TNavButton') and FFlat then
        begin
          SelectColor := TControl(Control).Parent.Brush.Color;
        end;
      BorderColor := clBtnShadow;
    end;


    if FDown then
    begin
      SelectColor := XPMenu.FCheckedAreaColor;
      BorderColor := xpMenu.FFSelectBorderColor;
    end;

    if FDown and FMouseInControl then
    begin
      SelectColor := XPMenu.FCheckedAreaSelectColor;
      BorderColor := xpMenu.FFSelectBorderColor;
    end;

    if not TControl(Control).Enabled then
      BorderColor := clBtnShadow;


    TextFormat := + DT_CENTER + DT_VCENTER;;
    R := Control.ClientRect;

    CWidth := (R.Right - R.Left);
    CHeight := (R.Bottom - R.Top);


    if (FDown or FMouseInControl) and FTransparent then
    begin
      BF := TBitmap.Create;
      try
        BF.Width := R.Right - R.Left;
        BF.Height := R.Bottom - R.Top;

        if FFlat then
        begin
          if GetDeviceCaps(C.Handle, BITSPIXEL) > 16 then
            BF.Canvas.Brush.Color := NewColor(C, xpMenu.FFSelectColor, 20)
          else
            BF.Canvas.Brush.Color := SelectColor;
        end
        else
        begin
          if GetDeviceCaps(C.Handle, BITSPIXEL) > 16 then
            BF.Canvas.Brush.Color := NewColor(C, SelectColor, 5)
          else
            BF.Canvas.Brush.Color := SelectColor;
        end;
        BF.Canvas.FillRect (R);
        BitBlt(C.handle,
               R.Left,
               R.Top,
               R.Right - R.left,
               R.Bottom - R.top,
               BF.Canvas.Handle,
               0,
               0,
               SRCAND);
      finally
        BF.Free;
      end;
    end;




    C.Brush.Color := SelectColor;
    if not FTransparent then
       c.FillRect (R);

    if Control.ClassName = 'TNavButton' then
    begin
      c.FillRect (R);
    end;
    C.Pen.Color := NewColor(C, BorderColor, 30);

    if (FFlat) and (not FTransparent) and (not FDown) and (not FMouseInControl) then
      C.Pen.Color :=  C.Brush.Color;

    if FTransparent  then
      C.Brush.Style := bsClear;
    if ((FTransparent) and (FMouseInControl)) or
       ((FTransparent) and (FDown)) or
       ((not FTransparent )) or
       ((not FFlat))
     then
     begin
       C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
     end;

    if TControl(Control).Enabled then
    begin
      if (FFlat) then
      begin
        if (FLButtonBressed ) or (FDown) then
        begin
          C.Pen.Color := BorderColor;
          C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);

          C.MoveTo(R.Left , R.Bottom - 1);
          C.LineTo(R.Left , R.Top );
          C.LineTo(R.Right  , R.Top );
        end
        else
        if (FMouseInControl) then
        begin
         C.Pen.Color := xpmenu.FFSelectBorderColor;
         C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;
      end;

      if (not FFlat) then
        if (FLButtonBressed ) or (FDown) then
        begin
          C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
          C.MoveTo(R.Left , R.Bottom - 1);
          C.LineTo(R.Left , R.Top );
          C.LineTo(R.Right  , R.Top );
        end
        else
        begin
          C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
          C.MoveTo(R.Right - 1, R.Top );
          C.LineTo(R.Right - 1, R.Bottom - 1);
          C.LineTo(R.Left , R.Bottom - 1);
        end;
    end;
    Txt := TSpeedButton(Control).Caption;

    TextRect := R;

    TxtFont := TSpeedButton(Control).Font;
    C.Font.Assign (TxtFont);

    TWidth := C.TextWidth(Txt);
    //THeight := C.TextHeight(Txt);
    TextRect.Left := (CWidth - TWidth) div 2;


    if TControl(Control).IsRightToLeft then
      TextFormat := TextFormat + DT_RTLREADING;

//--- //"Holger Lembke" <holger@hlembke.de>

    if (Txt <> '') then
    begin
      FillChar(TextRect, sizeof(TextRect),0);
      DrawText(C.Handle,
               PChar(Txt), Length(Txt),
               TextRect,
               DT_CALCRECT + control.DrawTextBiDiModeFlags(0));
      TWidth := TextRect.Right;
      THeight := TextRect.Bottom;
    end
    else
    begin
      TWidth := 0;
      THeight := 0;
    end;

//---

    if (TSpeedButton(Control).Glyph <> nil) then
    begin
      B := TBitmap.Create;
      BWidth := TSpeedButton(Control).Glyph.Width  div
                TSpeedButton(Control).NumGlyphs;

      BHeight :=  TSpeedButton(Control).Glyph.Height;

      B.Width := BWidth;
      B.Height := BHeight;
      if Length(TSpeedButton(Control).Caption) > 0 then
        Space := TSpeedButton(Control).Spacing
      else
        Space := 0;

      IconRect := Rect(R.Left , R.Top, R.Left+BWidth, R.Top + BHeight);


      // Suggested by : "Holger Lembke" <holger@hlembke.de>
      Offset := 1;
      if (not Control.Enabled) and (NumGlyphs > 1) then
        Offset := 2;
      if (FLButtonBressed) and (NumGlyphs > 2) then
        Offset := 3;
      if (FDown) and (NumGlyphs > 3) then
        Offset := 4;


      B.Canvas.CopyRect (Rect(0, 0, BWidth, BHeight),
                         TSpeedButton(Control).Glyph.Canvas,
                         Rect((BWidth * Offset) - BWidth, 0, BWidth * Offset, BHeight));


      FLayout := TSpeedButton(Control).Layout;
      if Control.IsRightToLeft then
      begin
        if FLayout = blGlyphLeft then
          FLayout := blGlyphRight
        else
          if FLayout = blGlyphRight then FLayout := blGlyphLeft;
      end;
      case FLayout of
        blGlyphLeft:
        begin
          IconRect.Left := (CWidth - (BWidth + Space + TWidth)) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Top  := ((CHeight - (BHeight)) div 2) - 1;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Left := IconRect.Right + Space;
          TextRect.Right := TextRect.Left + TWidth;
          TextRect.Top := (CHeight - (THeight)) div 2;
          TextRect.Bottom := TextRect.Top + THeight;

        end;
        blGlyphRight:
        begin
          IconRect.Right := (CWidth + (BWidth + Space + TWidth)) div 2;
          IconRect.Left := IconRect.Right - BWidth;
          IconRect.Top  := (CHeight - (BHeight)) div 2;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Right := IconRect.Left - Space;
          TextRect.Left := TextRect.Right - TWidth;
          TextRect.Top := (CHeight - (THeight)) div 2;
          TextRect.Bottom := TextRect.Top + THeight;

        end;
        blGlyphTop:
        begin
          IconRect.Left := (CWidth - BWidth) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Top  := (CHeight - (BHeight + Space + THeight)) div 2;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Left := (CWidth - (TWidth)) div 2;
          TextRect.Right := TextRect.Left + TWidth;
          TextRect.Top := IconRect.Bottom + Space;
          TextRect.Bottom := TextRect.Top + THeight;

        end;
        blGlyphBottom:
        begin
          IconRect.Left := (CWidth - BWidth) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Bottom  := (CHeight + (BHeight + Space + THeight)) div 2;
          IconRect.Top := IconRect.Bottom - BHeight;

          TextRect.Left := (CWidth - (TWidth)) div 2;
          TextRect.Right := TextRect.Left + TWidth;
          TextRect.Bottom := IconRect.Top - Space;
          TextRect.Top := TextRect.Bottom - THeight;

        end;

      end;

      xpMenu.DrawIcon(Control, C , B, IconRect,
        FMouseinControl,
        FIsFocused,
        TControl(Control).Enabled,
        FDown or FLButtonBressed,
        false,
        TControl(Control).IsRightToLeft);

      B.Free;
    end;

    XPMenu.DrawTheText(Control,
                       Txt, '', C,
                       TextRect, false,
                       TControl(Control).Enabled,
                       false,
                       false,
                       TControl(Control).IsRightToLeft,
                       TxtFont,
                       TextFormat);
  finally
    C.Free;
  end;

end;

procedure TControlSubClass.PaintBitButn;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor: TColor;
  Txt: string;
  TextRect, IconRect: TRect;
  TxtFont: TFont;
  B: TBitmap;
  CWidth, CHeight, BWidth, BHeight, TWidth, THeight, Space: integer;
  TextFormat: integer;
begin

  C := TControlCanvas.Create;
  try
    C.Control := Control;

    if (FMouseInControl or FBressed) then
    begin
      if (Control.Tag and 1000) = 1000 then
        SelectColor := NewColor(C,
                                  TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColorSelect)
      else
        SelectColor := NewColor(C, clBtnFace, xpMenu.FDimParentColorSelect);
      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
    end
    else
    begin
      if (Control.Tag and 1000) = 1000 then
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColor)
      else
        SelectColor := XPMenu.FFIconBackColor;
      BorderColor := clBtnShadow;
    end;

    if (not FmouseinControl) and (FIsFocused) then
    begin
      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor,60);
    end;

    if (Control.Tag and 1001) = 1001 then
    begin
      BorderColor := SelectColor;
    end;


    TextFormat := + DT_CENTER + DT_VCENTER;

    R := Control.ClientRect;

    CWidth := (R.Right - R.Left);
    CHeight := (R.Bottom - R.Top);


    C.Brush.Color := Control.Parent.Brush.Color;
    C.FillRect(R);

    C.Brush.Color := SelectColor;


    C.Pen.Color := NewColor(C, BorderColor, 30);
    c.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 4, 4);

    if (Control.Tag and 1001) <> 1001 then
    begin
      if TControl(Control).Enabled then
        if (FLButtonBressed and FmouseinControl) or (FBressed) then
        begin
          C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
          C.MoveTo(R.Left , R.Bottom - 2);
          C.LineTo(R.Left , R.Top + 1);
          C.LineTo(R.Left + 1, R.Top );
          C.LineTo(R.Right - 1 , R.Top );
        end
        else
        begin
          C.Pen.Color :=  GetShadeColor(C, BorderColor, 50);
          C.MoveTo(R.Right - 1, R.Top + 1);
          C.LineTo(R.Right - 1, R.Bottom - 2);
          C.LineTo(R.Right - 2, R.Bottom - 1);
          C.LineTo(R.Left , R.Bottom - 1);
        end;
    end;
    Txt := TBitBtn(Control).Caption;

    TextRect := R;

    TxtFont := TBitBtn(Control).Font;
    C.Font.Assign (TxtFont);

    TWidth := C.TextWidth(Txt);

    TextRect.Left := (CWidth - TWidth) div 2;


//--- //"Holger Lembke" <holger@hlembke.de>

    if (Txt <> '') then
    begin
      FillChar(TextRect, sizeof(TextRect),0);
      DrawText(C.Handle,
               PChar(Txt), Length(Txt),
               TextRect,
               DT_CALCRECT + control.DrawTextBiDiModeFlags(0));
      TWidth := TextRect.Right;
      THeight := TextRect.Bottom;
    end
    else
    begin
      TWidth := 0;
      THeight := 0;
    end;

//---
    if TBitBtn(Control).IsRightToLeft then
      TextFormat := TextFormat + DT_RTLREADING;


    if (TBitBtn(Control).Glyph <> nil) then
    begin
      B := TBitmap.Create;
      BWidth := TBitBtn(Control).Glyph.Width  div
                TBitBtn(Control).NumGlyphs;

      BHeight :=  TBitBtn(Control).Glyph.Height;

      B.Width := BWidth;
      B.Height := BHeight;
      Space := TBitBtn(Control).Spacing;
      if (Trim(TBitBtn(Control).Caption) = '') then Space := 0; //"Holger Lembke" <holger@hlembke.de>
      IconRect := Rect(R.Left , R.Top, R.Left+BWidth, R.Top + BHeight);

      B.Canvas.CopyRect (Rect(0, 0, BWidth, BHeight),
                         TBitBtn(Control).Glyph.Canvas,
                         Rect(0, 0, BWidth, BHeight));

      case TBitBtn(Control).Layout of
        blGlyphLeft:
        begin
          IconRect.Left := (CWidth - (BWidth + Space + TWidth)) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Top  := (CHeight - (BHeight)) div 2;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Left := IconRect.Right + Space;
          TextRect.Right := TextRect.Left + TWidth;

          TextRect.Top := (CHeight - (THeight)) div 2;
          TextRect.Bottom := TextRect.Top + THeight;
        end;
        blGlyphRight:
        begin
          IconRect.Right := (CWidth + (BWidth + Space + TWidth)) div 2;
          IconRect.Left := IconRect.Right - BWidth;
          IconRect.Top  := (CHeight - (BHeight)) div 2;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Right := IconRect.Left - Space;
          TextRect.Left := TextRect.Right - TWidth;
          TextRect.Top := (CHeight - (THeight)) div 2;
          TextRect.Bottom := TextRect.Top + THeight;
        end;
        blGlyphTop:
        begin
          IconRect.Left := (CWidth - BWidth) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Top  := (CHeight - (BHeight + Space + THeight)) div 2;
          IconRect.Bottom := IconRect.Top + BHeight;

          TextRect.Left := (CWidth - (TWidth)) div 2;
          TextRect.Right := TextRect.Left + TWidth;
          TextRect.Top := IconRect.Bottom + Space;
          TextRect.Bottom := TextRect.Top + THeight;

        end;
        blGlyphBottom:
        begin
          IconRect.Left := (CWidth - BWidth) div 2;
          IconRect.Right := IconRect.Left + BWidth;
          IconRect.Bottom  := (CHeight + (BHeight + Space + THeight)) div 2;
          IconRect.Top := IconRect.Bottom - BHeight;

          TextRect.Left := (CWidth - (TWidth)) div 2;
          TextRect.Right := TextRect.Left + TWidth;
          TextRect.Bottom := IconRect.Top - Space;
          TextRect.Top := TextRect.Bottom - THeight;

        end;
      end;

      xpMenu.DrawIcon(Control, C , B, IconRect,
        FMouseinControl,
        FIsFocused,
        TControl(Control).Enabled,
        false,
        false,
        TControl(Control).IsRightToLeft);

      B.Free;
    end;

    if (Control.Tag and 1002) = 1002 then
    begin
      if TBitBtn(Control).IsRightToLeft then
        TextFormat := + DT_RIGHT + DT_VCENTER
      else
        TextFormat := + DT_LEFT + DT_VCENTER;
      TextRect := R;
      InflateRect(TextRect, -4,-2);
    end;

    XPMenu.DrawTheText(Control,
                       Txt, '', C,
                       TextRect, false,
                       TControl(Control).Enabled,
                       TBitBtn(Control).Default,
                       false,
                       TControl(Control).IsRightToLeft,
                       TxtFont,
                       TextFormat);

  finally
    C.Free;
  end;
end;

procedure TControlSubClass.PaintUpDownButton;
var
  C: TControlCanvas;
  R: TRect;
  SelectColor, BorderColor, ArrowColor: TColor;
  P: TPoint;
  H: integer;

  procedure DrawUpDownButton(ARect: TRect; Arrow: integer; Active: boolean);
  begin
    if Control.Enabled then ArrowColor := clBlack else ArrowColor := clWhite;
    if Active then
    begin
      if FLButtonBressed then
      begin
        BorderColor := XPMenu.FFSelectBorderColor;
        SelectColor := XPMenu.FCheckedAreaSelectColor ;
        ArrowColor := clWhite;
      end
      else begin
        BorderColor := XPMenu.FFSelectBorderColor;
        SelectColor := NewColor(C, XPMenu.FFSelectColor, 60);//XPMenu.FFSelectColor;
      end;
    end
    else begin
      if Control.Tag = 1000 then
        SelectColor := NewColor(C, TControl(Control).Parent.Brush.Color, xpMenu.FDimParentColor)
      else
        SelectColor := NewColor(C, XPMenu.FFSelectColor, xpMenu.FDimParentColor);//clBtnFace;
      BorderColor := NewColor(C, TControl(Control).Parent.Brush.Color, 80);//SelectColor;
    end;

    C.Pen.Color := BorderColor;
    C.Brush.Color := SelectColor;
    C.Font.Color := ArrowColor;

    if C.Pixels[ARect.Left, ARect.Top] <> ColorToRGB(BorderColor) then begin
      C.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      C.Pen.Color := ArrowColor;
      DrawArrow(C, ARect.Left + ((ARect.Right - ARect.Left) div 2),
                   ARect.Top + ((ARect.Bottom - ARect.Top)  div 2) -2, Arrow);
    end;
  end;

begin
  C := TControlCanvas.Create;
  try
    C.Control := Control;
    R := Control.ClientRect;

    H := (R.Bottom - R.Top) div 2;
    P := Control.ScreenToClient(Mouse.CursorPos);
    DrawUpDownButton(Rect(R.Left, R.Top, R.Right, R.Top + H), 0,
      TControl(Control).Enabled and FMouseInControl and (P.Y < H));
    DrawUpDownButton(Rect(R.Left, R.Bottom - H, R.Right, R.Bottom), 1,
      TControl(Control).Enabled and FMouseInControl and not(P.Y < H));
  finally
    C.Free;
  end;
end;

procedure TControlSubClass.PaintGroupBox;
var
  C: TControlCanvas;
  R, RText: TRect;
  ShadowColor, LightColor: TColor;
  TextHeight, TextWidth: integer;
  Text: string;
begin

 if FMsg <> WM_PAINT then exit;
  C := TControlCanvas.Create;
  try
    C.Control := Control;

    R := Control.ClientRect;
    C.Font.Assign (TGroupBox(Control).Font);
    C.Font.Height := TGroupBox(Control).Font.Height;
    Text := TGroupBox(Control).Caption;

    TextHeight := C.TextHeight(Text);
    TextWidth  := C.TextWidth(Text);
    if Length(Text) = 0 then
       TextHeight := C.TextHeight(' ');
    ShadowColor := GetShadeColor(C, TGroupBox(Control).color, 60);
    LightColor := NewColor(C, TGroupBox(Control).color, 60);

    InflateRect(R,-1, -1);
    Inc(R.Top, (TextHeight)-1);
    C.Brush.Style := bsClear;
    C.Pen.Color := TGroupBox(Control).Color; // Control Color;
    C.Rectangle (R.Left, R.Top, R.Right, R.Bottom);


    //----Draw the outer Frame
    R := Control.ClientRect;
    Inc(R.Top, (TextHeight div 2)-1);
    C.Pen.Color := TGroupBox(Control).Color;
    C.MoveTo(R.Left + 1, R.Top);   // Repeat
    C.LineTo(R.Left + 1, R.Bottom);
    if TGroupBox(Control).Ctl3D then
      Frame3D(C, R, LightColor, ShadowColor, 1)
    else
      Frame3D(C, R, ShadowColor, ShadowColor, 1);


    // Fill Upper part (outside frame)
    R := Control.ClientRect;
    R.Bottom := R.Top + (TextHeight div 2) + 1;
    C.Brush.Style := bsSolid;
    C.Brush.Color := Control.Parent.Brush.Color;  // Parent Color;
    C.Pen.Color := C.Brush.Color;
    C.FillRect(R);


    if Control.IsRightToLeft then
    begin
      C.TextFlags := ETO_RTLREADING;
      RText.Right := R.Right  - 9;
      RText.Left := RText.Right - TextWidth;
    end
    else
    begin
      RText.Left := R.Left + 9;
      RText.Right := RText.Left + TextWidth;
    end;

    RText.Top := R.Top ;
    RText.Bottom := R.Top + TextHeight;

                     //(inside frame)
    InflateRect(R, -1, 0);
    R.Top := R.Bottom;
    R.Bottom := R.Top + (TextHeight div 2) + 1;
    C.Brush.Style := bsSolid;
    R.Left := RText.Left;
    R.Right := RText.Right;
    C.Brush.Color := TGroupBox(Control).Color; // Control Color;
    C.Pen.Color := C.Brush.Color;
    C.FillRect(R);

    R.Right := Control.ClientRect.Right;

    C.MoveTo(R.Right-2, R.Top);
    C.LineTo(R.Right-2, RText.Bottom);

    C.Brush.Style := bsClear;
    if Control.IsRightToLeft then
      C.TextFlags := ETO_RTLREADING;

    C.TextRect (RText, RText.Left, RText.Top, Text);

    // Draw Upper Line
    R := Control.ClientRect;
    Inc(R.Top, (TextHeight div 2) + 1);
    if TGroupBox(Control).Ctl3D then
      C.Pen.Color := LightColor
    else
      C.Pen.Color := ShadowColor;
    C.MoveTo(R.Left, R.Top);
    C.LineTo(RText.Left, R.Top);

    C.MoveTo(RText.Right, R.Top);
    C.LineTo(R.Right -1, R.Top);

  finally
    C.Free;
  end;
end;

procedure TControlSubClass.PaintPanel;
var
  C: TControlCanvas;
  R: TRect;
  ShadowColor, LightColor: TColor;
begin
  if FMsg <> WM_PAINT then exit;
  C := TControlCanvas.Create;
  try
    C.Control := Control;

    R := Control.ClientRect;
    ShadowColor := GetShadeColor(C, TPanel(Control).color, 60);
    LightColor := NewColor(C, TPanel(Control).color, 60);
    if TPanel(Control).BevelOuter  <> bvNone then
    begin
      if TPanel(Control).BevelOuter = bvLowered then
        Frame3D(C, R, ShadowColor, LightColor, TPanel(Control).BevelWidth)
      else
        Frame3D(C, R, LightColor, ShadowColor, TPanel(Control).BevelWidth);
    end;

    if TPanel(Control).BevelInner  <> bvNone then
    begin
      InflateRect(R, -TPanel(Control).BorderWidth, -TPanel(Control).BorderWidth);

      if TPanel(Control).BevelInner = bvLowered then
        Frame3D(C, R, ShadowColor, LightColor, TPanel(Control).BevelWidth)
      else
        Frame3D(C, R, LightColor, ShadowColor, TPanel(Control).BevelWidth);
    end;
  finally
    C.Free;
  end;

end;

type
  TCastWinControl = class(TWinControl);

procedure TControlSubClass.PaintNCWinControl;
var
  DC: HDC;
  C: TControlCanvas;
  R: TRect;
  BorderColor: TColor;

begin
  C := TControlCanvas.Create;
  DC := GetWindowDC(TWinControl(Control).Handle);
  try
    C.Control := Control;
    C.Handle := DC;

    XPMenu.SetGlobalColor(C);

    if (FMouseInControl) or (FIsFocused) then
    begin
      if FBorderStyle = bsSingle then
        BorderColor := NewColor(C, XPMenu.FFSelectBorderColor, 60)
      else
        BorderColor := NewColor(C, XPMenu.FFSelectBorderColor, 80);
    end
    else
    begin
      if FBorderStyle = bsSingle then
        borderColor := GetShadeColor(C, Control.Parent.Brush.Color, 60)
      else
        borderColor := Control.Parent.Brush.Color;
    end;

    if TCastWinControl(Control).Ctl3D <> false then
    begin
      FBuilding := true;
      TCastWinControl(Control).Ctl3D := false;
    end;

    C.Pen.Color := BorderColor;
    C.Brush.Style := bsClear;

    R := Rect(0, 0, Control.Width, Control.Height);
    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  finally
    C.Free;
    ReleaseDC(TWinControl(Control).Handle, DC);
  end;
end;

procedure TControlSubClass.PaintProgressBar;
var
  DC: HDC;
  C: TControlCanvas;
  R: TRect;
  BorderColor: TColor;

begin
  C := TControlCanvas.Create;
  DC := GetWindowDC(TWinControl(Control).Handle);
  try
    C.Control := Control;
    C.Handle := DC;

    if (FMouseInControl) then
      BorderColor := XPMenu.FFSelectBorderColor
    else
      BorderColor := GetShadeColor(C, Control.Parent.Brush.Color, 60);

    C.Pen.Color := BorderColor;
    C.Brush.Style := bsClear;

    R := Rect(0, 0, Control.Width, Control.Height);
    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  finally
    C.Free;
    ReleaseDC(TWinControl(Control).Handle, DC);
  end;
end;

procedure TControlSubClass.PaintHotKey;
var
  DC: HDC;
  C: TControlCanvas;
  R: TRect;
  BorderColor: TColor;

begin
  C := TControlCanvas.Create;
  DC := GetWindowDC(TWinControl(Control).Handle);
  try
    C.Control := Control;
    C.Handle := DC;

    XPMenu.SetGlobalColor(C);

    if (FMouseInControl) or (FIsFocused) then
      BorderColor := NewColor(C, XPMenu.FFSelectBorderColor, 60)
    else
      BorderColor := GetShadeColor(C, Control.Parent.Brush.Color, 60);

    C.Pen.Color := BorderColor;
    C.Brush.Style := bsClear;

    R := Rect(0, 0, Control.Width, Control.Height);
    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, -1, -1);
    C.Pen.Color := clWindow;
    C.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

  finally
    C.Free;
    ReleaseDC(TWinControl(Control).Handle, DC);
  end;
end;

// XPMenuManager
//
// Uwe Runkel, uwe@runkel.info
//
// Enable XPMenu to be used globally (all windows in the application use XPMenu).
// Hence you don't need more than one instance in an application. However it is also
// possible to have more than one instance. But only one instance is used for subclassing.
// If this instance is destroyed the manager looks if there is another instance which is
// allowed to subclass.

constructor TXPMenuManager.Create;
begin
  inherited Create;
  FXPMenuList := TList.Create;         // list of XPMenu components in the application
  FFormList := TList.Create;           // list of subclassed forms
  FPendingFormsList := TList.Create;   // list of forms inserted but not subclassed yet
  FDisableSubclassing := false;        // This disables the XPMenuManager
  FActiveXPMenu := nil;                // Currently for subclassing used XPMenu
                                       //    if this is nil no subclassing is done.
  Application.HookMainWindow(MainWindowHook);
end;

destructor TXPMenuManager.Destroy;
begin
{Bret Goldsmith bretg@yahoo.com}
{alexs <alexs75@hotbox.ru>  }
  Application.UnhookMainWindow(MainWindowHook);

  FPendingFormsList.Free;
  FXPMenuList.Free;
  FFormList.Free;
  inherited;
end;

// A component has been inserted or removed, if it is a form and subclassing is
// allowed then subclass it, so this form doesn't need a XPMenu component as well
procedure TXPMenuManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (FActiveXPMenu = nil) or FDisableSubclassing then Exit;
  case Operation of
    opInsert:
      // At this place we cannot subclass the control because it did not yet get its
      // initial window procedure.
      // So we add it to an intermediate list and subclass it at a later moment.
      if (AComponent is TCustomForm) and (FPendingFormsList.IndexOf(AComponent) < 0) then
        FPendingFormsList.Add(AComponent);
    opRemove:
      if (AComponent is TWinControl) then
      begin
        if AComponent is TCustomForm then begin
          // Remove the destroyed form from any form list if it is still there.
          FPendingFormsList.Remove(AComponent);
          FFormList.Remove(AComponent);
        end;
      end;
  end;
end;

// Add some XPMenu to the manager
procedure TXPMenuManager.Add(AXPMenu: TXPMenu);
begin
  FXPMenuList.Add(AXPMenu);
  FFormList.Add(AXPMenu.Form);
  if (FActiveXPMenu = nil) and AXPMenu.Active and not(AXPMenu.DisableSubclassing) and
    not(FDisableSubclassing) then
  begin
    FActiveXPMenu := AXPMenu;
    CollectForms;
  end;
end;

// Remove some XPMenu from the manager
procedure TXPMenuManager.Delete(AXPMenu: TXPMenu);
begin
  if AXPMenu = FActiveXPMenu then
    UpdateActiveXPMenu(AXPMenu);
  FXPMenuList.Remove(AXPMenu);
end;

// Select a new ActiveXPMenu (except the one given in the parameter)
procedure TXPMenuManager.UpdateActiveXPMenu(AXPMenu: TXPMenu);
var
  Cnt : integer;
  XPM : TXPMenu;
  Item: TControlSubClass;
  Comp: TControlSubClass;

begin
  XPM := FindSubclassingXPMenu(AXPMenu);
  if XPM = nil then
  begin
    FPendingFormsList.Clear;
    if not Assigned(Application.MainForm) then Exit;
    for Cnt := 0 to FFormList.Count - 1 do
      if (AXPMenu = nil) or (FFormList[Cnt] <> AXPMenu.Form) then
        RemoveChildSubclassing(TCustomForm(FFormList[Cnt]));
    FFormList.Clear;
    FActiveXPMenu := XPM;
  end
  else begin
    if FActiveXPMenu = nil then
    begin
      FActiveXPMenu := XPM;
      CollectForms;
    end
    else begin
      for Cnt := 0 to FActiveXPMenu.ComponentCount - 1 do
        if (FActiveXPMenu.Components[Cnt] is TControlSubClass) then
        begin
          Comp := FActiveXPMenu.Components[Cnt] as TControlSubClass;
          if (AXPMenu <> nil) and not(AXPMenu.Form.ContainsControl(Comp.Control)) then
          begin
            Item := TControlSubClass.Create(XPM);
            Item.Control := Comp.Control;
            Item.orgWindowProc := Comp.orgWindowProc;
            Item.Control.WindowProc := Item.ControlSubClass;
            Item.XPMenu := XPM;
            Item.FCtl3D := Comp.FCtl3D;
            Item.FBorderStyle := Comp.FBorderStyle;
      {Item.FOnDrawCell := Comp.FOnDrawCell;}
            Item.FDefaultDrawing := Comp.FDefaultDrawing;
            Item.FSelCol := Comp.FSelCol;
            Item.FSelRow := Comp.FSelRow;
          end;
        end;
      FActiveXPMenu := XPM;
    end;
  end;
end;

// Find an XPMenu which can be used for subclassing
function TXPMenuManager.FindSubclassingXPMenu(Exclude: TXPMenu): TXPMenu;
var
  XPM: TXPMenu;
  Cnt: integer;

begin
  Result := nil;
  if (FXPMenuList.Count = 0) or FDisableSubclassing then Exit;
  Cnt := 0;
  repeat
    XPM := TXPMenu(FXPMenuList[Cnt]);
    if XPM.Active and not(XPM.DisableSubclassing) and (XPM <> Exclude)
    then Result := XPM;
    inc(Cnt);
  until (Result <> nil) or (Cnt = FXPMenuList.Count);
end;

// Listens to messages sent to the application and looks if a window is inserted.
function TXPMenuManager.MainWindowHook(var Message: TMessage): boolean;
var
  i: integer;
  NewForm: TCustomForm;

  FMenuItem: TMenuItem; // +jt
  FMenu: TMenu; // +jt
 r: TRECT; // +jt
 pt: TPOINT; // +jt
 hWndM: HWND; // +j
begin
  Result := false;
  // +ahuser// ahuser: "Andreas Hausladen" <Andreas.Hausladen@gmx.de>
  if XPMenuManager = nil then  // prevent AVs on termination
    Exit;
  // +ahuser
  if Message.Msg = WM_DRAWMENUBORDER then
  begin
   FMenuItem:=TMenuItem(Message.LParam);
   if Assigned(FMenuItem) then
   begin
     GetMenuItemRect(0,FMenuItem.Parent.Handle,FMenuItem.MenuIndex,r);
     FMenu := FMenuItem.Parent.GetParentMenu;
     pt.x:=r.Left+(r.Right-r.Left) div 2;
     pt.y:=r.Top+(r.Bottom-r.Top) div 2;
     hWndM :=WindowFromPoint(pt);
     if (hWndM <> 0) and Assigned(FActiveXPMenu) then  //Rappido <rappido@quicknet.nl> 2003 09 13
       FActiveXPMenu.DrawWindowBorder(hWndM, FMenu.IsRightToLeft);
   end;
 end;

 if Message.Msg = WM_DRAWMENUBORDER2 then
 begin
   hWndM := HWND(Message.LParam);
   if (hWndM <> 0) and Assigned(FActiveXPMenu) then //Rappido <rappido@quicknet.nl> 2003 09 13
     FActiveXPMenu.DrawWindowBorder(hWndM, boolean(Message.WParam));
 end;

 if (Assigned(FPendingFormsList)) and (FPendingFormsList <> nil) then
 try
    if (FPendingFormsList.Count > 0) then
    begin
      for i := 0 to FPendingFormsList.Count - 1 do begin
        NewForm := TCustomForm(FPendingFormsList[i]);
        if FFormList.IndexOf(NewForm) < 0 then begin
          FFormList.Add(NewForm);
          if not(FDisableSubclassing) then
            FActiveXPMenu.InitItems(NewForm, true, true);
        end;
      end;
      FPendingFormsList.Clear;
    end;
 except
 end;


end;

// Collect all forms of the application and subclass them
procedure TXPMenuManager.CollectForms;
var
  FCnt, CCnt: integer;
  HasXPMenu : boolean;

begin
  if not FDisableSubclassing then
    for FCnt := 0 to Screen.FormCount - 1 do
      if (FFormList.IndexOf(Screen.Forms[FCnt]) < 0) and (Screen.Forms[FCnt].Tag <> 999) then
      begin
        HasXPMenu := false;
        for CCnt := 0 to Screen.Forms[FCnt].ComponentCount - 1 do
          HasXPMenu := HasXPMenu or (Screen.Forms[FCnt].Components[CCnt] is TXPMenu);
        if not(HasXPMenu) then
          FPendingFormsList.Add(Screen.Forms[FCnt]);
      end;
end;

// Remove subclassing from the original components
procedure TXPMenuManager.RemoveChildSubclassing(AForm: TCustomForm);
var
  Cnt    : integer;
  Comp   : TComponent;
  Control: TControl;

begin
//exit;
  for Cnt := FActiveXPMenu.ComponentCount - 1 downto 0 do begin
    Comp := FActiveXPMenu.Components[Cnt];
    if (Comp is TControlSubClass) then begin
      Control := TControlSubClass(Comp).Control;
      if AForm.ContainsControl(Control) then begin
        try
          Control.WindowProc := TControlSubClass(Comp).orgWindowProc;
          if Control is TCustomEdit then begin
            TEdit(Control).Ctl3D := TControlSubClass(Comp).FCtl3D;
            TEdit(Control).BorderStyle := TControlSubClass(Comp).FBorderStyle;
          end;
          if Control.ClassName = 'TDBLookupComboBox' then
            TComboBox(Control).Ctl3D := TControlSubClass(Comp).FCtl3D;
          if Control is TCustomListBox then begin
            TListBox(Control).Ctl3D := TControlSubClass(Comp).FCtl3D;
            TListBox(Control).BorderStyle := TControlSubClass(Comp).FBorderStyle;
          end;
          if Control is TCustomListView then begin
            TListView(Control).Ctl3D := TControlSubClass(Comp).FCtl3D;
            TListView(Control).BorderStyle := TControlSubClass(Comp).FBorderStyle;
          end;
          if Control is TCustomTreeView then begin
            TTreeView(Control).Ctl3D := TControlSubClass(Comp).FCtl3D;
            TTreeView(Control).BorderStyle := TControlSubClass(Comp).FBorderStyle;
          end;
        except
        end;
      end;
    end;
  end;
end;

// Add a form manually to the current XPMenu
procedure TXPMenuManager.AddForm(AForm: TCustomForm);
begin
  if FPendingFormsList.IndexOf(AForm) < 0 then
    FPendingFormsList.Add(AForm);
end;

// Remove a form manually from the current XPMenu
procedure TXPMenuManager.RemoveForm(AForm: TCustomForm);
begin
  if FPendingFormsList.IndexOf(AForm) >= 0 then
    FPendingFormsList.Remove(AForm);
  if FFormList.IndexOf(AForm) >= 0 then
    FFormList.Remove(AForm);
end;

// Disable/Enable subclassing by the manager
procedure TXPMenuManager.SetDisableSubclassing(AValue: boolean);
begin
  if FDisableSubclassing = AValue then Exit;
  FDisableSubclassing := AValue;
  UpdateActiveXPMenu(nil);
end;

// Check if a Form is subclassed
function TXPMenuManager.IsFormSubclassed(AForm: TCustomForm): boolean;
begin
  Result := ((FFormList <> nil) and (FFormList.IndexOf(AForm) >= 0)) or
    ((FPendingFormsList <> nil) and (FPendingFormsList.IndexOf(AForm) >= 0));
end;

// Check if a Component is subclassed
function TXPMenuManager.IsComponentSubclassed(AComponent: TComponent): boolean;
var
  Cnt: integer;

begin
  Result := false;
  with FActiveXPMenu do
    for Cnt := 0 to ComponentCount - 1 do
      if Components[Cnt] is TControlSubClass then
        if TControlSubClass(Components[Cnt]).Control = TControl(AComponent) then
        begin
          Result := True;
          Break; // ahuser
        end;
end;


end.

