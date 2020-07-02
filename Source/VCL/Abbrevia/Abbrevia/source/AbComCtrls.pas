(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbComCtrls.pas                              *}
{*********************************************************}
{* ABBREVIA: Listview and treeview components that work  *}
{*   with an archive component.  The treeview can have a *}
{*   listview associated, in which case the listview will*}
{*   only show items in the selected folder.             *}
{*********************************************************}

unit AbComCtrls;

interface

{$I AbDefine.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, Graphics, AbBrowse,
  AbArcTyp;

const
  AbTreeArchiveImage        = 0;
  AbTreeFolderImage         = 1;
  AbTreeFolderExpandedImage = 2;

type
{ ===== TAbListItem ========================================================= }
  TAbListItem = class(TListItem)
  protected {private}
    FArchiveItem : TAbArchiveItem;
  protected {methods}
    function GetIsDirectory : Boolean;
    function GetIsEncrypted : Boolean;
  public {properties}
    property ArchiveItem : TAbArchiveItem
      read FArchiveItem
      write FArchiveItem;
    property IsDirectory : Boolean
      read GetIsDirectory;
    property IsEncrypted : Boolean
      read GetIsEncrypted;
  end;


{ ===== TAbListItems ======================================================== }
  TAbListItems = class(TListItems)
  protected {methods}
    function GetItem(aIndex: Integer): TAbListItem;
    procedure SetItem(aIndex: Integer; aValue: TAbListItem);
  public {properties}
    property Item[Index: Integer]: TAbListItem
      read GetItem
      write SetItem; default;
  end;


{ ===== TAbCustomListView =================================================== }
type
  TAbViewColumn =
    (vcName, vcFileType, vcLastModified, vcSize, vcRatio,
     vcPacked, vcCRC, vcAttributes, vcEncrypted, vcMethod, vcPath);
  TAbViewColumns = set of TAbViewColumn;

const
  AbDefVisibleColumns = [Low(TAbViewColumn)..High(TAbViewColumn)];

type
  TAbCustomTreeView = class;

  {$IF NOT DECLARED(TWindowProcPtr)}
  TWindowProcPtr = Pointer;
  {$IFEND}

  TAbCustomListView = class(TCustomListView)
  protected {private}
    FArchive : TAbBaseBrowser;
    FDefHeaderProc : TWindowProcPtr;
    FFlatList: Boolean;
    FHeaderHandle : HWND;
    FHeaderImages : TImageList;
    FHeaderInstance : Pointer;
    FInUpdateSortArrows: Boolean;
    FPath : string;
    FSortAscending : Boolean;
    FSortColIndex : Integer;
    FSortColumn : TAbViewColumn;
    FSortUpBmp : HBITMAP;
    FSortDownBmp : HBITMAP;
    FTreeView : TAbCustomTreeView;
    FVisibleColumns : TAbViewColumns;

  protected {methods}
    procedure ColClick(aColumn: TListColumn);
      override;
    function CreateListItem: TListItem;
      override;
    function CreateListItems: TListItems;
      override;
    procedure CreateWnd;
      override;
    function CustomDrawSubItem(Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
      override;
    procedure DblClick;
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    function GetListItems: TAbListItems;
    function GetVersion: string;
    procedure HeaderWndProc(var Msg: TMessage);
      virtual;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
      override;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SetArchive(aValue : TAbBaseBrowser);
    procedure SetFlatList(aValue : Boolean);
    procedure SetPath(aValue : string);
    procedure SetTreeView(aValue : TAbCustomTreeView);
    procedure SetVisibleColumns(aValue : TAbViewColumns);
    procedure UpdateColumns;
    procedure UpdateSortArrow;
    procedure UpdateView;

  protected {properties}
    property HeaderImages : TImageList
      read FHeaderImages;

  public {methods}
    constructor Create(aOwner: TComponent);
      override;
    destructor Destroy;
      override;
    procedure Sort(aColumn: TAbViewColumn; aAscending: Boolean);

  public {properties}
    property Archive : TAbBaseBrowser
      read FArchive
      write SetArchive;
    property Columns;
    // Show only items in the current path
    property FlatList : Boolean
      read FFlatList
      write SetFlatList;
    property Items: TAbListItems
      read GetListItems
      stored False;
    property TreeView : TAbCustomTreeView
      read FTreeView
      write SetTreeView;
    property Path : string
      read FPath
      write SetPath;
    property Version : string
      read GetVersion
      stored False;
    property VisibleColumns : TAbViewColumns
      read FVisibleColumns
      write SetVisibleColumns
      default AbDefVisibleColumns;
  end;


{ ===== TAbListView ========================================================= }
  TAbListView = class(TAbCustomListView)
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property Archive;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property Groups;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property GroupHeaderImages;
    property GroupView default False;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property Path;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property TreeView;
    property Version;
    property ViewStyle;
    property Visible;
    property VisibleColumns;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnItemChecked;
    property OnStartDock;
    property OnStartDrag;
  end;


{ ===== TAbCustomTreeView =================================================== }
  TAbCustomTreeView = class(TTreeView)
  protected {private}
    FArchive: TAbBaseBrowser;
    FListView: TAbCustomListView;
    FPath: string;

  protected {methods}
    procedure Change(aNode: TTreeNode);
      override;
    procedure DoChange(Sender : TObject);
      virtual;
    procedure GetSelectedIndex(aNode: TTreeNode);
      override;
    function GetVersion: string;
    procedure Notification(aComponent : TComponent; aOperation : TOperation);
      override;
    procedure SelectPathNode;
    procedure SetArchive(aValue: TAbBaseBrowser);
    procedure SetListView(aValue: TAbCustomListView);
    procedure SetPath(const aValue: string);

  public {methods}
    constructor Create(aOwner: TComponent);
      override;

  public {properties}
    property Archive: TAbBaseBrowser
      read FArchive
      write SetArchive;
    property HideSelection
      default False;
    property ListView: TAbCustomListView
      read FListView
      write SetListView;
    property Path: string
      read FPath
      write SetPath;
    property Version: string
      read GetVersion
      stored False;
  end;


{ ===== TAbTreeView ========================================================= }
  TAbTreeView = class(TAbCustomTreeView)
  published
    property Align;
    property Anchors;
    property Archive;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Indent;
    property Items;
    property ListView;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property Path;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Version;
    property Visible;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


{ ===== TAbProgressBar ====================================================== }
  TAbProgressBar = class(TProgressBar, IAbProgressMeter)
  protected {private}
    function  GetVersion : string;

  public {methods}
    procedure DoProgress(Progress : Byte);
    procedure Reset;

  published {properties}
    property Version: string
      read GetVersion
      stored False;
  end;


implementation

{$R AbComCtrls.res}

uses
  CommCtrl, Contnrs, Forms, ShellAPI, StrUtils, AbConst, AbResString, AbUtils,
  AbZipTyp;

const
  HDF_SORTDOWN = $0200;
  HDF_SORTUP   = $0400;

{ -------------------------------------------------------------------------- }
{$IF NOT DECLARED(StartsText)}
function StartsText(const aSubText, aText: string): Boolean;
begin
  Result := (Length(aText) > Length(aSubText)) and
    SameText(aSubText, Copy(aText, 1, Length(aSubText)));
end;
{$IFEND}
{ -------------------------------------------------------------------------- }
function AbNormalizeFilename(const aFilename: string): string;
var
  i: Integer;
begin
  Result := aFilename;
  for i := 1 to Length(Result) do
    if IsDelimiter('\/', Result, i) then
      Result[i] := PathDelim;
  if IsDelimiter(PathDelim, Result, Length(Result)) then
    SetLength(Result, Length(Result) - 1);
end;
{ -------------------------------------------------------------------------- }
var
  ComCtl32MajorVer: Integer = -1;

function IsComCtl32Version6: Boolean;
type
  PDllVersionInfo = ^TDllVersionInfo;
  TDllVersionInfo = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
  end;
var
  DllGetVersion: function(pdvi: PDllVersionInfo): HRESULT; stdcall;
  dvi: TDllVersionInfo;
  hComCtl32: HMODULE;
begin
  if ComCtl32MajorVer = -1 then begin
    ComCtl32MajorVer := 0;
    hComCtl32 := LoadLibrary(comctl32);
    if hComCtl32 <> 0 then begin
      DllGetVersion := GetProcAddress(hComCtl32, 'DllGetVersion');
      if Assigned(DllGetVersion) then begin
        dvi.cbSize := SizeOf(dvi);
        if Succeeded(DllGetVersion(@dvi)) then
          ComCtl32MajorVer := dvi.dwMajorVersion;
      end;
      FreeLibrary(hComCtl32);
    end;
  end;
  Result := ComCtl32MajorVer >= 6;
end;
{ -------------------------------------------------------------------------- }
function SameEvent(const aEvent1, aEvent2: TNotifyEvent): Boolean;
begin
  Result := (TMethod(aEvent1).Code = TMethod(aEvent2).Code) and
    (TMethod(aEvent1).Data = TMethod(aEvent2).Data);
end;



{ ===== TAbListItem ========================================================= }
function TAbListItem.GetIsDirectory: Boolean;
begin
  Result := (ArchiveItem = nil) or ArchiveItem.IsDirectory;
end;
{ -------------------------------------------------------------------------- }
function TAbListItem.GetIsEncrypted: Boolean;
begin
  Result := (ArchiveItem <> nil) and ArchiveItem.IsEncrypted;
end;


{ ===== TAbListItems ======================================================== }
function TAbListItems.GetItem(aIndex: Integer): TAbListItem;
begin
  Result := inherited Item[aIndex] as TAbListItem;
end;
{ -------------------------------------------------------------------------- }
procedure TAbListItems.SetItem(aIndex: Integer; aValue: TAbListItem);
begin
  inherited Item[aIndex] := aValue;
end;

{ ===== TAbCustomListView =================================================== }
constructor TAbCustomListView.Create(aOwner: TComponent);
var
  Bmp : TBitmap;
  sfi: SHFILEINFO;
begin
  inherited;
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
  // Load header image into an image list;  the header's hbm property
  // doesn't support transparency
  FHeaderImages := TImageList.Create(Self);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, 'AbComCtrls_Lock');
    FHeaderImages.AddMasked(Bmp, clFuchsia);
  finally
    Bmp.Free;
  end;
  // Load system image lists
  LargeImages := TImageList.Create(Self);
  LargeImages.ShareImages := True;
  LargeImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
    SHGFI_LARGEICON or SHGFI_SYSICONINDEX);
  SmallImages := TImageList.Create(Self);
  SmallImages.ShareImages := True;
  SmallImages.Handle := SHGetFileInfo('', 0, sfi, SizeOf(sfi),
    SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  // Load sort arrow bitmaps for older comctrl32.dll versions
  FSortAscending := True;
  FSortColumn := vcName;
  if not IsComCtl32Version6 then begin
    FSortUpBmp := LoadImage(HInstance, 'AbComCtrls_SortUp', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
    FSortDownBmp := LoadImage(HInstance, 'AbComCtrls_SortDown', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DColors);
  end;
  // Set default column visibility
  VisibleColumns := AbDefVisibleColumns;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomListView.Destroy;
begin
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, NativeInt(FDefHeaderProc));
  FreeObjectInstance(FHeaderInstance);
  if FSortUpBmp <> 0 then
    DeleteObject(FSortUpBmp);
  if FSortDownBmp <> 0 then
    DeleteObject(FSortDownBmp);
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.ColClick(aColumn: TListColumn);
var
  Col: TAbViewColumn;
begin
  inherited;
  Col := TAbViewColumn(aColumn.Tag);
  Sort(Col, (Col <> FSortColumn) or not FSortAscending);
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.CreateListItem: TListItem;
begin
  Result := TAbListItem.Create(Items);
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.CreateListItems: TListItems;
begin
  Result := TAbListItems.Create(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.CreateWnd;
begin
  inherited;
  FHeaderHandle := ListView_GetHeader(Handle);
  if FHeaderHandle <> 0 then begin
    FDefHeaderProc := TWindowProcPtr(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, NativeInt(FHeaderInstance));
  end;
  Header_SetImageList(ListView_GetHeader(Handle), FHeaderImages.Handle);
  UpdateColumns;
  UpdateView;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.CustomDrawSubItem(Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
var
  i: Integer;
  R: TRect;
begin
  Result := True;
  if (Stage = cdPrePaint) and TAbListItem(Item).IsEncrypted then
    if TAbViewColumn(Columns[SubItem].Tag) = vcEncrypted then begin
      Result := False;
      R := Item.DisplayRect(drBounds);
      Inc(R.Left, 6);
      for i := 0 to SubItem - 1 do
        Inc(R.Left, Columns[i].Width);
      HeaderImages.Draw(Canvas, R.Left, R.Top, 0);
    end
    else begin
      Result := True;
      // Fixed other columns drawing with wrong font after using TImageList.Draw
      Canvas.Brush.Color := ColorToRGB(Color);
      SetBkMode(Canvas.Handle, TRANSPARENT);
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.DblClick;
begin
  inherited;
  if TAbListItem(Selected).IsDirectory then
    if Path = '' then
      Path := Selected.Caption
    else
      Path := Path + PathDelim + Selected.Caption;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.DoChange(Sender: TObject);
begin
  UpdateView;
  if (Sender = FArchive) and Assigned(FTreeView) then
    FTreeView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.GetListItems: TAbListItems;
begin
  Result := inherited Items as TAbListItems;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.GetVersion: string;
begin
  Result := AbVersionS;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.HeaderWndProc(var Msg: TMessage);
const
  FMT_MASK = HDF_BITMAP or HDF_BITMAP_ON_RIGHT or HDF_SORTDOWN or HDF_SORTUP;
var
  Item: THDItem;
begin
  if (Msg.Msg = HDM_SETITEM) and not FInUpdateSortArrows then begin
    Item.Mask := HDI_FORMAT;
    if Header_GetItem(FHeaderHandle, Msg.WParam, Item) then begin
      PHDItem(Msg.LParam).Mask := PHDItem(Msg.LParam).Mask and not HDI_BITMAP;
      PHDItem(Msg.LParam).fmt := PHDItem(Msg.LParam).fmt and not FMT_MASK
        or (Item.fmt and FMT_MASK);
    end;
  end;
  Msg.Result := CallWindowProc(FDefHeaderProc, FHeaderHandle, Msg.Msg,
    Msg.WParam, Msg.LParam);
  if Msg.Msg = WM_DESTROY then
    FHeaderHandle := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := (vcEncrypted in VisibleColumns) and (Stage = cdPrePaint) and
    (Target = dtSubItem);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited;
  if aOperation = opRemove then begin
    if aComponent = FArchive then begin
      FArchive := nil;
      Clear;
    end;
    if aComponent = FTreeView then begin
      if Assigned(FArchive) and SameEvent(FArchive.OnChange, FTreeView.DoChange) then
        FArchive.OnChange := DoChange;
      FTreeView := nil;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetArchive(aValue: TAbBaseBrowser);
begin
  if aValue <> FArchive then begin
    if Assigned(FArchive) then begin
      FArchive.RemoveFreeNotification(Self);
      if SameEvent(FArchive.OnChange, DoChange) then
        if Assigned(TreeView) and (TreeView.Archive = FArchive) then
          FArchive.OnChange := TreeView.DoChange
        else
          FArchive.OnChange := nil;
    end;
    FArchive := aValue;
    if Assigned(FArchive) then begin
      FArchive.FreeNotification(Self);
      FArchive.OnChange := DoChange;
      DoChange(Self);
    end
    else
      Items.Clear;
    if Assigned(TreeView) then
      TreeView.Archive := aValue;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetFlatList(aValue : Boolean);
begin
  if aValue <> FFlatList then begin
    FFlatList := aValue;
    UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetPath(aValue: string);
begin
  if aValue <> FPath then begin
    FPath := ExcludeTrailingPathDelimiter(aValue);
    if Assigned(TreeView) then
      TreeView.Path := aValue;
    if not FlatList then
      UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetTreeView(aValue: TAbCustomTreeView);
begin
  if aValue <> FTreeView then begin
    if Assigned(FTreeView) then begin
      FTreeView.RemoveFreeNotification(Self);
      FTreeView.ListView := nil;
    end;
    FTreeView := aValue;
    if Assigned(FTreeView) then begin
      FTreeView.FreeNotification(Self);
      if Assigned(FArchive) then
        FTreeView.Archive := FArchive
      else if Assigned(FTreeView.Archive) then
        Archive := FTreeView.Archive;
      FTreeView.ListView := Self;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.SetVisibleColumns(aValue : TAbViewColumns);
begin
  if aValue <> FVisibleColumns then begin
    FVisibleColumns := aValue;
    UpdateColumns;
    UpdateView;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomListView_SortProc(aItem1, aItem2: TAbListItem;
  aListView: TAbCustomListView): Integer; stdcall;
var
  Item1, Item2: TAbArchiveItem;
  Ratio1, Ratio2: Single;
begin
  if aItem1.IsDirectory <> aItem2.IsDirectory then
    if aItem1.IsDirectory then
      Result := -1
    else
      Result := 1
  else begin
    Result := 0;
    if aListView.FSortColumn in [vcFileType, vcPath] then begin
      Result := CompareText(aItem1.SubItems[aListView.FSortColIndex],
                            aItem2.SubItems[aListView.FSortColIndex]);
    end
    else if not aItem1.IsDirectory then begin
      // Don't do more advanced sorts for directories, since they may be
      // implicitly stored and won't have corresponding archive items
      Item1 := aItem1.ArchiveItem;
      Item2 := aItem2.ArchiveItem;
      case aListView.FSortColumn of
        vcLastModified:
          begin
            if Item1.LastModTimeAsDateTime < Item2.LastModTimeAsDateTime then
              Result := -1
            else if Item1.LastModTimeAsDateTime > Item2.LastModTimeAsDateTime then
              Result := 1;
          end;
        vcSize:
          begin
            if Item1.UncompressedSize < Item2.UncompressedSize then
              Result := -1
            else if Item1.UncompressedSize > Item2.UncompressedSize then
              Result := 1;
          end;
        vcRatio:
          begin
            if Item1.UncompressedSize > 0 then
              Ratio1 := Item1.CompressedSize / Item1.UncompressedSize
            else
              Ratio1 := 1;
            if Item2.UncompressedSize > 0 then
              Ratio2 := Item2.CompressedSize / Item2.UncompressedSize
            else
              Ratio2 := 1;
            if Ratio1 > Ratio2 then
              Result := -1
            else if Ratio1 < Ratio2 then
              Result := 1
          end;
        vcPacked:
          begin
            if Item1.CompressedSize < Item2.CompressedSize then
              Result := -1
            else if Item1.CompressedSize > Item2.CompressedSize then
              Result := 1;
          end;
        vcCRC:
          begin
            if Longword(Item1.CRC32) < Longword(Item2.CRC32) then
              Result := -1
            else if Longword(Item1.CRC32) > Longword(Item2.CRC32) then
              Result := 1;
          end;
        vcAttributes,
        vcMethod:
          begin
            Result := CompareText(aItem1.SubItems[aListView.FSortColIndex],
                                  aItem2.SubItems[aListView.FSortColIndex]);
          end;
        vcEncrypted:
          begin
            if not Item1.IsEncrypted and Item2.IsEncrypted then
              Result := -1
            else if Item1.IsEncrypted and not Item2.IsEncrypted then
              Result := 1
          end;
      end;
    end;
    if Result = 0 then
      Result := AnsiCompareText(aItem1.Caption, aItem2.Caption);
  end;
  if not aListView.FSortAscending then
    Result := -Result;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.Sort(aColumn: TAbViewColumn; aAscending: Boolean);
begin
  if (aColumn <> FSortColumn) or (aAscending <> FSortAscending) then begin
    FSortColumn := aColumn;
    FSortAscending := aAscending;
    UpdateSortArrow;
    CustomSort(TLVCompare(@TAbCustomListView_SortProc), LPARAM(Self));
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.UpdateColumns;
const
  ColWidths: array[TAbViewColumn] of Integer = (
    180{vcName}, 110{vcFileType}, 130{vcLastModified}, 80{vcSize}, 50{vcRatio},
    80{vcPacked}, 70{vcCRC}, 30{vcAttributes}, 28{vcEncrypted}, 60{vcMethod},
    300{vcPath});
var
  Col: TAbViewColumn;
  Column: TListColumn;
begin
  if HandleAllocated then
    Items.BeginUpdate;
  Columns.BeginUpdate;
  try
    Columns.Clear;
    for Col := Low(Col) to High(Col) do begin
      if not (Col in FVisibleColumns) then
        Continue;
      Column := Columns.Add;
      case Col of
        vcName: Column.Caption := AbItemNameHeadingS;
        vcFileType: Column.Caption := AbFileTypeHeadingS;
        vcLastModified: Column.Caption := AbLastModifiedHeadingS;
        vcSize: Column.Caption := AbFileSizeHeadingS;
        vcRatio: Column.Caption := AbRatioHeadingS;
        vcPacked: Column.Caption := AbPackedHeadingS;
        vcCRC: Column.Caption := AbCRCHeadingS;
        vcAttributes: Column.Caption := AbFileAttrHeadingS;
        vcEncrypted: Column.ImageIndex := 0;
        vcMethod: Column.Caption := AbMethodHeadingS;
        vcPath: Column.Caption := AbPathHeadingS;
      end;
      Column.Width := ColWidths[Col];
      Column.Tag := Ord(Col);
      if Col in [vcSize, vcRatio, vcPacked] then
        Column.Alignment := taRightJustify;
    end;
  finally
    Columns.EndUpdate;
    if HandleAllocated then
      Items.EndUpdate;
  end;
  UpdateSortArrow;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.UpdateSortArrow;
var
  i: Integer;
  Item: THDITEM;
begin
  if not HandleAllocated then
    Exit;
  FInUpdateSortArrows := True;
  try
    for i := 0 to Columns.Count - 1 do begin
      FillChar(Item, SizeOf(Item), 0);
      Item.Mask := HDI_FORMAT;
      if not IsComCtl32Version6 then
        Item.Mask := Item.Mask or HDI_BITMAP;
      Header_GetItem(FHeaderHandle, Columns[i].Index, Item);
      // Add sort arrow to requested column
      if TAbViewColumn(Columns[i].Tag) = FSortColumn then begin
        FSortColIndex := i - 1;
        if IsComCtl32Version6 then begin
          Item.fmt := Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
          if FSortAscending then
            Item.fmt := Item.fmt or HDF_SORTUP
          else
            Item.fmt := Item.fmt or HDF_SORTDOWN;
        end
        else begin
          Item.fmt := Item.fmt or HDF_BITMAP or HDF_BITMAP_ON_RIGHT;
          if FSortAscending then
            Item.hbm := FSortUpBmp
          else
            Item.hbm := FSortDownBmp;
        end;
      end
      // Remove sort arrow from other columns
      else begin
        if IsComCtl32Version6 then
          Item.fmt := Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP)
        else begin
          Item.Mask := Item.Mask and not HDI_BITMAP;
          Item.fmt := Item.fmt and not (HDF_BITMAP OR HDF_BITMAP_ON_RIGHT);
        end;
      end;
      Header_SetItem(FHeaderHandle, Columns[i].Index, Item);
    end;
  finally
    FInUpdateSortArrows := False;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomListView.UpdateView;
var
  ArcItem: TAbArchiveItem;
  Col: TAbViewColumn;
  ColImage: Integer;
  ColText, Filename, FolderName: string;
  DOSAttr: Integer;
  Folders: TStringList;
  i, j: Integer;
  ListItem: TAbListItem;
  ParentDir: string;
  sfi: SHFILEINFO;
begin
//  ListItem := nil; // Suppress compiler warning
  if (Items.Count = 0) and (FArchive = nil) then
    Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      Folders := TStringList.Create;
      try
        for i := 0 to FArchive.Count - 1 do
          if FArchive[i].Action <> aaDelete then begin
            ArcItem := FArchive[i];
            Filename := AbNormalizeFilename(ArcItem.FileName);
            // Exclude unwanted items
            if FlatList and ArcItem.IsDirectory then
              Continue;
            // Create new ListItem
            ParentDir := ExtractFileDir(FileName);
            if FlatList or (ParentDir = Path) then begin
              // If an ListItem has already been created for a folder, use it
              if ArcItem.IsDirectory then begin
                FolderName := ExtractFileName(FileName);
                if Folders.Find(FolderName, j) then
                  ListItem := Folders.Objects[j] as TAbListItem
                else begin
                  ListItem := Items.Add as TAbListItem;
                  Folders.AddObject(FolderName, ListItem);
                end
              end
              else
                ListItem := Items.Add as TAbListItem;
              ListItem.ArchiveItem := FArchive[i];
            end
            else if (Path = '') or StartsText(Path + PathDelim, ParentDir) then begin
              // Create folder for implicitly stored directories,
              // if one hasn't been created already
              while ParentDir <> Path do begin
                FileName := ParentDir;
                ParentDir := ExtractFileDir(FileName);
              end;
              FolderName := ExtractFileName(FileName);
              if Folders.IndexOf(FolderName) <> -1 then
                Continue;
              ListItem := Items.Add as TAbListItem;
              Folders.AddObject(FolderName, ListItem);
              ArcItem := nil;
            end
            else
              // ListItem isn't below Path
              Continue;
            // Get file type information from the shell
            if ListItem.IsDirectory then
              DOSAttr := FILE_ATTRIBUTE_DIRECTORY
            else
              DOSAttr := FILE_ATTRIBUTE_NORMAL;
            SHGetFileInfo(PChar(ExtractFileName(Filename)), DOSAttr, sfi, sizeof(sfi),
              SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
            // Fill in columns
            ListItem.Caption := ExtractFileName(Filename);
            ListItem.ImageIndex := sfi.iIcon;
            ListItem.SubItems.Clear;
            for Col := Succ(Low(Col)) to High(Col) do
              if Col in FVisibleColumns then begin
                ColText := '';
                ColImage := -1;
                case Col of
                  vcFileType:
                    ColText := sfi.szTypeName;
                  vcLastModified:
                    if ArcItem <> nil then
                      ColText := DateToStr(ArcItem.LastModTimeAsDateTime) + ' ' +
                        TimeToStr(ArcItem.LastModTimeAsDateTime);
                  vcSize:
                    if not ListItem.IsDirectory then
                      ColText := FormatFloat('#,##0', ArcItem.UncompressedSize);
                  vcRatio:
                    if not ListItem.IsDirectory then
                      if ArcItem.UncompressedSize > 0 then
                        ColText := Format('%d%%',
                          [100 - Round(ArcItem.CompressedSize * 100 / ArcItem.UncompressedSize)])
                      else
                        ColText := '0%';
                  vcPacked:
                    if not ListItem.IsDirectory then
                      ColText := FormatFloat('#,##0', ArcItem.CompressedSize);
                  vcCRC:
                    if not ListItem.IsDirectory then
                      ColText := IntToHex(ArcItem.CRC32, 8);
                  vcAttributes:
                    if ArcItem <> nil then begin
                      {$WARN SYMBOL_PLATFORM OFF}
                      if (faReadOnly and ArcItem.ExternalFileAttributes) = faReadOnly then
                        ColText := ColText + AbReadOnlyS;
                      if (faHidden and ArcItem.ExternalFileAttributes) = faHidden then
                        ColText := ColText + AbHiddenS;
                      if (faSysFile and ArcItem.ExternalFileAttributes) = faSysFile then
                        ColText := ColText + AbSystemS;
                      if (faArchive and ArcItem.ExternalFileAttributes) = faArchive then
                        ColText := ColText + AbArchivedS;
                      {$WARN SYMBOL_PLATFORM ON}
                    end;
                  vcMethod:
                    if ArcItem is TAbZipItem then
                      ColText := ZipCompressionMethodToString(
                        TAbZipItem(ArcItem).CompressionMethod);
                  vcPath:
                    ColText := ExtractFileDir(FileName);
                end;
                ListItem.SubItems.Add(ColText);
                ListItem.SubItemImages[ListItem.SubItems.Count - 1] := ColImage;
              end;
          end;
        finally
          Folders.Free;
        end;
      CustomSort(TLVCompare(@TAbCustomListView_SortProc), LPARAM(Self));
    end;
  finally
    Items.EndUpdate;
  end;
end;


{ ===== TAbCustomTreeView =================================================== }
constructor TAbCustomTreeView.Create(aOwner: TComponent);
var
  Bmp : TBitmap;
  Icon : TIcon;
  sfi: SHFILEINFO;
begin
  inherited;
  HideSelection := False;
  Images := TImageList.Create(Self);
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, 'AbComCtrls_Zip');
    Images.AddMasked(Bmp, clFuchsia);
    Icon := TIcon.Create;
    try
      // On Windows 7 an empty filename returns the drive icon instead of a folder
      SHGetFileInfo('Folder', FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
        SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
      Icon.Handle := sfi.hIcon;
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Brush.Color := clWindow;
      Bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
      Bmp.Canvas.Draw(0, 0, Icon);
      Images.AddMasked(Bmp, clWindow);
      SHGetFileInfo('Folder', FILE_ATTRIBUTE_DIRECTORY, sfi, sizeof(sfi),
        SHGFI_ICON or SHGFI_OPENICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES);
      Icon.Handle := sfi.hIcon;
      Bmp.Canvas.FillRect(Rect(0, 0, 16, 16));
      Bmp.Canvas.Draw(0, 0, Icon);
      Images.AddMasked(Bmp, clWindow);
    finally
      Icon.Free;
    end;
  finally
    Bmp.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.Change(aNode: TTreeNode);
var
  Filename: string;
begin
  inherited;
  if aNode.Selected then begin
    Filename := '';
    if aNode <> Items.GetFirstNode then begin
      Filename := aNode.Text;
      aNode := aNode.Parent;
      while aNode <> Items.GetFirstNode do begin
        Filename := aNode.Text + PathDelim + Filename;
        aNode := aNode.Parent;
      end;
    end;
    Path := Filename;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.DoChange(Sender: TObject);
var
  Nodes: TStringList;
  ZipNode: TTreeNode;

  function GetNode(const aFilename: string): TTreeNode;
  var
    i: Integer;
  begin
    if aFilename = '' then
      Result := ZipNode
    else if Nodes.Find(aFilename, i) then
      Result := TTreeNode(Nodes.Objects[i])
    else begin
      Result := Items.AddChild(GetNode(ExtractFileDir(aFilename)), ExtractFileName(aFilename));
      Result.ExpandedImageIndex := AbTreeFolderExpandedImage;
      Result.ImageIndex := AbTreeFolderImage;
      Nodes.AddObject(aFilename, Result);
    end;
  end;

var
  i: Integer;
  Filename: string;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if Assigned(FArchive) then begin
      Nodes := TStringList.Create;
      try
        Nodes.Sorted := True;
        if Archive.FArchive <> nil then
          Filename := ExtractFileName(Archive.FArchive.ArchiveName)
        else
          Filename := PathDelim;
        ZipNode := Items.AddChild(nil, Filename);
        ZipNode.ExpandedImageIndex := AbTreeArchiveImage;
        ZipNode.ImageIndex := AbTreeArchiveImage;
        for i := 0 to FArchive.Count - 1 do
          if FArchive[i].Action <> aaDelete then begin
            Filename := AbNormalizeFilename(FArchive[i].FileName);
            if not FArchive[i].IsDirectory then
              Filename := ExtractFileDir(Filename);
            GetNode(Filename);
          end;
      finally
        Nodes.Free;
      end;
      Items.AlphaSort(True);
      ZipNode.Expand(False);
      SelectPathNode;
    end;
  finally
    Items.EndUpdate;
  end;
  if (Sender = FArchive) and Assigned(FListView) then
    FListView.DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.GetSelectedIndex(aNode: TTreeNode);
begin
  if aNode.Expanded then
    aNode.SelectedIndex := aNode.ExpandedImageIndex
  else
    aNode.SelectedIndex := aNode.ImageIndex;
end;
{ -------------------------------------------------------------------------- }
function TAbCustomTreeView.GetVersion: string;
begin
  Result := AbVersionS;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.Notification(aComponent: TComponent;
  aOperation: TOperation);
begin
  inherited;
  if aOperation = opRemove then begin
    if aComponent = FArchive then begin
      FArchive := nil;
      Items.Clear;
    end;
    if aComponent = FListView then begin
      if Assigned(FArchive) and SameEvent(FArchive.OnChange, FListView.DoChange) then
        FArchive.OnChange := DoChange;
      FListView := nil;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SelectPathNode;
var
  Filename, Remaining: string;
  i: Integer;
  Node: TTreeNode;
begin
  // Find selected node, expanding parents along the way
  Node := Items.GetFirstNode;
  Remaining := FPath;
  if StartsText(PathDelim, Remaining) then
    System.Delete(Remaining, 1, 1);
  while Remaining <> '' do begin
    Node.Expand(False);
    i := Pos(PathDelim, Remaining);
    if i = 0 then
      i := Length(Remaining) + 1;
    Filename := Copy(Remaining, 1, i - 1);
    Remaining := Copy(Remaining, i + 1, MaxInt);
    if Filename = '' then
      Continue;
    Node := Node.getFirstChild;
    while (Node <> nil) and not SameText(Filename, Node.Text) do
      Node := Node.getNextSibling;
    if Node = nil then begin
      Node := Items.GetFirstNode;
      Break;
    end;
  end;
  Selected := Node;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SetArchive(aValue: TAbBaseBrowser);
begin
  if aValue <> FArchive then begin
    if Assigned(FArchive) then begin
      FArchive.RemoveFreeNotification(Self);
      if SameEvent(FArchive.OnChange, DoChange) then
        if Assigned(ListView) and (ListView.Archive = FArchive) then
          FArchive.OnChange := ListView.DoChange
        else
          FArchive.OnChange := nil;
    end;
    FArchive := aValue;
    if Assigned(FArchive) then begin
      FArchive.FreeNotification(Self);
      FArchive.OnChange := DoChange;
      DoChange(Self);
    end
    else
      Items.Clear;
    if Assigned(ListView) then
      ListView.Archive := aValue;
    SelectPathNode;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SetListView(aValue: TAbCustomListView);
begin
  if aValue <> FListView then begin
    if Assigned(FListView) then begin
      FListView.RemoveFreeNotification(Self);
      FListView.TreeView := nil;
    end;
    FListView := aValue;
    if Assigned(FListView) then begin
      FListView.FreeNotification(Self);
      if Assigned(FArchive) then
        FListView.Archive := FArchive
      else if Assigned(FListView.Archive) then
        Archive := FListView.Archive;
      FListView.TreeView := Self;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomTreeView.SetPath(const aValue: string);
begin
  if FPath <> aValue then begin
    FPath := ExcludeTrailingPathDelimiter(aValue);
    SelectPathNode;
    if Assigned(FListView) then
      FListView.Path := aValue;
  end;
end;


{ ===== TAbProgressBar ====================================================== }
procedure TAbProgressBar.DoProgress(Progress : Byte);
begin
  Position := Progress;
  Application.ProcessMessages;
end;
{ -------------------------------------------------------------------------- }
function  TAbProgressBar.GetVersion : string;
begin
  Result := AbVersionS;
end;
{ -------------------------------------------------------------------------- }
procedure TAbProgressBar.Reset;
begin
  DoProgress(0);
end;

end.
