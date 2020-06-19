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
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbCView.pas                                 *}
{*********************************************************}
{* ABBREVIA: Cabinet archive viewer component            *}
{*   Use AbQCView.pas for CLX                            *}
{*********************************************************}

Unit AbCView;

{$I AbDefine.inc}

interface

uses
  Windows, Classes, AbView, AbCBrows, AbCabTyp, AbArcTyp;

type
  TAbCabView = class(TAbBaseViewer)
  protected
    FCabComponent : TAbCustomCabBrowser;
    FEmptyItemList: TAbArchiveList;
    function GetItem(RowNum : Integer) : TAbCabItem;
    procedure SetCabComponent(Value : TAbCustomCabBrowser);
    procedure Notification(AComponent : TComponent; Operation : TOperation);
      override;
    procedure DoChange(Sender : TObject);
      override;
  public
    constructor Create(AOwner : TComponent);
      override;
    destructor  Destroy;
      override;
    property Items[RowNum : Integer] : TAbCabItem
      read GetItem;
  published {properties}
    property Align;
    property Attributes;
    property BorderStyle;
    property Color;
    property Colors;
    property Ctl3D;
    property Cursor;
    property Headings;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DisplayOptions;
    property HeaderRowHeight;
    property SortAttributes;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Version;
    property CabComponent : TAbCustomCabBrowser
      read FCabComponent write SetCabComponent;
  published {Events}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSorted;
    property OnDrawSortArrow;
end;


implementation

type
  TAbCabBrowserFriend = class(TAbCustomCabBrowser);


{ ===== TAbCabView ========================================================= }
constructor TAbCabView.Create(AOwner : TComponent);
begin
  inherited;
  FEmptyItemList := FItemList;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCabView.Destroy;
begin
  FItemList := FEmptyItemList;
  inherited;
end;
{ -------------------------------------------------------------------------- }
function TAbCabView.GetItem(RowNum : Integer) : TAbCabItem;
begin
  if Assigned(FItemList) then
    Result := TAbCabItem(FItemList.Items[FRowMap[RowNum]])
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabView.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if Assigned(FCabComponent) and (AComponent = FCabComponent) then begin
      FCabComponent := nil;
      Refresh;
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabView.SetCabComponent(Value : TAbCustomCabBrowser);
begin
  FCabComponent := Value;
  FCabComponent.OnChange := DoChange;
  FCabComponent.OnLoad := DoLoad;
  DoChange(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabView.DoChange(Sender : TObject);
begin
  if Assigned(FCabComponent) and Assigned(TAbCabBrowserFriend(FCabComponent).Archive) then
    FItemList := TAbCabBrowserFriend(FCabComponent).Archive.ItemList
  else if FEmptyItemList <> nil then
    FItemList := FEmptyItemList;
  inherited DoChange(Sender);
end;

end.

