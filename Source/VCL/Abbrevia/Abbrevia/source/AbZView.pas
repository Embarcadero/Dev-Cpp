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
{* ABBREVIA: AbZView.pas                                 *}
{*********************************************************}
{* ABBREVIA: Zip archive viewer component                *}
{*   Use AbQZView.pas for CLX                            *}
{*********************************************************}

unit AbZView;

{$I AbDefine.inc}

interface

uses
  Classes,
  Windows,
  Controls,
  AbView,
  AbZBrows,
  AbZipTyp;

type
  TAbIncludeItemEvent = procedure (Sender:  TObject;
                                   Item:    TAbZipItem;
                               var Include: Boolean) of object;

  TAbZipView = class(TAbBaseViewer)
  protected
    FZipComponent : TAbCustomZipBrowser;
    FOnIncludeItem: TAbIncludeItemEvent;

    function GetItem(RowNum : Integer) : TAbZipItem;
    procedure SetZipComponent(Value : TAbCustomZipBrowser);
    procedure Notification(AComponent : TComponent; Operation : TOperation);
      override;
    procedure DoChange(Sender : TObject);
      override;
  public
    property Items[RowNum : Integer] : TAbZipItem
      read GetItem;
  published {properties}
    property Align;
    property Anchors;
    property Attributes;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property Color;
    property Colors;
    property Ctl3D;
    property ParentCtl3D;
    property DragCursor;
    property Cursor;
    property Headings;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DisplayOptions;
    property HeaderRowHeight;
    property SortAttributes;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property Font;
    property GradientEndColor;
    property GradientStartColor;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Version;
    property ZipComponent : TAbCustomZipBrowser read FZipComponent write SetZipComponent;
  published {Events}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnSorted;
    property OnDrawSortArrow;

    property OnIncludeItem: TAbIncludeItemEvent
      read FOnIncludeItem
      write FOnIncludeItem;
end;

implementation

uses
  AbArcTyp;

{ ===== TAbZipView ========================================================= }
function TAbZipView.GetItem(RowNum : Integer) : TAbZipItem;
begin
  if Assigned(FItemList) then
    Result := TAbZipItem(FItemList.Items[FRowMap[RowNum]])
  else
    Result := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipView.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if Assigned(FZipComponent) and (AComponent = FZipComponent) then begin
      FZipComponent := nil;
      Refresh;
    end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipView.SetZipComponent(Value : TAbCustomZipBrowser);
begin
  if Value <> nil then begin
    FZipComponent := Value;

    if not (csDesigning in ComponentState) then begin
      FZipComponent.OnChange := DoChange;
      FZipComponent.OnLoad := DoLoad;
      DoChange(Self);
    end;
  end
  else
    FZipComponent := nil;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipView.DoChange(Sender : TObject);
var
  i : Integer;
  TheArchive : TAbArchive;
  Include : Boolean;
begin
  FItemList.Clear;
  if Assigned(FZipComponent) then begin
    { let's make this a bit easier to read }
    TheArchive := FZipComponent.FArchive;

    if Assigned(TheArchive) then begin
      for i := 0 to Pred(TheArchive.ItemList.Count) do begin
        if Assigned(FOnIncludeItem) then begin
          FOnIncludeItem(self, TAbZipItem(TheArchive.ItemList[i]), Include);
          if Include then
            FItemList.Add(TheArchive.ItemList[i]);
        end
        else begin
        { if it doesn't look like a folder place holder... }
        if TAbZipItem(TheArchive.ItemList[i]).DiskFileName <>
           TAbZipItem(TheArchive.ItemList[i]).DiskPath then
            { ...add it to the display list }
            FItemList.Add(TheArchive.ItemList[i]);
        end;
      end;
    end
    else
      FItemList.Clear;
  end
  else
    FItemList.Clear;
  inherited DoChange(Sender);
end;

end.

