{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit CompOptionsList;

interface

uses
  SysUtils, Classes, StdCtrls, Windows, Controls, Grids, ValEdit, Themes, Types;

type
  TInplaceEditListAccess = class(Grids.TInplaceEditList);

  TCompOptionsList = class(TValueListEditor)
  private
    procedure DrawDropDownButton(ACol, ARow: Integer; ARect: TRect;AState: TGridDrawState);
    function MouseOverButton(X: Integer): Boolean;
  protected
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('Dev-C++', [TCompOptionsList]);
end;

procedure TCompOptionsList.DrawCell(ACol, ARow: Integer; ARect: TRect;AState: TGridDrawState);
begin
	inherited DrawCell(ACol, ARow, ARect, AState);
	DrawDropDownButton(ACol, ARow, ARect, AState);
end;

procedure TCompOptionsList.DrawDropDownButton(ACol, ARow: Integer;ARect: TRect; AState: TGridDrawState);
var
	Details: TThemedElementDetails;
begin
	if Assigned(EditList) and (ACol = 1) and (ARow >= FixedRows) and not (gdFocused in AState) and ItemProps[ARow - FixedRows].HasPickList then begin
		ARect.Left := ARect.Right - EditList.ButtonWidth;
		Details := ThemeServices.GetElementDetails(tcDropDownButtonNormal);
		ThemeServices.DrawElement(Canvas.Handle, Details, ARect);
	end;
end;

procedure TCompOptionsList.MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
var
	ACol: Integer;
	ARow: Integer;
begin
	inherited MouseDown(Button, Shift, X, Y);
	if Assigned(EditList) then begin
		MouseToCell(X, Y, ACol, ARow);
		if (Button = mbLeft) and (ARow > FixedRows) and
			ItemProps[ARow - FixedRows].HasPickList and
			not EditList.ListVisible and MouseOverButton(X) then
			begin
			EditorMode := True;
			TInplaceEditListAccess(EditList).DropDown;
		end;
	end;
end;

function TCompOptionsList.MouseOverButton(X: Integer): Boolean;
begin
	if Assigned(EditList) then
		Result := (UseRightToLeftAlignment and (X < EditList.ButtonWidth)) or
				  (not UseRightToLeftAlignment and (X > ClientWidth - EditList.ButtonWidth))
	else
		Result := false; // TODO: make one if?
end;

end.
