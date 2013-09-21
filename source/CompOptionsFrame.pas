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

unit CompOptionsFrame;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ValEdit, ComCtrls, ExtCtrls, CompOptionsList, project, utils, prjtypes, StdCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QGrids, QComCtrls, QExtCtrls, project, prjtypes;
{$ENDIF}

type
  TCompOptionsFrame = class(TFrame)
    tabs: TTabControl;
    vle: TCompOptionsList;
    procedure tabsChange(Sender: TObject);
    procedure vleSetEditText(Sender: TObject; ACol, ARow: Integer;const Value: String);
  private
    { Private declarations }
    fProject: TProject;
  public
    { Public declarations }
    procedure FillOptions(Proj: TProject);
  end;

implementation

uses 
  devcfg, multilangsupport;

{$R *.dfm}

{ TCompOptionsFrame }

procedure TCompOptionsFrame.FillOptions(Proj: TProject);
var
	I : integer;
begin
	fProject := Proj;

	for I := 0 to devCompiler.fOptionList.Count - 1 do
		if tabs.Tabs.IndexOf(Lang[PCompilerOption(devCompiler.fOptionList[I])^.optSection]) = -1 then
			tabs.Tabs.Add(Lang[PCompilerOption(devCompiler.fOptionList[I])^.optSection]);

	tabsChange(nil);
end;

procedure TCompOptionsFrame.tabsChange(Sender: TObject);
var
	I,J,idx : integer;
	currenttab : AnsiString;
	option : TCompilerOption;
begin
	vle.OnSetEditText := nil;

	vle.Strings.BeginUpdate;
	vle.Strings.Clear;

	currenttab := tabs.Tabs[tabs.TabIndex];

	for I := 0 to devCompiler.fOptionList.Count - 1 do begin
		option := PCompilerOption(devCompiler.fOptionList[I])^;
		if SameStr(Lang[option.optSection], currenttab) then begin
			if Assigned(option.optChoices) and (option.optValue < option.optChoices.Count) then
				idx := vle.InsertRow(Lang[option.optName], option.optChoices.Names[option.optValue], True) // a,b,c,d
			else
				idx := vle.InsertRow(Lang[option.optName], BoolValYesNo[option.optValue > 0], True); // No Yes

			vle.Strings.Objects[idx] := Pointer(I);
			vle.ItemProps[idx].EditStyle := esPickList;
			vle.ItemProps[idx].ReadOnly := true;
			if Assigned(option.optChoices) then begin
				for j := 0 to option.optChoices.Count - 1 do
					vle.ItemProps[idx].PickList.Add(option.optChoices.Names[J]);
			end else begin
				vle.ItemProps[idx].PickList.Add(BoolValYesNo[False]);
				vle.ItemProps[idx].PickList.Add(BoolValYesNo[True]);
			end;
		end;
	end;
	vle.ColWidths[0] := vle.ClientWidth - 90;
	vle.ColWidths[1] := 90;
	vle.OnSetEditText := vleSetEditText;

	vle.Strings.EndUpdate;
end;

procedure TCompOptionsFrame.vleSetEditText(Sender: TObject; ACol,ARow: Integer; const Value: String);
var
	option : PCompilerOption;
	I: integer;
begin
	option := PCompilerOption(devCompiler.fOptionList[Integer(vle.Strings.Objects[ARow])]);

	if SameStr(Value,'Yes') then
		option^.optValue := 1
	else if SameStr(Value,'No') then
		option^.optValue := 0
	else if Assigned(option^.optChoices) then begin
		for i := 0 to option^.optChoices.Count - 1 do
			if SameStr(Value,option^.optChoices.Names[i]) then begin
				option^.optValue := i;
				break;
			end;
	end;

	// update string too
	devCompiler.SetOption(option,Integer(vle.Strings.Objects[ARow]),ValueToChar[option^.optValue]);
end;

end.

