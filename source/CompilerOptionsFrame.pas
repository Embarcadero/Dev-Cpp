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

unit CompilerOptionsFrame;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ValEdit, ComCtrls, ExtCtrls, project, utils, prjtypes, StdCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QGrids, QComCtrls, QExtCtrls, project, prjtypes;
{$ENDIF}

type
  TCompOptionsFrame = class(TFrame)
    tabs: TTabControl;
    vle: TValueListEditor;
    procedure tabsChange(Sender: TObject);
    procedure vleSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    { Private declarations }
    fProject: TProject;
  public
    { Public declarations }
    procedure FillOptions(Proj: TProject);
  end;

implementation

uses 
  devcfg;

{$R *.dfm}

{ TCompOptionsFrame }

procedure TCompOptionsFrame.FillOptions(Proj: TProject);
var
	I : integer;
begin
	fProject := Proj;

	for I := 0 to devCompiler.OptionsCount - 1 do
		if tabs.Tabs.IndexOf(devCompiler.Options[I].optSection) = -1 then
			tabs.Tabs.Add(devCompiler.Options[I].optSection);

	tabsChange(nil);
end;

procedure TCompOptionsFrame.tabsChange(Sender: TObject);
var
	I,J,idx : integer;
	currenttab : AnsiString;
begin
	vle.Strings.Clear;

	currenttab := tabs.Tabs[tabs.TabIndex];

	for I := 0 to devCompiler.OptionsCount - 1 do begin
		if SameStr(devCompiler.Options[I].optSection, currenttab) then begin
			if Assigned(devCompiler.Options[I].optChoices) and (devCompiler.Options[I].optValue < devCompiler.Options[I].optChoices.Count) then
				idx := vle.InsertRow(devCompiler.Options[I].optName, devCompiler.Options[I].optChoices.Names[devCompiler.Options[I].optValue], True)
			else
				idx := vle.InsertRow(devCompiler.Options[I].optName, BoolValYesNo[devCompiler.Options[I].optValue > 0], True);

			vle.Strings.Objects[idx] := Pointer(I);
			vle.ItemProps[idx].EditStyle := esPickList;
			vle.ItemProps[idx].ReadOnly := True;
			if Assigned(devCompiler.Options[I].optChoices) then begin
				for j := 0 to devCompiler.Options[I].optChoices.Count - 1 do
					vle.ItemProps[idx].PickList.Add(devCompiler.Options[I].optChoices.Names[J]);
			end else begin
				vle.ItemProps[idx].PickList.Add(BoolValYesNo[False]);
				vle.ItemProps[idx].PickList.Add(BoolValYesNo[True]);
			end;
		end;
	end;
	vle.ColWidths[0] := vle.ClientWidth - 90;
end;

procedure TCompOptionsFrame.vleSetEditText(Sender: TObject; ACol,ARow: Integer; const Value: String);
var
	opt, opt1: TCompilerOption;
	I: integer;
begin
	if (vle.Strings.Count = 0) then
		Exit;

	opt := devCompiler.Options[Integer(vle.Strings.Objects[ARow])];

	if Value = 'Yes' then
		opt.optValue := 1  // True
	else if Value = 'No' then
		opt.optValue := 0  //False
	else if opt.optChoices = nil then
		Exit
	else begin
		for i := 0 to opt.optChoices.Count - 1 do
			if Value = opt.optChoices.Names[i] then begin
				opt.optValue := i;
				break;
			end;
	end;

	devCompiler.Options[Integer(vle.Strings.Objects[ARow])] := opt;

	if opt.optValue > 0 then
		if opt.optIsGroup then begin
			for I := 0 to devCompiler.OptionsCount - 1 do
				if (I <> Integer(vle.Strings.Objects[ARow])) and (devCompiler.Options[I].optSection = opt.optSection) then begin
					opt1 := devCompiler.Options[I];
					opt1.optValue := 0;
					devCompiler.Options[I] := opt1;
				end;
			tabsChange(tabs);
			vle.Row := ARow;
		end;
end;

end.

