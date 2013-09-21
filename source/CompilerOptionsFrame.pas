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
  Grids, ValEdit, ComCtrls, ExtCtrls, project, prjtypes;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QGrids, QComCtrls, QExtCtrls, project, prjtypes;
{$ENDIF}

type
  TCompOptionsFrame = class(TFrame)
    tv: TTreeView;
    vle: TValueListEditor;
    Splitter1: TSplitter;
    procedure tvChange(Sender: TObject; Node: TTreeNode);
    procedure vleSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure FrameResize(Sender: TObject);
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
  function FindNode(ParentNode: TTreeNode; Text: string): TTreeNode;
  var
    I: integer;
  begin
    Result := nil;
    if not Assigned(ParentNode) then begin
      for I := 0 to tv.Items.Count - 1 do
        if AnsiCompareText(tv.Items.Item[I].Text, Text) = 0 then begin
          Result := tv.Items.Item[I];
          Break;
        end;
    end
    else
      for I := 0 to ParentNode.Count - 1 do
        if AnsiCompareText(ParentNode.Item[I].Text, Text) = 0 then begin
          Result := ParentNode.Item[I];
          Break;
        end;
  end;
  procedure CreateSectionNode(CompilersNode: TTreeNode; NodePath: string);
  var
    s, s1: string;
    idx: integer;
    tmpNode, Node: TTreeNode;
  begin
    if NodePath = '' then Exit;
    Node := CompilersNode;
    s := NodePath;
    repeat
      s1 := s;
      idx := Pos('/', s);
      if idx > 0 then begin
        s1 := Copy(s, 1, idx - 1);
        Delete(s, 1, idx);
      end;
      tmpNode := FindNode(Node, s1);
      if Assigned(tmpNode) then
        Node := tmpNode
      else
        Node := tv.Items.AddChild(Node, s1);
    until idx = 0;
  end;
var
  I: integer;
begin
  fProject := Proj;
  tv.Items.Clear;
  for I := 0 to devCompiler.OptionsCount - 1 do
    CreateSectionNode(nil, devCompiler.Options[I].optSection);
{$IFDEF WIN32}
  tv.AlphaSort(True);
{$ENDIF}
{$IFDEF LINUX}
  tv.AlphaSort();
  {$MESSAGE 'check if AlphaSort(True) is already available'}
{$ENDIF}
  if tv.Items.Count > 0 then
    tv.Selected := tv.Items.Item[0];
end;

procedure TCompOptionsFrame.tvChange(Sender: TObject; Node: TTreeNode);
  function SectionPath(childNode: TTreeNode): string;
  begin
    Result := '';
    while Assigned(childNode) do begin
      Result := childNode.Text + '/' + Result;
      childNode := childNode.Parent;
    end;
    if Length(Result) > 0 then
      Delete(Result, Length(Result), 1);
  end;
var
  I, J, idx: integer;
  NodePath: string;
  ShowOption: boolean;
begin
  if not Assigned(Node) then
    Exit;

  vle.OnSetEditText := nil;
  vle.Strings.Clear;

  NodePath := SectionPath(Node);
  for I := 0 to devCompiler.OptionsCount - 1 do begin
    ShowOption := (not Assigned(fProject)) or (Assigned(fProject) and not (fProject.Options.typ in devCompiler.Options[I].optExcludeFromTypes));
    if ShowOption and (AnsiCompareText(devCompiler.Options[I].optSection, NodePath) = 0) then begin
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
      end
      else begin
        vle.ItemProps[idx].PickList.Add(BoolValYesNo[False]);
        vle.ItemProps[idx].PickList.Add(BoolValYesNo[True]);
      end;
    end;
  end;
  vle.ColWidths[0] := vle.ClientWidth - 64;
  vle.OnSetEditText := vleSetEditText;
end;

procedure TCompOptionsFrame.vleSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
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
    if i = opt.optChoices.Count then
      exit;
  end;

  devCompiler.Options[Integer(vle.Strings.Objects[ARow])] := opt;

  if opt.optValue > 0 then
    if opt.optIsGroup then begin
      for I := 0 to devCompiler.OptionsCount - 1 do
        if (I <> Integer(vle.Strings.Objects[ARow])) and
          (devCompiler.Options[I].optSection = opt.optSection) then begin
          opt1 := devCompiler.Options[I];
          opt1.optValue := 0;
          devCompiler.Options[I] := opt1;
        end;
      tvChange(tv, tv.Selected);
      vle.Row := ARow;
    end;
end;

procedure TCompOptionsFrame.FrameResize(Sender: TObject);
begin
  vle.ColWidths[0] := vle.ClientWidth - 64;
end;

end.

