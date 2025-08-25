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

unit CodeCompletion;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, Forms, SysUtils, Controls, Graphics, CppParser,
  cbutils, IntList, StatementList;
{$ENDIF}
{$IFDEF LINUX}
Xlib, Classes, QForms, SysUtils, QControls, QGraphics, CppParser,
U_IntList, QDialogs, Types;
{$ENDIF}

type
  TCodeCompletion = class(TComponent)
  private
    fParser: TCppParser;
    fFullCompletionStatementList: TList;
    fCompletionStatementList: TList;
    fMinWidth: integer;
    fMinHeight: integer;
    fMaxWidth: integer;
    fMaxHeight: integer;
    fPos: TPoint;
    fColor: TColor;
    fWidth: integer;
    fHeight: integer;
    fEnabled: boolean;
    fShowCount: integer;
    fOnKeyPress: TKeyPressEvent;
    fOnResize: TNotifyEvent;
    fOnlyGlobals: boolean;
    fCurrentStatement: PStatement;
    fHideDelay: integer; // allow empty list once, then hide when empty again
    fIncludedFiles: TStringList;
    fIsIncludedCacheFileName: String;
    fIsIncludedCacheResult: boolean;
    function ApplyClassFilter(Statement, CurrentClass: PStatement; InheritanceStatements: TList): boolean;
    function ApplyMemberFilter(Statement, CurrentClass, ParentClass: PStatement; InheritanceStatements: TList): boolean;
    procedure GetCompletionFor(Phrase: String);
    procedure FilterList(const Member: String);
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsIncluded(const FileName: String): boolean;
    function IsVisible: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Search(const Phrase, Filename: String);
    procedure Hide;
    function SelectedStatement: PStatement;
    property CurrentStatement: PStatement read fCurrentStatement write fCurrentStatement;
  published
    property ShowCount: integer read fShowCount write fShowCount;
    property Parser: TCppParser read fParser write fParser;
    property Position: TPoint read fPos write SetPosition;
    property Color: TColor read fColor write fColor;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Enabled: boolean read fEnabled write fEnabled;
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write fOnKeyPress;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property OnlyGlobals: boolean read fOnlyGlobals write fOnlyGlobals;
    property Visible: boolean read IsVisible;
  end;

procedure Register;

implementation

uses
  CodeCompletionForm, Math;

{ TCodeCompletion }

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCodeCompletion]);
end;

constructor TCodeCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fIncludedFiles := TStringList.Create;
  fIncludedFiles.Sorted := True;
  fIncludedFiles.Duplicates := dupIgnore;

  fCompletionStatementList := TList.Create;
  fFullCompletionStatementList := TList.Create;

  CodeComplForm := TCodeComplForm.Create(Self);
  CodeComplForm.OnResize := OnFormResize;

  fWidth := 320;
  fHeight := 240;
  fColor := clWhite;
  fEnabled := True;
  fOnlyGlobals := False;
  fShowCount := 100; // keep things fast
  fHideDelay := 0;

  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(CodeComplForm);
  FreeAndNil(fCompletionStatementList);
  FreeAndNil(fFullCompletionStatementList);
  FreeAndNil(fIncludedFiles);
  inherited Destroy;
end;

function TCodeCompletion.ApplyClassFilter(Statement, CurrentClass: PStatement; InheritanceStatements: TList): boolean;
begin
  Result :=
    (
    (Statement^._Scope in [ssLocal, ssGlobal]) or // local or global var or
    (
    (Statement^._Scope = ssClassLocal) and // class var
    (
    (Statement^._Parent = CurrentClass) or // from current class
    (
    (InheritanceStatements.IndexOf(Statement^._Parent) <> -1) and
    (Statement^._ClassScope <> scsPrivate)
    ) // or an inheriting class
    )
    )
    ) and
    (IsIncluded(Statement^._FileName) or
    IsIncluded(Statement^._DefinitionFileName));
end;

function TCodeCompletion.ApplyMemberFilter(Statement, CurrentClass, ParentClass: PStatement; InheritanceStatements:
  TList): boolean;
var
  cs: set of TStatementClassScope;
begin
  Result := Statement^._Parent <> nil; // only members
  if not Result then
    Exit;

  // all members of current class
  Result := Result and ((ParentClass = CurrentClass) and (Statement^._Parent = ParentClass));

  // all public and published members of var's class
  Result := Result or
    (
    (ParentClass = Statement^._Parent) and
    (not (Statement^._ClassScope in [scsProtected, scsPrivate]))
    // or member of an inherited class
    );

  if (CurrentClass = nil) or (Statement^._Parent = CurrentClass) then
    cs := [scsPrivate, scsProtected]
  else
    cs := [scsPrivate];

  // all inherited class's non-private members
  Result := Result or
    (
    (InheritanceStatements.IndexOf(Statement^._Parent) <> -1) and
    (not (Statement^._ClassScope in cs)) // or member of an inherited class
    );
end;

procedure TCodeCompletion.GetCompletionFor(Phrase: String);
var
  Node: PStatementNode;
  Statement: PStatement;
  I: integer;
  InheritanceStatements: TList;
  ParentStatement, ParentTypeStatement, CurrentStatementParent: PStatement;
begin
  // Reset filter cache
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;

  // Pulling off the same trick as in TCppParser.FindStatementOf, but ignore everything after last operator
  InheritanceStatements := TList.Create;
  try
    I := fParser.FindLastOperator(Phrase);
    if I = 0 then begin

      // only add globals and members of the current class

      // Also consider classes the current class inherits from
      fParser.GetMultipleInheritanceStatements(fCurrentStatement, InheritanceStatements);
      Node := fParser.Statements.FirstNode;
      while Assigned(Node) do begin
        Statement := Node^.Data;
        if ApplyClassFilter(Statement, fCurrentStatement, InheritanceStatements) then
          fFullCompletionStatementList.Add(Statement);
        Node := Node^.NextNode;
      end;

    end else begin

      // Get parent of current statement
      if Assigned(fCurrentStatement) then
        CurrentStatementParent := fCurrentStatement^._Parent
      else
        CurrentStatementParent := nil;

      // Find last operator
      Delete(Phrase, I, MaxInt);

      // Add statements of all the text before the last operator
      ParentStatement := fParser.FindStatementOf(Phrase, fCurrentStatement);
      if not Assigned(ParentStatement) then
        Exit;

      // Then determine which type it has (so we can use it as a parent)
      if ParentStatement^._Kind = skClass then // already found type
        ParentTypeStatement := ParentStatement
      else
        ParentTypeStatement := fParser.FindTypeDefinitionOf(ParentStatement^._Type, CurrentStatementParent);
      // TODO: should not be nil
      if not Assigned(ParentTypeStatement) then
        Exit;

      // Then add members of the ClassIDs and InheritanceIDs
      fParser.GetMultipleInheritanceStatements(ParentTypeStatement, InheritanceStatements);

      Node := fParser.Statements.FirstNode;
      while Assigned(Node) do begin
        Statement := Node^.Data;
        if ApplyMemberFilter(Statement, fCurrentStatement, ParentTypeStatement, InheritanceStatements) then
          fFullCompletionStatementList.Add(Statement);
        Node := Node^.NextNode;
      end;
    end;
  finally
    InheritanceStatements.Free;
  end;
end;

// Return 1 to show Item2 above Item1, otherwise -1

function ListSort(Item1, Item2: Pointer): Integer;
var
  Statement1, Statement2: PStatement;
begin
  Statement1 := PStatement(Item1);
  Statement2 := PStatement(Item2);

  // Show stuff from local headers first
  if (Statement1^._InSystemHeader) and (not Statement2^._InSystemHeader) then begin
    Result := 1;
  end else if (not Statement1^._InSystemHeader) and (Statement2^._InSystemHeader) then begin
    Result := -1;

    // Show local statements first
  end else if (Statement1^._Scope in [ssGlobal]) and not (Statement2^._Scope in [ssGlobal]) then begin
    Result := 1;
  end else if not (Statement1^._Scope in [ssGlobal]) and (Statement2^._Scope in [ssGlobal]) then begin
    Result := -1;

    // otherwise, sort by name
  end else
    Result := CompareText(Statement1^._Command, Statement2^._Command);
end;

procedure TCodeCompletion.FilterList(const Member: String);
var
  I: integer;
begin
  fCompletionStatementList.Clear;
  if Member <> '' then begin // filter, case insensitive
    fCompletionStatementList.Capacity := fFullCompletionStatementList.Count;
    for I := 0 to fFullCompletionStatementList.Count - 1 do
      if StartsText(Member, PStatement(fFullCompletionStatementList[I])^._Command) then
        fCompletionStatementList.Add(fFullCompletionStatementList[I]);
  end else
    fCompletionStatementList.Assign(fFullCompletionStatementList);
  fCompletionStatementList.Sort(@ListSort);
end;

procedure TCodeCompletion.Hide;
begin
  OnKeyPress := nil;
  CodeComplForm.Hide;

  // Clear data, do not free pointed memory: data is owned by CppParser
  fCompletionStatementList.Clear;
  fFullCompletionStatementList.Clear;
  CodeComplForm.lbCompletion.Items.BeginUpdate;
  CodeComplForm.lbCompletion.Items.Clear;
  CodeComplForm.lbCompletion.Items.EndUpdate;
  fIncludedFiles.Clear; // is recreated anyway on reshow, so save some memory when hiding
end;

procedure TCodeCompletion.Search(const Phrase, Filename: String);
var
  I: integer;
begin
  if fEnabled then begin

    Screen.Cursor := crHourglass;

    // only perform full new search if just invoked
    if not CodeComplForm.Showing then begin
      fParser.GetFileIncludes(Filename, fIncludedFiles);
      GetCompletionFor(Phrase);
    end;

    // Sort here by member
    I := fParser.FindLastOperator(Phrase);
    while (I > 0) and (I <= Length(Phrase)) and CharInSet(Phrase[i], ['.', ':', '-', '>']) do
      Inc(I);

    // filter fFullCompletionStatementList to fCompletionStatementList
    FilterList(Copy(Phrase, I, MaxInt));

    if fCompletionStatementList.Count > 0 then begin
      CodeComplForm.lbCompletion.Items.BeginUpdate;
      try
        CodeComplForm.lbCompletion.Items.Clear;

        // Only slow one hundred statements...
        for I := 0 to min(fShowCount, fCompletionStatementList.Count - 1) do
          CodeComplForm.lbCompletion.Items.AddObject('', fCompletionStatementList[I]);
      finally
        CodeComplForm.lbCompletion.Items.EndUpdate;
      end;

      CodeComplForm.Show;
      CodeComplForm.lbCompletion.SetFocus;
      if CodeComplForm.lbCompletion.Items.Count > 0 then
        CodeComplForm.lbCompletion.ItemIndex := 0;

      fHideDelay := 0;
    end else if fHideDelay = 0 then begin
      CodeComplForm.lbCompletion.Items.Clear;
      fHideDelay := 1;
    end else begin
      Hide;
    end;

    Screen.Cursor := crDefault;
  end;
end;

function TCodeCompletion.SelectedStatement: PStatement;
begin
  if fEnabled then begin
    if (fCompletionStatementList.Count > CodeComplForm.lbCompletion.ItemIndex) and (CodeComplForm.lbCompletion.ItemIndex
      <> -1) then
      Result := PStatement(fCompletionStatementList[CodeComplForm.lbCompletion.ItemIndex])
    else begin
      if fCompletionStatementList.Count > 0 then
        Result := PStatement(fCompletionStatementList[0])
      else
        Result := nil;
    end;
  end else
    Result := nil;
end;

procedure TCodeCompletion.SetPosition(Value: TPoint);
begin
  fPos := Value;
  if fPos.X + fWidth > Screen.Width then
    CodeComplForm.Left := fPos.X - fWidth
  else
    CodeComplForm.Left := fPos.X;
  if fPos.Y + fHeight > Screen.Height then
    CodeComplForm.Top := fPos.Y - fHeight - 16
  else
    CodeComplForm.Top := fPos.Y;
end;

procedure TCodeCompletion.OnFormResize(Sender: TObject);
begin
  if Enabled then begin
    fWidth := CodeComplForm.Width;
    fHeight := CodeComplForm.Height;
    if Assigned(fOnResize) then
      fOnResize(Self);
  end;
end;

function TCodeCompletion.IsIncluded(const FileName: String): boolean;
begin
  // Only do the slow check if the cache is invalid
  if not SameStr(FileName, fIsIncludedCacheFileName) then begin
    fIsIncludedCacheFileName := FileName;
    fIsIncludedCacheResult := FastIndexOf(fIncludedFiles, FileName) <> -1;
  end;

  // Cache has been updated. Use it.
  Result := fIsIncludedCacheResult;
end;

function TCodeCompletion.IsVisible: boolean;
begin
  Result := fEnabled and CodeComplForm.Visible;
end;

end.


