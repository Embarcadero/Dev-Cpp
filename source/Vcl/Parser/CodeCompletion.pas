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
{

    History:
    
      23 May 2004 - Peter Schraut (peter_)
        * Fixed this issue in TCodeCompletion.Search:
          https://sourceforge.net/tracker/index.php?func=detail&aid=935068&group_id=10639&atid=110639
    
}
unit CodeCompletion;

interface

uses 
{$IFDEF WIN32}
  Windows, Classes, Forms, SysUtils, Controls, Graphics, StrUtils, CppParser,
  ExtCtrls, stringutils, U_IntList;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Classes, QForms, SysUtils, QControls, QGraphics, StrUtils, CppParser,
  QExtCtrls, U_IntList, QDialogs, Types;
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
    fCurrClassID: integer;
    fIncludedFiles: TStringList;
    function GetTypeID(_Value: AnsiString; il: TIntList): integer;
    function ApplyClassFilter(Index, ParentID: integer; InheritanceIDs: TIntList): boolean;
    function ApplyMemberFilter(const _Class: AnsiString; Index, CurrentID: integer; ClassIDs, InheritanceIDs: TIntList): boolean;
    procedure GetCompletionFor(const _Class, _Value: AnsiString; HasDot: boolean);
    procedure FilterList(const _Class, _Value: AnsiString; HasDot: boolean);
    function GetMember(const Phrase: AnsiString): AnsiString;
    function GetHasDot(const Phrase: AnsiString): boolean;
    procedure SetPosition(Value: TPoint);
    procedure OnFormResize(Sender: TObject);
    function IsIncluded(const FileName: AnsiString): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Search(Sender: TWinControl;const Phrase, Filename: AnsiString);
    procedure Hide;
    function SelectedStatement: PStatement;
    function SelectedIsFunction: boolean;
    function GetClass(const Phrase: AnsiString): AnsiString;
  published
    property ShowCount : integer read fShowCount write fShowCount;
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
    property CurrentClass: integer read fCurrClassID write fCurrClassID;
  end;

implementation

uses
  CodeCompletionForm, Math;

{ TCodeCompletion }

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
  fColor := clWindow;
  fEnabled := True;
  fOnlyGlobals := False;
  fShowCount := 100; // keep things fast
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(CodeComplForm);
  FreeAndNil(fCompletionStatementList);
  FreeAndNil(fFullCompletionStatementList);
  FreeAndNil(fIncludedFiles);
  inherited Destroy;
end;

function TCodeCompletion.GetClass(const Phrase: AnsiString): AnsiString;
var
  I: integer;
begin
  I := LastDelimiter('.', Phrase) - 1;
  if I = -1 then begin
    I := LastDelimiter('>', Phrase);
    if I > 1 then begin
      if Phrase[I - 1] <> '-' then
        I := -1
      else
        Dec(I, 2);
    end
    else begin
      I := LastDelimiter(':', Phrase);
      if I > 1 then begin
        if Phrase[I - 1] <> ':' then
          I := -1
        else
          Dec(I, 2);
      end
      else
        I := -1;
    end;
  end;
  if I = -1 then begin
    Result := '';
    I := Length(Phrase);
    while (I > 0) and (Phrase[I] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) do begin
      Result := Phrase[I] + Result;
      Dec(I);
    end;
  end
  else begin
    Result := '';
    while (I > 0) and (Phrase[I] in ['A'..'Z', 'a'..'z', '_', '0'..'9', '(', ')', '[', ']']) do begin
      if (Phrase[I] = '.') or
        ((I > 1) and (Phrase[I] = '>') and (Phrase[I - 1] = '-')) or
        ((I > 1) and (Phrase[I] = ':') and (Phrase[I - 1] = ':')) then
        Break;
      Result := Phrase[I] + Result;
      Dec(I);
    end;
    // check if it is function; if yes, cut-off the arguments ;)
    if Pos('(', Result) > 0 then
      Result := Copy(Result, 1, Pos('(', Result) - 1);
    // check if it is an array; if yes, cut-off the dimensions ;)
    if Pos('[', Result) > 0 then
      Result := Copy(Result, 1, Pos('[', Result) - 1);
  end;
end;

function TCodeCompletion.ApplyClassFilter(Index, ParentID: integer; InheritanceIDs: TIntList): boolean;
begin
  Result :=
    (
    (PStatement(fParser.Statements[Index])^._Scope in [ssLocal, ssGlobal]) or // local or global var or
    (
    (PStatement(fParser.Statements[Index])^._Scope = ssClassLocal) and // class var
    (
    (PStatement(fParser.Statements[Index])^._ParentID = ParentID) or // from current class
    (
    (InheritanceIDs.IndexOf(PStatement(fParser.Statements[Index])^._ParentID) <> -1) and
    (PStatement(fParser.Statements[Index])^._ClassScope <> scsPrivate)
    ) // or an inheriting class
    )
    )
    ) and
    (IsIncluded(PStatement(fParser.Statements[Index])^._FileName) or
    IsIncluded(PStatement(fParser.Statements[Index])^._DeclImplFileName));
end;

function TCodeCompletion.ApplyMemberFilter(const _Class: AnsiString; Index, CurrentID: integer; ClassIDs, InheritanceIDs: TIntList): boolean;
var
  cs: set of TStatementClassScope;
begin
  Result := PStatement(fParser.Statements[Index])^._ParentID <> -1; // only members
  if not Result then Exit;

  // all members of current class
  Result := Result and ((ClassIDs.IndexOf(CurrentID) <> -1) and (PStatement(fParser.Statements[Index])^._ParentID = CurrentID));

  // all public and published members of var's class
  Result := Result or
    (
    (ClassIDs.IndexOf(PStatement(fParser.Statements[Index])^._ParentID) <> -1) and
    (not (PStatement(fParser.Statements[Index])^._ClassScope in [scsProtected, scsPrivate])) // or member of an inherited class
    );

  if (CurrentID = -1) or (PStatement(fParser.Statements[Index])^._ParentID = fCurrClassID) then
    cs := [scsPrivate, scsProtected]
  else
    cs := [scsPrivate];

  // all inherited class's non-private members
  Result := Result or
    (
    (InheritanceIDs.IndexOf(PStatement(fParser.Statements[Index])^._ParentID) <> -1) and
    (not (PStatement(fParser.Statements[Index])^._ClassScope in cs)) // or member of an inherited class
    );
end;

procedure TCodeCompletion.GetCompletionFor(const _Class, _Value: AnsiString; HasDot: boolean);
var
  I: integer;
  InheritanceIDs: TIntList;
  ClassIDs: TIntList;
  sl: TStringList;
  ParID: integer;
  iID: integer;
  pST: PStatement;
  CurrentID: integer;
  bOnlyLocal: boolean;
  procedure GetInheritance(ClassIndex: integer);
  var
    I: integer;
    isID: integer;
    iST: integer;
  begin
    if ClassIndex <> -1 then begin
      ParID := PStatement(fParser.Statements[ClassIndex])^._ID;
      isID := ClassIndex;
      repeat
        sl.CommaText := PStatement(fParser.Statements[isID])^._InheritsFromIDs;
        iST := -1;
        for I := 0 to sl.Count - 1 do begin
          iST := -1;
          iID := StrToIntDef(sl[I], -1);
          if iID = -1 then
            Continue;
          InheritanceIDs.Add(iID);
          iST := fParser.IndexOfStatement(iID);
          if iST = -1 then
            Continue;
          pST := PStatement(fParser.Statements[iST]);
          fIncludedFiles.Add(pST^._Filename);
          fIncludedFiles.Add(pST^._DeclImplFilename);
        end;
        isID := iST;
      until isID = -1;
    end;
  end;
begin
	bOnlyLocal := False;
	ParID := -1;
	ClassIDs := TIntList.Create;
	InheritanceIDs := TIntList.Create;
	sl := TStringList.Create;
	try

		if not HasDot then begin // only add globals and members of the current class
			GetInheritance(fCurrClassID);

			// Then add globals...
			for I := 0 to fParser.Statements.Count - 1 do
				if ApplyClassFilter(I, ParID, InheritanceIDs) then
					fFullCompletionStatementList.Add(fParser.Statements[I]);

		end else begin

			// Look for the parent class before the operator. Don't scan the headers?
			for I := 0 to fParser.Statements.Count - 1 do
				if SameStr(PStatement(fParser.Statements[I])^._ScopelessCmd,_Class) then begin
					if PStatement(fParser.Statements[I])^._Kind = skClass then begin

						// added for the case "Class::Member", where "Class" is the actual class
						ClassIDs.Clear;
						ClassIDs.Add(PStatement(fParser.Statements[I])^._ID);
						bOnlyLocal := True;
					end else
						GetTypeID(PStatement(fParser.Statements[I])^._Type, ClassIDs);
				end;

			if not bOnlyLocal then
				for I := 0 to ClassIDs.Count - 1 do
					GetInheritance(fParser.IndexOfStatement(ClassIDs[I]));

			if fCurrClassID <> -1 then
				CurrentID := PStatement(fParser.Statements[fCurrClassID])^._ID
			else
				CurrentID := -1;

			// Then look for members...
			for I := 0 to fParser.Statements.Count - 1 do
				if ApplyMemberFilter(_Class, I, CurrentID, ClassIDs, InheritanceIDs) then
					fFullCompletionStatementList.Add(fParser.Statements[I]);
		end;
	finally
		sl.Free;
		InheritanceIDs.Free;
		ClassIDs.Free;
	end;
end;

function ListSort(Item1, Item2: Pointer): Integer;
begin
	// first take into account that parsed statements need to be higher
	// in the list than loaded ones
	if PStatement(Item1)^._Loaded and (not PStatement(Item2)^._Loaded) then
		Result := 1
	else if (not PStatement(Item1)^._Loaded) and PStatement(Item2)^._Loaded then
		Result := -1
	else // otherwise, sort by name
		Result := CompareText(PStatement(Item1)^._ScopelessCmd, PStatement(Item2)^._ScopelessCmd);
end;

procedure TCodeCompletion.FilterList(const _Class, _Value: AnsiString;HasDot: boolean);
var
	I: integer;
begin
	fCompletionStatementList.Clear;
	if _Class <> '' then begin
		if not HasDot then begin // class only
			for I := 0 to fFullCompletionStatementList.Count - 1 do
				if StartsText(_Class, PStatement(fFullCompletionStatementList[I])^._ScopelessCmd) then
					fCompletionStatementList.Add(fFullCompletionStatementList[I]);
		end else begin // class and method
			for I := 0 to fFullCompletionStatementList.Count - 1 do
				// ignore "this" pointer as a member
				if (I <> fParser.GetThisPointerID) and StartsText(_Value, PStatement(fFullCompletionStatementList[I])^._ScopelessCmd) then
					fCompletionStatementList.Add(fFullCompletionStatementList[I]);
		end;
	end else begin
		for I := 0 to fFullCompletionStatementList.Count - 1 do
			fCompletionStatementList.Add(fFullCompletionStatementList[I]);
	end;
	fCompletionStatementList.Sort(@ListSort);
end;

function TCodeCompletion.GetHasDot(const Phrase: AnsiString): boolean;
var
  I: integer;
begin
  Result := LastDelimiter('.', Phrase) > 0;
  if not Result then begin
    I := LastDelimiter('>', Phrase);
    if I > 1 then
      Result := Phrase[I - 1] = '-';
  end;
  if not Result then begin
    I := LastDelimiter(':', Phrase);
    if I > 1 then
      Result := Phrase[I - 1] = ':';
  end;
end;

function TCodeCompletion.GetMember(const Phrase: AnsiString): AnsiString;
var
  I: integer;
begin
  I := LastDelimiter('.', Phrase);
  if I = 0 then begin
    I := LastDelimiter('>', Phrase);
    if I <> 0 then begin
      if (I > 1) and (Phrase[I - 1] <> '-') then
        I := 0;
    end
    else begin
      I := LastDelimiter(':', Phrase);
      if I <> 0 then
        if (I > 1) and (Phrase[I - 1] <> ':') then
          I := 0;
    end;
  end;
  if I = 0 then
    Result := ''
  else
    Result := Copy(Phrase, I + 1, Length(Phrase) - I + 2);
end;

function TCodeCompletion.GetTypeID(_Value: AnsiString; il: TIntList): integer;
var
	I: integer;
begin
	Result := -1;
	if (_Value <> '') and (_Value[Length(_Value)] = '>') then // template
		Delete(_Value, Pos('<', _Value), MaxInt);
	for I := 0 to fParser.Statements.Count - 1 do
		if (CompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd) = 0) or
		   (CompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '*') = 0) or
		   (CompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '&') = 0) or
		   (CompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '**') = 0) then begin
			if (Result = -1) or ((Result <> -1) and (PStatement(fParser.Statements[I])^._ParentID <> Result)) then begin
				Result := PStatement(fParser.Statements[I])^._ID;
				if Assigned(il) then
					il.Add(Result)
				else
					Break;
			end;
		end;
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

procedure TCodeCompletion.Search(Sender: TWinControl;const Phrase, Filename: AnsiString);
var
	C: AnsiString;
	M: AnsiString;
	D: boolean;
	I : integer;
begin
	if fEnabled then begin

		C := GetClass(Phrase);
		M := GetMember(Phrase);
		D := GetHasDot(Phrase);

		if not D or (C <> '') then begin

			Screen.Cursor := crHourglass;

			// only perform full new search if just invoked
			if not CodeComplForm.Showing then begin
				fIncludedFiles.CommaText := fParser.GetFileIncludes(Filename);
				GetCompletionFor(C, M, D);
			end;

			// filter fFullCompletionStatementList to fCompletionStatementList
			FilterList(C, M, D); // and sort here too
			Screen.Cursor := crDefault;
		end;

		if fCompletionStatementList.Count > 0 then begin
			CodeComplForm.lbCompletion.Items.BeginUpdate;
			CodeComplForm.lbCompletion.Items.Clear;

			// Only slow one hundred statements...
			for I := 0 to min(fShowCount,fCompletionStatementList.Count - 1) do
				CodeComplForm.lbCompletion.Items.AddObject('',fCompletionStatementList[I]);

			CodeComplForm.lbCompletion.Items.EndUpdate;

			CodeComplForm.Show;
			CodeComplForm.lbCompletion.SetFocus;
			if CodeComplForm.lbCompletion.Items.Count > 0 then
				CodeComplForm.lbCompletion.ItemIndex := 0;
		end else
			Hide;
	end;
end;

function TCodeCompletion.SelectedIsFunction: boolean;
var
  st: PStatement;
begin
  if fEnabled then begin
    st := SelectedStatement;
    if st <> nil then
      Result := st^._Kind in [skFunction, skConstructor, skDestructor]
    else
      Result := False;
  end
  else
    Result := False;
end;

function TCodeCompletion.SelectedStatement: PStatement;
begin
  if fEnabled then begin
    if (fCompletionStatementList.Count > CodeComplForm.lbCompletion.ItemIndex) and (CodeComplForm.lbCompletion.ItemIndex <> -1) then
      Result := PStatement(fCompletionStatementList[CodeComplForm.lbCompletion.ItemIndex])
    else begin
      if fCompletionStatementList.Count > 0 then
        Result := PStatement(fCompletionStatementList[0])
      else
        Result := nil;
    end;
  end
  else
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

function TCodeCompletion.IsIncluded(const FileName: AnsiString): boolean;
begin
  Result := fIncludedFiles.IndexOf(Filename) <> -1;
end;

end.

