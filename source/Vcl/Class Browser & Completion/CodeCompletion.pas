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
  ExtCtrls, U_IntList, Dialogs;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Classes, QForms, SysUtils, QControls, QGraphics, StrUtils, CppParser,
  QExtCtrls, U_IntList, QDialogs, Types;
{$ENDIF}

type
  {** Modified by Peter **}
  TCompletionEvent = procedure(Sender: TObject; const AStatement: TStatement; const AIndex: Integer) of object;
  
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
    fHintWindow: THintWindow;
    fHintTimer: TTimer;
    fHintTimeout: cardinal;
    fOnKeyPress: TKeyPressEvent;
    fOnResize: TNotifyEvent;
    fOnlyGlobals: boolean;
    fCurrClassID: integer;
    fIncludedFiles: TStringList;
    function GetOnCompletion: TCompletionEvent; {** Modified by Peter **}
    procedure SetOnCompletion(Value: TCompletionEvent); {** Modified by Peter **}
//    procedure GetTypeOfVar(_Value: string; var List, InhList: TIntList);
    function GetTypeID(_Value: string; il: TIntList): integer;
    function ApplyStandardFilter(Index: integer): boolean;
    function ApplyClassFilter(Index, ParentID: integer; InheritanceIDs: TIntList): boolean;
    function ApplyMemberFilter(_Class: string; Index, CurrentID: integer; ClassIDs, InheritanceIDs: TIntList): boolean;
    procedure GetCompletionFor(_Class, _Value: string; HasDot: boolean = False);
//    procedure GetCompletionFor1(_Class, _Value: string; HasDot: boolean = False);
    procedure FilterList(_Class, _Value: string; HasDot: boolean = False);
    function GetClass(Phrase: string): string;
    function GetMember(Phrase: string): string;
    function GetHasDot(Phrase: string): boolean;
    procedure SetParser(Value: TCppParser);
    procedure SetPosition(Value: TPoint);
    procedure SetHintTimeout(Value: cardinal);
    procedure HintTimer(Sender: TObject);
    procedure ComplKeyPress(Sender: TObject; var Key: Char);
    procedure OnFormResize(Sender: TObject);
    procedure SetColor(Value: TColor);
    function IsIncluded(FileName: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Search(Sender: TWinControl; Phrase, Filename: string);
    procedure Hide;
    function SelectedStatement: PStatement;
    function SelectedIsFunction: boolean;
    procedure ShowArgsHint(FuncName: string; Rect: TRect);
    procedure ShowMsgHint(Rect: TRect; HintText: string);
  published
    property Parser: TCppParser read fParser write SetParser;
    property Position: TPoint read fPos write SetPosition;
    property Color: TColor read fColor write SetColor;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Enabled: boolean read fEnabled write fEnabled;
    property HintTimeout: cardinal read fHintTimeout write SetHintTimeout;
    property MinWidth: integer read fMinWidth write fMinWidth;
    property MinHeight: integer read fMinHeight write fMinHeight;
    property MaxWidth: integer read fMaxWidth write fMaxWidth;
    property MaxHeight: integer read fMaxHeight write fMaxHeight;
    property OnCompletion: TCompletionEvent read GetOnCompletion write SetOnCompletion; {** Modified by Peter **}
    property OnKeyPress: TKeyPressEvent read fOnKeyPress write fOnKeyPress;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
    property OnlyGlobals: boolean read fOnlyGlobals write fOnlyGlobals;
    property CurrentClass: integer read fCurrClassID write fCurrClassID;
  end;

implementation

uses CodeCompletionForm;

{ TCodeCompletion }

constructor TCodeCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fHintWindow := THintWindow.Create(Self);
  fHintWindow.Color := clInfoBk;
  fHintTimer := TTimer.Create(Self);
  fHintTimeout := 4000;
  fHintTimer.Interval := fHintTimeout;
  fHintTimer.OnTimer := HintTimer;
  fHintTimer.Enabled := False;

  fIncludedFiles := TStringList.Create;
  fIncludedFiles.Sorted := True;
  fIncludedFiles.Duplicates := dupIgnore;
  fCompletionStatementList := TList.Create;
  fFullCompletionStatementList := TList.Create;
  CodeComplForm := TCodeComplForm.Create(Self);
  CodeComplForm.fParser := fParser;
  CodeComplForm.fCompletionStatementList := fCompletionStatementList;
  CodeComplForm.OnResize := OnFormResize;
  fWidth := 320;
  fHeight := 240;
  fMinWidth := 256;
  fMinHeight := 128;
  fMaxWidth := 0;//480;
  fMaxHeight := 0;//320;
  fColor := clWindow;
  fEnabled := True;
  fOnlyGlobals := False;
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(CodeComplForm);
  FreeAndNil(fCompletionStatementList);
  FreeAndNil(fFullCompletionStatementList);
  FreeAndNil(fHintWindow);
  FreeAndNil(fHintTimer);
  FreeAndNil(fIncludedFiles);
  inherited Destroy;
end;

function TCodeCompletion.GetClass(Phrase: string): string;
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
    if AnsiPos('(', Result) > 0 then
      Result := Copy(Result, 1, AnsiPos('(', Result) - 1);
    // check if it is an array; if yes, cut-off the dimensions ;)
    if AnsiPos('[', Result) > 0 then
      Result := Copy(Result, 1, AnsiPos('[', Result) - 1);
  end;
end;

function TCodeCompletion.ApplyStandardFilter(Index: integer): boolean;
begin
  Result := not PStatement(fParser.Statements[Index])^._NoCompletion;
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

function TCodeCompletion.ApplyMemberFilter(_Class: string; Index, CurrentID: integer; ClassIDs, InheritanceIDs: TIntList): boolean;
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

procedure TCodeCompletion.GetCompletionFor(_Class, _Value: string; HasDot: boolean = False);
var
  I, I1: integer;
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
  ClassIDs := TIntList.Create;
  InheritanceIDs := TIntList.Create;
  sl := TStringList.Create;
  try
    ParID := -1;
    if not HasDot then begin
      GetInheritance(fCurrClassID);
      for I := 0 to fParser.Statements.Count - 1 do begin
        if ApplyStandardFilter(I) and
          ApplyClassFilter(I, ParID, InheritanceIDs) then
          fCompletionStatementList.Add(PStatement(fParser.Statements[I]));
      end;
    end
    else begin // looking for class members only
      for I1 := 0 to fParser.Statements.Count - 1 do
        if PStatement(fParser.Statements[I1])^._ScopelessCmd = _Class then begin
          if PStatement(fParser.Statements[I1])^._Kind = skClass then begin
            // added for the case "Class::Member", where "Class" is the actual class
            ClassIDs.Clear;
            ClassIDs.Add(PStatement(fParser.Statements[I1])^._ID);
            bOnlyLocal := True;
          end
          else
            GetTypeID(PStatement(fParser.Statements[I1])^._Type, ClassIDs);
        end;

      if not bOnlyLocal then
        for I1 := 0 to ClassIDs.Count - 1 do
          GetInheritance(fParser.IndexOfStatement(ClassIDs[I1]));

      if fCurrClassID <> -1 then
        CurrentID := PStatement(fParser.Statements[fCurrClassID])^._ID
      else
        CurrentID := -1;
      for I := 0 to fParser.Statements.Count - 1 do begin
        if ApplyStandardFilter(I) and
          ApplyMemberFilter(_Class, I, CurrentID, ClassIDs, InheritanceIDs) then
          fCompletionStatementList.Add(PStatement(fParser.Statements[I]));
      end;
    end;
  finally
    sl.Free;
    InheritanceIDs.Free;
    ClassIDs.Free;
  end;
end;

procedure TCodeCompletion.FilterList(_Class, _Value: string;
  HasDot: boolean);
var
  I: integer;
begin
  CodeComplForm.lbCompletion.Items.BeginUpdate;
  CodeComplForm.lbCompletion.Items.Clear;
  try
    if _Class <> '' then begin //empty
      fCompletionStatementList.Clear;
      for I := 0 to fFullCompletionStatementList.Count - 1 do
        if not HasDot then begin //class only
          if Assigned(fFullCompletionStatementList[I]) and AnsiStartsText(_Class, PStatement(fFullCompletionStatementList[I])^._ScopelessCmd) then begin
            fCompletionStatementList.Add(fFullCompletionStatementList[I]);
            CodeComplForm.lbCompletion.Items.Add('');
          end;
        end
        else begin //class and method
        // ignore "this" pointer as a member
          if Assigned(fFullCompletionStatementList[I]) and (PStatement(fFullCompletionStatementList[I])^._ID <> fParser.GetThisPointerID) then
            if AnsiStartsText(_Value, PStatement(fFullCompletionStatementList[I])^._ScopelessCmd) then begin
              fCompletionStatementList.Add(fFullCompletionStatementList[I]);
              CodeComplForm.lbCompletion.Items.Add('');
            end;
        end;
    end
    else begin
      for I := 0 to fFullCompletionStatementList.Count - 1 do
        CodeComplForm.lbCompletion.Items.Add('');
      fCompletionStatementList.Clear;
      fCompletionStatementList.Assign(fFullCompletionStatementList);
    end;
  except
  end;
  CodeComplForm.lbCompletion.Items.EndUpdate;
end;

function TCodeCompletion.GetHasDot(Phrase: string): boolean;
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

function TCodeCompletion.GetMember(Phrase: string): string;
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

function TCodeCompletion.GetTypeID(_Value: string; il: TIntList): integer;
var
  I: integer;
begin
  Result := -1;
  if (_Value <> '') and (_Value[Length(_Value)] = '>') then // template
    Delete(_Value, Pos('<', _Value), MaxInt);
  for I := 0 to fParser.Statements.Count - 1 do
    if (AnsiCompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd) = 0) or
      (AnsiCompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '*') = 0) or
      (AnsiCompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '&') = 0) or
      (AnsiCompareText(_Value, PStatement(fParser.Statements[I])^._ScopelessCmd + '**') = 0) then begin
      if (Result = -1) or ((Result <> -1) and (PStatement(fParser.Statements[I])^._ParentID <> Result) {and (PStatement(fParser.Statements[I])^._ParentID <> -1)}) then begin
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
  CodeComplForm.Hide;
end;

procedure TCodeCompletion.ComplKeyPress(Sender: TObject; var Key: Char);
begin
  if fEnabled then begin
    case Key of
{$IFDEF WIN32}
      Char(vk_Escape), '.', '>': CodeComplForm.Hide;
{$ENDIF}
{$IFDEF LINUX}
      Char(xk_Escape), '.', '>': CodeComplForm.Hide;
{$ENDIF}
    end;

    if Assigned(fOnKeyPress) then
      fOnKeyPress(Sender, Key);
  end;
end;

function ListSort(Item1, Item2: Pointer): Integer;
begin
  // first take into account that parsed statements need to be higher
  // in the list than loaded ones
  Result := 0;
  if PStatement(Item1)^._Loaded and not PStatement(Item2)^._Loaded then
    Result := 1
  else if not PStatement(Item1)^._Loaded and PStatement(Item2)^._Loaded then
    Result := -1;

  // after that, consider string comparison
  if Result = 0 then
    Result := AnsiCompareText(PStatement(Item1)^._ScopelessCmd, PStatement(Item2)^._ScopelessCmd);
end;

procedure TCodeCompletion.Search(Sender: TWinControl; Phrase, Filename: string);
var
  P: TPoint;
  C: string;
  M: string;
  D: boolean;
begin
  if fEnabled then begin
    CodeComplForm.OnKeyPress := ComplKeyPress;
    CodeComplForm.SetColor(fColor);
    
    if (Sender <> nil) and (Sender is TWinControl) then
     begin
      P.X := TWinControl(Sender).Left;
      P.Y := TWinControl(Sender).Top + 16;
      if (Sender.Parent <> nil) and (Sender.Parent is TWinControl) then
        P := TWinControl(Sender.Parent).ClientToScreen(P)
      else
        P := TWinControl(Sender).ClientToScreen(P);
      fPos := P;
      SetPosition(fPos);
    end;

    CodeComplForm.Constraints.MinWidth := fMinWidth;
    CodeComplForm.Constraints.MinHeight := fMinHeight;

    // 23 may 2004 - peter schraut (peter_)
    // we set MaxWidth and MaxHeight to 0, to solve this bug:
    // https://sourceforge.net/tracker/index.php?func=detail&aid=935068&group_id=10639&atid=110639
    CodeComplForm.Constraints.MaxWidth := 0;
    CodeComplForm.Constraints.MaxHeight := 0;
    
    CodeComplForm.lbCompletion.Visible := False;

    C := GetClass(Phrase);
    M := GetMember(Phrase);
    D := GetHasDot(Phrase); // and (M<>'');
    if not D or (D and (C <> '')) then try
      Screen.Cursor := crHourglass;
      // only perform new search if just invoked
      if not CodeComplForm.Showing then begin
        fCompletionStatementList.Clear;
        fFullCompletionStatementList.Clear;
        fIncludedFiles.CommaText := fParser.GetFileIncludes(Filename);
        GetCompletionFor(C, M, D);
        fFullCompletionStatementList.Assign(fCompletionStatementList);
      end;
      // perform filtering in list
      FilterList(C, M, D);
    finally
      Screen.Cursor := crDefault;
    end;

    CodeComplForm.lbCompletion.Visible := True;
    if fCompletionStatementList.Count > 0 then begin
      fCompletionStatementList.Sort(@ListSort);
      SetWindowPos(CodeComplForm.Handle, 0, CodeComplForm.Left, CodeComplForm.Top, fWidth, fHeight, SWP_NOZORDER);
      CodeComplForm.lbCompletion.Repaint;
      CodeComplForm.Show;
      CodeComplForm.lbCompletion.SetFocus;
      if CodeComplForm.lbCompletion.Items.Count > 0 then
        CodeComplForm.lbCompletion.ItemIndex := 0;
    end
    else
      CodeComplForm.Hide;
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

procedure TCodeCompletion.SetParser(Value: TCppParser);
begin
  if fParser <> Value then begin
    fParser := Value;
    CodeComplForm.fParser := Value;
  end;
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

procedure TCodeCompletion.ShowArgsHint(FuncName: string; Rect: TRect);
var
  HintText: string;
  I: integer;
  S: string;
begin
  HintText := '';
  fCompletionStatementList.Clear;

  for I := 0 to fParser.Statements.Count - 1 do
    if AnsiCompareStr(PStatement(fParser.Statements[I])^._ScopelessCmd, FuncName) = 0 then begin
      S := Trim(PStatement(fParser.Statements[I])^._Args);
      if S <> '' then begin
        if HintText <> '' then
          HintText := HintText + #10;
        HintText := HintText + S;
      end;
    end;
  if HintText = '' then
    HintText := '* No parameters known *';
  ShowMsgHint(Rect, HintText);
end;

procedure TCodeCompletion.ShowMsgHint(Rect: TRect; HintText: string);
var
  P, MaxX, Lines: integer;
  s, s1: string;
begin
  MaxX := 0;
  Lines := 1;
  S := HintText;

  repeat
    P := Pos(#10, S);
    if P > 0 then begin
      S1 := Copy(S, 1, P - 1);
      S := Copy(S, P + 1, MaxInt);
      if fHintWindow.Canvas.TextWidth(S1) > MaxX then
        MaxX := fHintWindow.Canvas.TextWidth(S1) + 8;
      Inc(Lines);
    end
    else begin
      if fHintWindow.Canvas.TextWidth(S) > MaxX then
        MaxX := fHintWindow.Canvas.TextWidth(S) + 8;
    end;
  until P = 0;

  Rect.Right := Rect.Left + MaxX;
  Rect.Bottom := Rect.Top + fHintWindow.Canvas.TextHeight(HintText) * Lines;
  fHintWindow.ActivateHint(Rect, HintText);
  fHintTimer.Enabled := true;
end;

procedure TCodeCompletion.HintTimer(Sender: TObject);
begin
  fHintWindow.ReleaseHandle;
end;

procedure TCodeCompletion.SetHintTimeout(Value: cardinal);
begin
  if Value <> fHintTimer.Interval then
    fHintTimer.Interval := fHintTimeout;
end;

procedure TCodeCompletion.SetColor(Value: TColor);
begin
  fColor := Value;
end;

function TCodeCompletion.IsIncluded(FileName: string): boolean;
begin
  Result := fIncludedFiles.IndexOf(Filename) <> -1;
end;

{** Modified by Peter **}
function TCodeCompletion.GetOnCompletion: TCompletionEvent;
begin
  Result := CodeComplForm.OnCompletion;
end;

{** Modified by Peter **}
procedure TCodeCompletion.SetOnCompletion(Value: TCompletionEvent);
begin
  CodeComplForm.OnCompletion := Value;
end;

end.

