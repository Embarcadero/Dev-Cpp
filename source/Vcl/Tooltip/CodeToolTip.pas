{----------------------------------------------------------------------------------

  The contents of this file are subject to the GNU General Public License
  Version 1.1 or later (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.gnu.org/copyleft/gpl.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Peter Schraut.
  http://www.console-dev.de

  
  Portions created by Peter Schraut are Copyright 
  (C) 2002, 2003, 2004 by Peter Schraut (http://www.console-dev.de) 
  All Rights Reserved.

  
----------------------------------------------------------------------------------}

//
//
//  History:
//
//    xx/xx/2002
//      Initial release
//
//    31/10/2003
//      Complete rewrite of first version
//
//    01/11/2003
//      Beautified code
//
//    02/11/2003
//      Improved active parameter highlighting
//      Added "SkipString" function
//
//    21/03/2004
//      Added compatibility for latest synedit with wordwrap
//
//    22/03/2004
//      Added 'overloaded' feature. when more than 1 prototype
//      with the same name is in the list, the hintwindow displays
//      two buttons (up/down) where the user can walk through
//      all the same named functions. (like in VC++ .NET)
//      
//      added highlighting for hints. the hints use the same
//      highlighter-attributes as the editor does :)
//
//      the codehint is now displayed below the currentline and at the
//      same x position where the token starts. (like in VC++ .NET)
//
//      now looks the file very rubbish and needs some serious cleanup :P
//
//
//    2004-12-05
//      NEW_SYNEDIT is no longer need as we use "new' synedit
//      and never going back to "old". removing all defines and ifdefs
//
//    24/03/2004
//      Made more changes for downwards compatibility with old SynEdit.
//      Check the 'NEW_SYNEDIT' define.
//      Outsourced XPToolTip code to its own unit
//      Sorted functions in alphabetical order
//
//    25/03/2004
//      Some minor fixes regarding overloaded tooltips
//      Turned of DropShadow by default, since it's a bit annoying for code tooltips
//      Fixed 'Select' function. It now sets FCustomSelection to True
//
//    26/03/2004
//      Fixed a bug where no hint appeared when the cursor was directly before a '('
//
//    2011
//      Rewritten the code that tries to find out which function needs to be displayed.
//      Completely restyled the tooltip font. Now only highlights the currently needed argument.
//      The tooltip now also shows when placing the cursor inside completed arglists between ().
//      The tooltip now updates automatically when hopping from function to function.

unit CodeToolTip;

interface
uses
{$IFDEF WIN32}
  SysUtils, Dialogs, Classes, Windows, Messages, Graphics, Controls, Menus, Forms, StdCtrls,
  SynEditKbdHandler, SynEdit, SynEditHighlighter, CppParser, utils;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, QDialogs, Classes, Xlib, QGraphics, QControls, QMenus, QForms, QStdCtrls,
  QSynEditKbdHandler, QSynEdit, QSynEditHighlighter, Types;
{$ENDIF}

type
  TCustomCodeToolTipButton = class(TPersistent)
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure Paint(const TargetCanvas: TCanvas); virtual; abstract;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  public
    constructor Create; virtual;
    function ClientRect: TRect;
  end;


  TCodeToolTipUpButton = class(TCustomCodeToolTipButton)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint(const TargetCanvas: TCanvas); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCodeToolTipDownButton = class(TCustomCodeToolTipButton)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint(const TargetCanvas: TCanvas); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { Options to customize the CodeToolTip behavior }
  TToolTipOptions = set of
  (
    ttoHideOnEsc,                 // hides the tooltip when ESC has been pressed
    ttoCurrentArgumentExtra,      // force the current argument to attract extra attention
    shoFindBestMatchingToolTip    // automatically find the best matching tooltip (for overloaded functions)
  );

  TCodeToolTip = class(THintWindow)
  private
    FActivated: Boolean;
    FList: TList;
    FParser: TCppParser;
    FTokenPos: Integer;
    FUpButton: TCustomCodeToolTipButton;
    FDownButton: TCustomCodeToolTipButton;
    FSelIndex: Integer;
    FBmp: TBitmap;
    FOptions: TToolTipOptions;
    FEditor: TCustomSynEdit;
    FKeyDownProc: TKeyEvent;
    FToolTips: TStringList;
    FCurParamIndex: Integer;
    FLookupEditor: TCustomSynEdit;
    FDelimiters: string;
    FMaxScanLength: Integer;
    FOldFunction : string;
    FCustomSelIndex: Boolean; // user clicked up/down
    procedure SetSelIndex(Value: Integer);
    procedure SetToolTips(const Strings: TStringList);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function GetCommaIndex(P: PChar; BraceStart, CurPos: Integer):Integer;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FindClosestToolTip(ToolTip: string; CommaIndex: Integer): string;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintToolTip;
    function RemoveEditor(AEditor: TCustomSynEdit): boolean;
    procedure RethinkCoordAndActivate;
    procedure SetEditor(const Value: TCustomSynEdit);
    property Hints: TStringList read FToolTips write SetToolTips;
    property MaxScanLength: Integer read FMaxScanLength write FMaxScanLength;
    property Options: TToolTipOptions read FOptions write FOptions;
    property SelIndex: Integer read FSelIndex write SetSelIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Select(ToolTip: string): Integer;
    procedure Show;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ReleaseHandle;
    property Parser: TCppParser read FParser write FParser;
    property Activated: Boolean read FActivated write FActivated;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
  end;

implementation

// contains the up/down buttons
// I tried to draw them using DrawFrameControl first,
// but it looked very bad, so I use a bitmap instead.
{$R CodeToolTip.res}

resourcestring
  SCodeToolTipIndexXOfX = '%2d/%2d';

var
 Identifiers : array[#0..#255] of ByteBool;

procedure MakeIdentTable;
var
	c: char;
begin
	FillChar(Identifiers, SizeOf(Identifiers), 0);

	for c := 'a' to 'z' do
		Identifiers[c] := True;

	for c := 'A' to 'Z' do
		Identifiers[c] := True;

	for c := '0' to '9' do
		Identifiers[c] := True;

	Identifiers['_'] := True;
	Identifiers['~'] := True;
end;

function PreviousWordString(S: string; Ending: Integer):string;
var
	Walker: Integer;
begin
	Result := '';

	if (Ending < 2) or (S = '') then
		Exit;

	Walker := Ending;

	repeat
		if Walker < 2 then
			Break;
		Dec(Walker);
	until not Identifiers[S[Walker]];

	if not Identifiers[S[Walker]] then
		Inc(Walker);

	Result := Copy(S, Walker, Ending-Walker);
end;

// Returns name of function, so passing foo(bar,bar,bar) will return foo
function GetPrototypeName(const S: string): string;
var
	iStart, iLen: Integer;
begin
	iStart := AnsiPos('(', S);
	iLen := 0;

	// If we found the starting ( of a function
	if iStart > 0 then begin

		repeat
			Dec(iStart);
		until not (S[iStart] in [#32,#9]);
		repeat
			Dec(iStart);
			Inc(iLen);
		until (iStart = 1) or (S[iStart] in [#0..#32]); // This fixes an unsigned 0 - 1 range error

		Result := Copy(S, iStart+1, iLen);
	end else
		Result := '';
end;

// Count commas in the parameter list
function CountCommas(const S: string): Integer;
var
	J: Integer;
begin
	Result := 0;
	for J := 1 to Length(S) do
		if S[J] = ',' then Inc(Result);
end;

//----------------- TCustomCodeToolTipButton ---------------------------------------------------------------------------

constructor TCustomCodeToolTipButton.Create;
begin
	FLeft := 0;
	FTop := 0;
	FWidth := 9;
	FHeight := 11;
end;

function TCustomCodeToolTipButton.ClientRect: TRect;
begin
	Result := Rect(FLeft, FTop, FLeft+FWidth, FTop+FHeight);
end;

//----------------- TCodeToolTipUpButton -------------------------------------------------------------------------------

constructor TCodeToolTipUpButton.Create;
begin
	inherited;

	FBitmap := TBitmap.Create;
	FBitmap.LoadFromResourceName(HInstance, 'UPBUTTON');
end;

destructor TCodeToolTipUpButton.Destroy;
begin
	FBitmap.Free;
	inherited;
end;

procedure TCodeToolTipUpButton.Paint(const TargetCanvas: TCanvas);
begin
	StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TCodeToolTipDownButton -----------------------------------------------------------------------------

constructor TCodeToolTipDownButton.Create;
begin
	inherited;

	FBitmap := TBitmap.Create;
	FBitmap.LoadFromResourceName(HInstance, 'DOWNBUTTON');
end;

destructor TCodeToolTipDownButton.Destroy;
begin
	FBitmap.Free;
	inherited;
end;

procedure TCodeToolTipDownButton.Paint(const TargetCanvas: TCanvas);
begin
	StretchBlt(TargetCanvas.Handle, FLeft, FTop, FWidth, FHeight, FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, SRCCOPY);
end;

//----------------- TCodeToolTip ---------------------------------------------------------------------------------------

constructor TCodeToolTip.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

	FLookupEditor := TSynEdit.Create(Self);

	FOptions := [ttoHideOnEsc,ttoCurrentArgumentExtra,shoFindBestMatchingToolTip];

	FBmp := TBitmap.Create;
	with FBmp do begin
		PixelFormat := pf24Bit;
		Width := Screen.Width; // worst case hintwidth
		Height := 32;
		with Canvas do begin
			Font.Name := 'Courier New';
			Font.Size := 10;
			Brush.Style := bsClear;
		end;
	end;

	FList := TList.Create;
	FToolTips := TStringList.Create;
	with FToolTips do begin
		//Sorted := True;
		CaseSensitive := True;
		//Duplicates := dupIgnore;
	end;

	FDelimiters := ','; // This character is used to separate parameters...
	FMaxScanLength := 192; // Number of character to walk through trying to find ( and )
	FKeyDownProc  := EditorKeyDown;

	FUpButton := TCodeToolTipUpButton.Create;
	FUpButton.Left := 4;
	FUpButton.Top := 3;

	FDownButton := TCodeToolTipDownButton.Create;
	FDownButton.Left := 20; // Edited at runtime
	FDownButton.Top := 3;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.ActivateHint(Rect: TRect; const AHint: string);
begin
	inherited;
	FActivated := True;
end;

procedure TCodeToolTip.ReleaseHandle;
begin
	FActivated := False;
	DestroyHandle;
end;

destructor TCodeToolTip.Destroy;
begin
	if Activated then ReleaseHandle;

	FKeyDownProc := nil;

	FEditor := nil;

	FreeAndNil(FUpButton);
	FreeAndNil(FDownButton);

	FList.Free;
	FToolTips.Free;
	FLookupEditor.Free;

	inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

// Handles input of the tooltip...
procedure TCodeToolTip.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Activated then begin
		case Key of
{$IFDEF WIN32}
			VK_ESCAPE: begin
{$ENDIF}
{$IFDEF LINUX}
			XK_ESCAPE: begin
{$ENDIF}
				if (ttoHideOnESC in FOptions) then ReleaseHandle;
			end;
		end;
	end;
end;

// Gaat de lijst met matchende tooltips langs, kijkt welke met beste past bij code
function TCodeToolTip.FindClosestToolTip(ToolTip: string; CommaIndex: Integer): string;
var
	I,K: Integer;
	NewIndex: Integer;
	Str: string;
	LastCommaCnt: Integer;
begin
	// If we reached a comma index that does not exist, quit
	if ToolTip = FToolTips.Strings[FSelIndex] then
		if CountCommas(FToolTips.Strings[FSelIndex]) <= CommaIndex then Exit;

	LastCommaCnt := 9999;
	NewIndex := 0;

	// loop through the list and find the closest matching tooltip
	// we compare the comma counts
	for I := 0 to FToolTips.Count-1 do begin
		Str := GetPrototypeName(FToolTips.Strings[I]);
		if Str = ToolTip then begin
			K := CountCommas(FToolTips.Strings[I]);
			if K >= CommaIndex then begin
				if K < LastCommaCnt then begin
					NewIndex := I;
					LastCommaCnt := K;
				end;
			end;
		end;
	end;

	Result := FToolTips.Strings[NewIndex];
	SelIndex := NewIndex;
end;

// Return the comma count the cursor is placed *after*, so foo(aaa,bbb,ccc) with the cursor somewhere in b will return 1
function TCodeToolTip.GetCommaIndex(P: PChar; BraceStart, CurPos: Integer):Integer;
var
  I: Integer;

  // skip to EOL
  procedure SkipLine;
  begin
    repeat
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0,#10,#13];
  end;

  // skip c/c++ commentblocks
  procedure SkipCommentBlock;
  begin
    repeat
      case P[i] of
        '*':
          if P[i+1] = '/' then
          begin
            Break;
          end;
      end;
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0];
  end;

  // skip strings! since it not unusual to
  // have commas in string we MUST ignore them!
  procedure SkipStrings;
  begin
    Inc(i);

    repeat
      case P[i] of
        // make sure not to skip inline string "'s
        // for example, a c string: "Hello \"Bond, James\"..."
        // This is ONE string only and we dont want to count commas in it
        '\':
          if P[i+1] = '"' then
            Inc(i);

        '"': Break;
      end;
      Inc(i);
      if i > CurPos then
        Break;
    until P[i] in [#0];
  end;

begin

	Result := 0;
	I := BraceStart;

	while I <= CurPos do begin
		if P[i] = '"' then begin
			SkipStrings;
		end else if P[i] = '/' then begin
			if P[i+1] = '/' then
				SkipLine
			else if P[i+1] = '*' then
				SkipCommentBlock;
		end else if P[i] = ',' then
			Inc(Result);

		Inc(i);
	end;
end;

procedure TCodeToolTip.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	Pt: TPoint;
	NeedRefresh: Boolean;
begin
	inherited;

	if Button = mbLeft then begin
		NeedRefresh := False;
		try
			Pt := ScreenToClient(Mouse.CursorPos);

			// check if we clicked in the UpButton
			if PtInRect(FUpButton.ClientRect, Pt) then begin
				if FSelIndex < FToolTips.Count-1 then
					Inc(FSelIndex, 1)
				else
					FSelIndex := 0;

				NeedRefresh := True;
				FCustomSelIndex := True;
			end;

			// check if we clicked in the DownButton
			if PtInRect(FDownButton.ClientRect, Pt) then begin
				if FSelIndex > 0 then
					Dec(FSelIndex, 1)
				else
					FSelIndex := FToolTips.Count-1;

				NeedRefresh := True;
				FCustomSelIndex := True;
			end;

		finally
			FEditor.SetFocus;
			if NeedRefresh then
				Show;
		end;
	end;
end;

// This function paints the tooltip to FBmp and copies it to the tooltip client surface
procedure TCodeToolTip.Paint;
begin
	PaintToolTip;
	Canvas.CopyRect(ClientRect, FBmp.Canvas, ClientRect);
end;

procedure TCodeToolTip.PaintToolTip;
var
	BracePos: Integer;
	WidthParam: Integer;
	I: Integer;
	CurParam: Integer;
	StrToken,S: string;
{$IFDEF WIN32}
	CurChar: Char;
{$ENDIF}
{$IFDEF LINUX}
	CurChar: WideChar;
{$ENDIF}

	procedure DrawParamLetterEx(Index: Integer; CurrentParam: Boolean);
	var
		HLAttr: TSynHighlighterAttributes;
	begin
		// we use a lookup editor to get the syntax coloring for our tooltips :)
		FLookupEditor.CaretX := Index;
		FLookupEditor.GetHighlighterAttriAtRowCol(FLookupEditor.CaretXY, StrToken, HLAttr);

		with FBmp.Canvas do begin
			Font.Color := clCaptionText;

			// The current argument should be drawing extra attention
			if CurrentParam then begin
				Font.Style := [fsBold];
				if (ttoCurrentArgumentExtra in FOptions) then
					Font.Color := clRed;
			end else if (AnsiPos('(',FLookupEditor.Text) > Index) and Assigned(HLAttr) then
				Font.Style := HLAttr.Style
			else
				Font.Style := []; // Don't highlight

			// draw the char and then increase the current width by the
			// width of the just drawn char ...
			TextOut(WidthParam, 1, FLookupEditor.Text[Index]);
			Inc(WidthParam, TextWidth(FLookupEditor.Text[Index]));
		end;
	end;
begin

	BracePos := AnsiPos('(', Caption);
	CurParam := 0;
	WidthParam := 4; // left start position in pixels

	// Clear the backbuffer and set options
	with FBmp.Canvas do begin
		Brush.Color := TColor($E1FFFF);
		FillRect(ClientRect);
		Font.Name := 'Courier New';
		Font.Size := 10;
		Brush.Style := bsClear;
	end;

	// when more than one tooltip is in the list
	// we must draw the buttons as well ...
	if FToolTips.Count > 1 then begin

		// paint the UP button
		FUpButton.Paint(FBmp.Canvas);
		Inc(WidthParam, FUpButton.Left + FUpButton.Width);

		// output text between the buttons
		FBmp.Canvas.Font.Style := [];
		S := Format(SCodeToolTipIndexXOfX, [FSelIndex+1, FToolTips.Count]);
		FBmp.Canvas.TextOut(WidthParam, 1, S);
		Inc(WidthParam, FBmp.Canvas.TextWidth(S)+3);

		// paint the DOWN button
		FDownButton.Left := WidthParam;
		FDownButton.Paint(FBmp.Canvas);
		Inc(WidthParam, 3 + FDownButton.Width+FUpButton.Left);
	end;

	// now loop through the hint and draw each letter
	for I := 1 to Length(Caption)-1 do begin
		CurChar := Caption[I];

		// if the current char is one of our delimiters
		// we must increase the CurParam variable which indicates
		// at which comma index our cursor is.
		if AnsiPos(CurChar, FDelimiters) > 0 then
			Inc(CurParam);

		if (CurParam = FCurParamIndex) and (AnsiPos(CurChar, FDelimiters)=0) and (I > BracePos) and (CurChar <> ')') and (CurChar <> ' ') and (AnsiPos(')',Caption) > I) then
			DrawParamLetterEx(I, True) // at current comma index
		else
			DrawParamLetterEx(I, False) // Normal
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCodeToolTip.RemoveEditor(aEditor: TCustomSynEdit): boolean;
begin
  Result := Assigned (aEditor);

  if Result then
  begin
    aEditor.RemoveKeyDownHandler(fKeyDownProc);
    if aEditor = fEditor then
      fEditor := nil;
  {$IFDEF SYN_COMPILER_5_UP}
    RemoveFreeNotification(aEditor);
  {$ENDIF}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.RethinkCoordAndActivate;
var
	Pt: TPoint;
	Width : integer;
begin

	// Simulate size to prevent drawing twice...
	Width := FBmp.Canvas.TextWidth(Caption);
	if FToolTips.Count > 1 then
		Width := Width + 72; // Button stuff

	// this displays the rect below the current line and at the same position where the token begins
	Pt := FEditor.ClientToScreen(FEditor.RowColumnToPixels(FEditor.BufferToDisplayPos(FEditor.CharIndexToRowCol(FTokenPos,true))));

	ActivateHint(Rect(Pt.X,
                    Pt.Y+FEditor.LineHeight+2,
                    Pt.X+Width,
                    Pt.Y+Canvas.TextHeight('Wg')+FEditor.LineHeight+4),
                    Caption);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCodeToolTip.Select(ToolTip: string): Integer;

//  selects the tooltip specified by ToolTip and returns
//  the index from it, in the list.
//  the tooltip must be already added to the tooltiplist,
//  otherwise it cannot find it and returns -1
//
//  on success it returns the index of the tooltip in the list
//  otherwise it returns -1

var
  I: Integer;
begin
  Result := -1;

  if FToolTips.Count <> -1 then
  begin
    for I := 0 to FToolTips.Count-1 do
    begin
      if FToolTips.Strings[I] = ToolTip then
      begin
        SelIndex := I;  // set the current index
        Result := I;  // return the index
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.SetEditor(const Value: TCustomSynEdit);
begin
  if (FEditor <> nil) then
    RemoveEditor (fEditor);

  FEditor := Value;

  if (FEditor <> nil) then
  begin
    FEditor.AddKeyDownHandler(FKeyDownProc);
    FEditor.FreeNotification(FEditor);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.SetSelIndex(Value: Integer);
// sets the selection index and repaints the hint when it is activated
begin
	if Value <> FSelIndex then begin
		FSelIndex := Value;
		if Activated then begin
			FCustomSelIndex := True;
			Show;
		end;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.SetToolTips(const Strings: TStringList);
begin
	FToolTips.Clear;
	FToolTips.Assign(Strings);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.Show;
var
	CurPos : Integer;
	P : PChar;
	I : Integer;
	nBraces : Integer;
	S : string;
	Idx : Integer;
	nCommas : Integer;
	ProtoFound : Boolean;
	ProtoName : PChar;

	// skip c/c++ commentblocks
	procedure SkipCommentBlock;
	begin
		repeat
			case P[CurPos] of '*':
				if P[CurPos-1] = '/' then begin
					Dec(CurPos);
					Break;
				end else
					Dec(CurPos);
			else
				Dec(CurPos);
			end;

			Inc(I);
			if I > FMaxScanLength then
				Break;
		until P[CurPos] in [#0];
	end;
begin

	// get the current position in the uncollapsed text
	Idx := FEditor.RowColToCharIndex(FEditor.CaretXY,true);
	CurPos := Idx;

	// get a pointer to the uncollapsed text
	P := PChar(FEditor.Lines.Text);

	nBraces := 0;
	nCommas := 0;

	// Find out where the function ends...
	for I := 1 to FMaxScanLength do begin

		// Opening brace, increase count
		if P[CurPos] = '(' then begin
			Inc(nBraces);

		// Ending brace, decrease count or success!
		end else if P[CurPos] = ')' then begin
			Dec(nBraces);
			if nBraces = -1 then
				break

		// Stopping characters...
		end else if (P[CurPos] = #0) or (P[CurPos] = ';') then begin
			ReleaseHandle;
			Exit;

		// Single line comments
		end else if (P[CurPos] = '/') and (P[CurPos+1] = '/') then begin
			repeat
				Inc(CurPos);
			until (CurPos > FMaxScanLength) or (P[CurPos] in [#10,#0]);

		// Multiline comments
		end else if (P[CurPos] = '/') and (P[CurPos+1] = '*') then begin
			repeat
				Inc(CurPos);
			until (CurPos > FMaxScanLength) or (P[CurPos] = #0) or ((P[CurPos] = '*') and (P[CurPos+1] = '/'));
		end;
		Inc(CurPos);
	end;

	// If we couldn't find the closing brace or reached the start
	if (nBraces <> -1) then begin // -1 means found it
		ReleaseHandle;
		Exit;
	end;

	// We've stopped at the ending ), start walking backwards *here*)
	for I := 1 to FMaxScanLength do begin
		Dec(CurPos);
		if CurPos < 1 then break;
		case P[CurPos] of
			'/':
				if P[CurPos-1] = '*' then
					SkipCommentBlock;

			')': begin
				Inc(nBraces);
			end;

			'(': begin
				Dec(nBraces);
				if nBraces = -2 then begin // Found it!
					Inc(CurPos);
					Break;
				end;
			end;

			',': begin
				if nBraces = -1 then
					Inc(nCommas);
			end;
		end;
	end;

	// Get the name of the function we're about to show
	S := PreviousWordString(P, CurPos);

	// Don't bother scanning the database when there's no word to scan for
	if S = '' then begin
		ReleaseHandle;
		Exit;
	end;

	// Only do the cumbersome list filling when showing a new tooltip...
	if (S <> FOldFunction) or not Activated then begin
		FSelIndex := 0;
		FCustomSelIndex := False;

		// Fill a cache of known functions...
		FToolTips.Clear;
		FParser.FillListOf(S, False, FList);
		FToolTips.BeginUpdate;
		try
			for I := 0 to FList.Count-1 do begin
				if PStatement(FList.Items[I])^._Kind = skFunction then
					FToolTips.Add(PStatement(FList.Items[I])^._FullText);
			end;
		finally
			FToolTips.EndUpdate;
		end;
	end;
	FOldFunction := S;

	// get the current token position in the text
	// this is where the prototype name usually starts
	FTokenPos := CurPos - Length(S) - 1;

	// Check if this function is a known one...
	ProtoFound := False;
	for I := 0 to FToolTips.Count-1 do begin

		// Also accept if S does not have any pointer prefixes
		ProtoName := PChar(GetPrototypeName(FToolTips.Strings[I]));
		while (ProtoName^ in ['*','&']) do
			Inc(ProtoName);

		if ProtoName = S then begin
			ProtoFound := True;

			// And set the selection index if we're about to show up
			if not Activated then
				FSelIndex := I;
			Break;
		end;
	end;

	// If we can't find it, hide
	if not ProtoFound then begin
		ReleaseHandle;
		Exit;
	end else begin
		// Otherwise, search for the best possible overload match according to comma count
		if (shoFindBestMatchingToolTip in FOptions) then

			// Only do so when the user didn't select his own
			if not FCustomSelIndex then
				S := FindClosestToolTip(S, nCommas);

		if (FToolTips.Count > 0) and (FSelIndex < FToolTips.Count) then
			S := FToolTips.Strings[FSelIndex];

		// set the hint caption
		Caption := S;

		// we use the LookupEditor to get the highlighter-attributes
		// from. check the DrawAdvanced method!
		FLookupEditor.Text := S;
		FLookupEditor.Highlighter := FEditor.Highlighter;

		// get the index of the current bracket where the cursor is
		FCurParamIndex := GetCommaIndex(P, CurPos, Idx-1);
		RethinkCoordAndActivate;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.WMNCHitTest(var Message: TWMNCHitTest);
var
	Pt: TPoint;
begin
	Message.Result := HTTRANSPARENT;
	Pt := ScreenToClient(Point(Message.XPos,Message.YPos));

	// hitcheck against the position of our both buttons
	if PtInRect(FUpButton.ClientRect, Pt) or PtInRect(FDownButton.ClientRect, Pt) then begin
		Message.Result := HTCLIENT;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCodeToolTip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
// override WMEraseBkgnd to avoid flickering
begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
	MakeIdentTable;

end.
